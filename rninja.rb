require_relative "diag"
require_relative "file"
require_relative "linewriter"

require "fileutils"
require "set"

module RNinja
  class FullBuilder
    attr_accessor :defs, :defaults

    def initialize(d, rn_dir)
      @d = d.fork
      @rules = {}
      @infos = {}
      @defs = []
      @vars = {}
      @defaults = []

      set builddir: rn_dir
    end

    def sanitize(val, colon: false, space: false, vars: false)
      return val.map{|v| sanitize(v, colon: colon, space: space, vars: vars) } if val.is_a?(Enumerable)
      val.to_s.gsub(/(?<!\$)([#{":" if colon}#{"\\s" if space}#{"\\$" if vars}])/, "$\\1")
    end

    def massage(val)
      return [*val].map!{|v| v.to_s } if val.is_a?(Enumerable)
      val.to_s
    end

    def massage_kv(kv)
      return [*kv].map!{|k, v| [k.to_s, massage(v)] }.to_h
    end

    def expand(str, extra: nil, skip: Set[])
      return str.map{|i| expand(i, extra: extra, skip: skip) } unless str.is_a?(String)

      str = str.gsub(/\$\n/, "")
      str.gsub!(/\$([\s:\$])/, "\\1")

      str.gsub!(/\$(?:(\w+)\b|\{(\w+)\})/) do |match|
        var = ($1 || $2).to_sym

        @d.error_r("Can't resolve recursive variable reference!") if skip.include?(var)

        catch(:found) do
          throw(:found, extra[var]) if extra && extra.include?(var)

          get(var, skip: skip | [var])
        end
      end

      str
    end

    def get(var, expanded: true, skip: Set[])
      var = var.to_sym
      val = @vars.fetch(var) { @d.error_r("Unknown variable '#{var}'") }
      expanded ? expand(val, skip: skip) : val
    end

    def set(**vars)
      vars.map{|k, v| [k, v] }.each do |key, val|
        @vars[key] = val
        @defs << [:set, key.to_s, massage(val)]
      end
    end

    def var_path(var, path) File.join("$#{var}", path) end
    def var_paths(var, *paths) paths.map{|p| var_path(var, p) } end

    def var_rel_path(var, path) var_path(var, File.rel_path(path, get(var))) end
    def var_rel_paths(var, *paths) paths.map{|p| var_rel_path(var, p) } end

    def var_glob(var, pat) Dir[File.join(get(var), expand(pat))].map{|f| var_rel_path(var, f) } end

    def rule(name, build: nil, from: nil, info: "#{name} $in", **opts)
      (build, from) = [build, from].map!{|v| massage(v) }

      if build || from
        @rules[[build, from].map! do |set|
          case set
            when nil; @d.error_r("Both build: and from: must be specified, or neither")
            when Set; set
            else Set[*set]
          end
        end.freeze] = name
      end

      @infos[name] = info

      @defs << [:rule, name, massage_kv(opts)]
    end

    def build(name, also: [], from: [], with: nil, imply: [], after: [], default: false, **opts)
      (name, also, from, imply, after) = [name, also, from, imply, after].map!{|v| [*massage(v)] }

      unless with
        with = @rules.fetch([name, from].map! do |set|
          set = [*set]
          Set[*set.map{|n| File.extname(n) }]
        end) { @d.error_r("No implicit rule found to build #{name.join(" ")} from #{from.empty? ? "nothing" : from.join(" ")}.") }
      end

      if !opts.include?(:description) && @infos.include?(with)
        opts[:description] = expand(@infos[with], extra: {
          in: from.map{|f| expand(f) }.join(" "),
          out: name.map{|f| expand(f) }.join(" "),
        })
      end


      [name, also, from, imply, after].each{|v| v.map!{|i| sanitize(i, colon: true, space: true) } }

      @defs << [:build, name, also, with, from, imply, after, massage_kv(opts)]
      @defaults << name if default
    end

    def phony(name, is:, **opts) build name, from: is, with: "phony", **opts end
  end

  @@errrored = false
  @@quiet = false
  @@verbose = false

  def self.error!; @@errored = true end
  def self.error?; @@errored end
  def self.quiet?; @@quiet end
  def self.quiet=(value) @@quiet = value end
  def self.verbose?; @@verbose end
  def self.verbose=(value) @@verbose = value end

  def self.diag
    Diagnostics.new(
      on_error: lambda{ self.error! },
      quiet: lambda{ self.quiet? },
      verbose: lambda{ self.verbose? },
    )
  end

  @@d = diag

  private_class_method def self.run_diag
    begin
      [true, yield]
    rescue Diagnostics::DiagnosticError
      [false, nil]
    rescue => e
      $stderr << e.backtrace[0] << ":#{e.to_s} (#{e.class})\n" <<
        e.backtrace[1..-1].map{|e2| " " * 8 + "from " << e2.to_s << "\n"}.join
      error!
    end
  end

  def self.run(rn_dir:, &block)
    @@d.pos("RNinja.run") do
      b = nil

      run_diag do
        FileUtils.mkdir_p(rn_dir)

        b = FullBuilder.new(@@d, rn_dir)
        b.instance_eval(&block)

        ninja_file = File.join(rn_dir, "build.ninja")

        File.open(ninja_file, "w") do |file|
          was_var = false
          file << LineWriter.lines do |l|
            emit_wrap = lambda do |parts|
              parts.each do |part|
                if l.peek.length > 0
                  if (l.peek.length + part.length + 1) > 78
                    l.peek << " $"
                    l << "  #{part}"
                  else
                    l.peek << " " << part
                  end
                else
                  l.trim << part
                end
              end
            end

            emit_opts = lambda do |kvs|
              kvs.each do |key, val|
                l << "#{key} ="
                if val.is_a?(String)
                  l.peek << " " << val
                else
                  emit_wrap.(val)
                end
              end
            end

            b.defs.each do |type, *args|
              is_var = false

              case type
                when :set
                  is_var = true

                  l.trim if was_var
                  (name, val) = args
                  emit_opts.(name => val)
                  l.sep

                when :rule
                  (name, opts) = args

                  l << "rule #{name}"

                  l.fmt with_indent: "  " do
                    emit_opts.(opts)
                  end

                  l.sep unless opts.empty?
                when :build
                  (name, also, with, from, imply, after, opts) = args

                  parts = ["build", *name]
                  parts << "|" unless also.empty?
                  parts.concat(also)
                  parts[-1] = "#{parts[-1]}:"
                  parts << with
                  parts.concat(from)
                  parts << "|" unless imply.empty?
                  parts.concat(imply)
                  parts << "||" unless after.empty?
                  parts.concat(after)

                  parts.map!{|p| p.to_s }

                  l << ""
                  emit_wrap.(parts)

                  l.fmt with_indent: "  " do
                    emit_opts.(opts)
                  end

                  l.sep unless opts.empty?

                else @d.fatal_r("Unknown deftype #{type.inspect}")
              end

              was_var = is_var
            end

            l.sep

            b.defaults.each do |default|
              l << "default"
              emit_wrap.(default)
            end

            l.trim
          end << "\n"
        end

        ninja_cmd = ["ninja", "-f", ninja_file, *ARGV]

        @@d.info(ninja_cmd.join(" "))

        status = Subprocess.call(ninja_cmd)

        @@d.info("Ninja exited with code #{status.exitstatus}")

        exit(status.exitstatus) if status.exitstatus != 0
      end
    end
  end
end