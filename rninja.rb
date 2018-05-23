require_relative "core"
require_relative "command"
require_relative "file"
require_relative "linewriter"

require "fileutils"
require "psych"
require "set"
require "yaml"

module RNinja
  class NinjaGenerator
    attr_reader :rn_dir

    def initialize(d, rn_dir)
      @d = d.fork
      @rn_dir = rn_dir
    end

    def name; "Ninja" end
    def file; "build.ninja" end

    def sanitize_path(val) val.gsub(/(?<!\$)([:\s])/, "$\\1") end

    def begin(l, defaults)
      @l = l
      @defaults = defaults

      emit_set(:builddir, rn_dir) # Special Ninja variable for file output
    end

    def pream_end; end

    def end
      @defaults.each do |default|
        @l << "default"
        emit_wrap(default)
      end
    end

    private def emit_wrap(parts)
      parts.each do |part|
        if @l.peek.length > 0
          if (@l.peek.length + part.length + 1) > 78
            @l.peek << " $"
            @l << "  #{part}"
          else
            @l.peek << " " << part
          end
        else
          @l.trim << part
        end
      end
    end

    private def emit_opts(kvs)
      kvs.each do |key, val|
        @l << "#{key} ="
        if val.is_a?(String)
          @l.peek << " " << val
        else
          emit_wrap(val)
        end
      end
    end

    def emit_set(name, val) emit_opts(name => val) end

    def emit_rule(name, opts)
      @l << "rule #{name}"

      @l.fmt with_indent: "  " do
        emit_opts(opts)
      end

      @l.sep unless opts.empty?
    end

    def emit_build(name, also, with, from, imply, after, opts)
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

      @l << ""
      emit_wrap(parts)

      @l.fmt with_indent: "  " do
        emit_opts(opts)
      end

      @l.sep unless opts.empty?
    end

    def cmd(script, args) [*%w[ninja -f], script, *args] end
  end

  class MakeGenerator
    attr_reader :rn_dir

    def initialize(d, rn_dir)
      @d = d.fork
      @has_mkdir = false
      @rn_dir = rn_dir
      @rules = {}
      @outs = Set[]
      @dirs = Set[]
    end

    def name; "Make" end
    def file; "Makefile" end

    def sanitize_path(val) val.gsub(/(?<!\\)([#])/, "\\\\1") end

    def begin(l, defaults)
      @l = l
      @defaults = defaults
    end

    def pream_end
      @l << ".SECONDARY:" << ".DELETE_ON_ERROR:"
      @l.sep

      @l << ".PHONY: $(rn_d)/.defaults.rnj" << "$(rn_d)/.defaults.rnj: #{@defaults.join(" ")}"
      @l.sep
    end

    def end
      q = @dirs.to_a

      until q.empty?
        dir = File.dirname(q.shift)

        q << dir if @dirs.add?(dir)
      end

      @dirs.reject!{|d| File.dirname(d) == d || @outs.include?(d) }

      unless @has_mkdir
        @l.sep
        emit_set(:rn_mkdir, "mkdir -p --")
        emit_set(:rn_touch, "touch")
        @l.sep

        emit_rule(:rn_mkdir, {"command" => "-$rn_mkdir $out || $rn_touch $out"})
      end

      @dirs.each do |itm|
        parent = File.dirname(itm)
        parent = nil if File.dirname(parent) == parent
        emit_build([itm], [], :rn_mkdir, [*parent], [], [], {
          "description" => "mkdir #{itm}"
        }, is_mkdir: true)
      end
    end

    def get(var, vars, expanded: true, skip: Set[])
      var = var.to_sym
      val = vars.fetch(var) do
        return yield if block_given?
        raise @d.error_r("unknown variable #{@d.hl(var)}")
      end
      expanded ? expand(val, vars, skip: skip) : val
    end

    def expand(str, vars, extra: nil, skip: Set[])
      return str.map{|i| expand(i, vars, extra: extra, skip: skip) } unless str.is_a?(String)

      str = str.gsub(/\$\n/, "")
      str.gsub!(/\$([\s:\$])/, "\\1")

      str.gsub!(/\$(?:(\w+)\b|\{(\w+)\})/) do |match|
        var = ($1 || $2).to_sym

        raise @d.error_r("Can't resolve recursive variable reference!") if skip.include?(var)

        catch(:found) do
          throw(:found, extra[var]) if extra && extra.include?(var)

          get(var, vars, skip: skip | [var]) { match }
        end
      end

      str
    end

    private def fix_vars(str)
      return str.map{|i| fix_vars(i) } unless str.is_a?(String)

      str.gsub(/\$(?:(\w+)\b|\{(\w+)\})/) {|m| "$(#{$1 || $2})" }
    end

    private def resplit(str)
      return str.flat_map{|i| resplit(i) } unless str.is_a?(String)

      str.split(/\s+|('.*'|".*")/).reject{|s| !s || s.empty? }
    end

    private def emit_wrap(parts, recipe:)
      parts.each do |part|
        if @l.peek.length > 0
          if (@l.peek.length + part.length + 1) > (recipe ? 78 : 79)
            @l.peek << (recipe ? " \\" : "\\")
            @l << " #{part}"
          else
            @l.peek << " " << part
          end
        else
          @l.trim << (recipe ? "\t#{part}" : part)
        end
      end
    end

    private def emit_opts(kvs, recipe:)
      kvs.each do |key, val|
        @l << "#{key} ="
        if val.is_a?(String)
          @l.peek << " " << val
        else
          emit_wrap(val, recipe: recipe)
        end
      end
    end

    def emit_set(name, val)
      emit_opts({name => fix_vars(val)}, recipe: false)
    end

    def emit_rule(name, opts)
      @has_mkdir = true if name == "rn_mkdir"
      @rules[name] = opts
    end

    def emit_build(name, also, with, from, imply, after, opts, is_mkdir: false)
      is_phony = with == "phony"

      vars = opts.map{|k, v| [k.to_sym, v.to_s] }.to_h

      targets = [*name, *also]
      target = targets[0]
      dependents = targets[1..-1]

      @outs.merge(targets)

      unless is_mkdir || is_phony
        Set.new(targets.map{|p| File.dirname(p) }.select{|d| File.dirname(d) != d }).each do |dir|
          @dirs << dir
          imply << dir
        end
      end

      if is_phony
        @l << ".PHONY:"
        emit_wrap(targets, recipe: false)
      end

      parts = ["#{target}:"]

      parts.concat(from)
      parts.concat(imply)
      parts << "|" unless after.empty?
      parts.concat(after)

      parts = resplit(fix_vars(expand(parts.map!{|p| p.to_s }, vars)))

      parts.each{|n| raise @d.fatal_r("invalid rule part '#{n}'") if n =~ /\s/ }

      @l << ""
      emit_wrap(parts, recipe: false)

      unless is_phony
        rule = @rules[with]

        raise @d.fatal_r("no rule found for #{@d.hl(with)}") unless rule

        parts = [rule["command"]]

        raise @d.fatal_r("bad command for rule #{@d.hl(with)}") unless parts.any?

        parts = resplit(fix_vars(expand(parts.map!{|p| p.to_s }, vars, extra: {
          in: imply.empty? ? "$^" : resplit(fix_vars(expand(from, vars))).join(" "),
          out: also.empty? && name.length == 1 ? "$@" : resplit(fix_vars(expand(name, vars))).join(" "),
        })))

        parts[0] = "@#{parts[0]}" unless parts.empty?

        parts.each{|n| @d.warn("maybe invalid recipe part '#{n}'") if n =~ /\s/ }

        @l << ""
        emit_wrap(parts, recipe: true)

        catch(:stop) { @l << "\t@echo \e[1m[RNinja]\e[m #{fix_vars(vars.fetch(:description) { throw(:stop) })}" }

        @l.sep unless parts.empty?
      end

      unless dependents.empty?
        parts = [*dependents[1..-2], "#{dependents[-1]}:", target]

        parts = resplit(fix_vars(expand(parts.map!{|p| p.to_s }, vars)))

        @l << ""
        emit_wrap(parts, recipe: false)

        @l.sep unless parts.empty?
      end
    end

    def cmd(script, args) [*%w[make -f], script, *args] end
  end

  class FullBuilder
    attr_accessor :defs, :defaults

    def initialize(d, config, generator)
      @d = d.fork
      @generator = generator
      @rules = {}
      @infos = {}
      @defs = []
      @vars = {}
      @defaults = []
      @config = config
    end

    def sanitize_path(val)
      return val.map{|v| sanitize_path(v) } if val.is_a?(Enumerable)
      @generator.sanitize_path(val.to_s)
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

        raise @d.error_r("Can't resolve recursive variable reference!") if skip.include?(var)

        val = catch(:found) do
          throw(:found, extra[var]) if extra && extra.include?(var)

          get(var, skip: skip | [var])
        end

        case val
          when String; val
          when Enumerable; val.join(" ")
          else val.to_s
        end
      end

      str
    end

    def get(var, expanded: true, skip: Set[])
      var = var.to_sym
      val = @vars.fetch(var) { raise @d.error_r("Unknown variable '#{var}'") }
      expanded ? expand(val, skip: skip) : val
    end

    def set(**vars)
      vars.each do |key, val|
        @vars[key] = val
        @defs << [:set, key.to_s, massage(val)]
      end
    end

    def var_path(var, path) File.join("$#{var}", path) end
    def var_paths(var, *paths) paths.map{|p| var_path(var, p) } end

    def var_rel_path(var, path) var_path(var, File.rel_path(path, get(var))) end
    def var_rel_paths(var, *paths) paths.map{|p| var_rel_path(var, p) } end

    def var_glob(var, pat) Dir[File.join(get(var), expand(pat))].map{|f| var_rel_path(var, f) } end

    def config(key) @config[key] end

    def rule(name, build: nil, from: nil, info: "#{name} $in", **opts)
      (build, from) = [build, from].map!{|v| massage(v) }

      if build || from
        @rules[[build, from].map! do |set|
          case set
            when nil; raise @d.error_r("Both build: and from: must be specified, or neither")
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
        end) { raise @d.error_r("No implicit rule found to build #{name.join(" ")} from #{from.empty? ? "nothing" : from.join(" ")}.") }
      end

      if !opts.include?(:description) && @infos.include?(with)
        opts[:description] = expand(@infos[with], extra: {
          in: from.map{|f| expand(f) }.join(" "),
          out: name.map{|f| expand(f) }.join(" "),
        })
      end


      [name, also, from, imply, after].each{|v| v.map!{|i| sanitize_path(i) } }

      @defs << [:build, name, also, with, from, imply, after, massage_kv(opts)]
      @defaults << name if default
    end

    def phony(name, is:, **opts) build name, from: is, with: "phony", **opts end
  end

  class ConfigBuilder
    def initialize(d, path)
      @d = d.fork
      @file = path

      raise @d.fatal_r("bad config filepath #{@d.hl(@file)}") if !@file || File.exist?(@file) && !File.file?(@file)

      @config = File.file?(@file.to_s) ? File.open(@file, "r") {|f| massage_config(YAML.load(f)) } : {}
      @keys = Set[:profiles]
      @defaults = {}
      @profiles = {}
      @default_profiles = []
    end

    private def try_clone(x)
      x.clone
    rescue TypeError
      x
    end

    def emit
      @d.pos("emit") do
        file2 = @file.clone << "~"

        file2 << "~" while File.exist?(file2)

        config_nodefs = @config.reject{|_, v| v == :default }

        begin
          File.open(file2, "w") do |f|
            f << YAML.dump(massage_config(@keys.map{|k| [k, k == :profiles ? @default_profiles : :default] }
              .to_h.merge(config_nodefs), :write))
          end

          FileUtils.mv(file2, @file)
        rescue Exception => e
          File.delete(file2)

          raise e
        end

        defs = {profiles: @default_profiles}

        defs.merge!(@defaults)
        [*@default_profiles, *config_nodefs[:profiles]].reverse.each{|p| defs.merge!(@profiles[p]) }

        @config = defs.merge(config_nodefs)
        @config = @config.map{|k, v| [try_clone(k).freeze, try_clone(v).freeze] }.to_h.freeze

        @d.p(@config)
      end
    end

    def massage_config_val(val, mode = :read)
      case val
        when Hash; massage_config(val, mode)
        when Enumerable; val.map{|i| massage_config_val(i, mode) }.to_a
        else val
      end
    end

    def massage_config(conf, mode = :read)
      conf.map do |key, val|
        [
          case mode
            when :read; key.to_s.to_sym
            when :write; key.to_s
            else raise "invalid massage_config mode #{mode.inspect}"
          end,
          massage_config_val(val, mode)
        ]
      end.to_h
    end

    def [](key)
      @d.pos("self[#{key.inspect}]") do
        @config.fetch(key) { raise @d.fatal_r("key #{@d.hl(key)} doesn't exist") }
      end
    end

    def init(*keys)
      @d.pos("init") do
        @keys.merge(keys)
      end
    end

    def defaults(**opts)
      @d.pos("defaults") do
        opts.rehash
        opts = massage_config(opts)
        @keys.merge(opts.each_key)
        @defaults.merge!(opts)
      end
    end

    def default_profiles(*profiles)
      @d.pos("default_profiles #{@d.hl(profiles)}") do
        @default_profiles = [*profiles]
      end
    end

    def profile(name, config)
      @d.pos("profile #{name.inspect}") do
        config.each_key{|k| raise @d.fatal_r("key #{@d.hl(k)} not defined") unless @keys.include?(k) }

        @profiles[name] = config.clone
      end
    end

    def rule(key)
      @d.pos("rule") do
        raise @d.fatal_r("config key #{key} doesn't exist") unless @config.include?(key)
      end
    end
  end

  @@d = Core.diag
  @@config = nil

  def self.[](key) @@config[key] end

  def self.config(file: nil, &block)
    @@d.pos("RNinja.config") do
      Core.run_diag do
        @@config = ConfigBuilder.new(@@d, file)
        @@config.instance_eval(&block)
        @@config.emit
      end
    end
  end

  def self.run(rn_dir:, conf: nil, gen: :ninja, &block)
    @@d.pos("RNinja.run") do
      b = nil

      Core.run_diag do
        FileUtils.mkdir_p(rn_dir)

        gen = case gen
          when :ninja; NinjaGenerator
          when :make; MakeGenerator
          else nil
        end.new(@@d, rn_dir)

        b = FullBuilder.new(@@d, @@config, gen)
        b.instance_eval(&block)

        build_script = File.join(rn_dir, gen.file)

        File.open(build_script, "w") do |file|
          was_var = false
          is_pream = true
          file << LineWriter.lines do |l|
            gen.begin(l, b.defaults)

            b.defs.each do |type, *args|
              is_var = type == :set

              if was_var && is_var
                l.trim
              end

              if is_pream && !is_var
                is_pream = false

                gen.pream_end
              end

              case type
                when :set; gen.emit_set(*args)
                when :rule; gen.emit_rule(*args)
                when :build; gen.emit_build(*args)
                else raise @d.fatal_r("Unknown deftype #{type.inspect}")
              end

              l.sep if is_var

              was_var = is_var
            end

            l.sep

            gen.end

            l.trim
          end << "\n"
        end

        build_cmd = gen.cmd(build_script, ARGV)

        @@d.info(build_cmd.map{|a| a =~ /\s/ ? "\"#{a}\"" : a }.join(" "))

        status = Command.run(build_cmd)

        @@d.info("#{gen.name} #{@@d.hl(status)}")

        exit(status.exitstatus) if status.exitstatus != 0
      end
    end
  end
end