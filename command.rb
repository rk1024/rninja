require_relative "core"

require "strscan"

module Command
  @@d = RNinja::Core.diag

  def self.read_to_end(io, into)
    loop do
      buf = io.read_nonblock(1048576)
      into << buf if into
    end
  rescue EOFError, Errno::EPIPE
    io.close
    true
  rescue Errno::EINTR
  rescue Errno::EWOULDBLOCK, Errno::EAGAIN
    false
  end

  def self.get_io(opt, mode)
    [
      *(case opt
        when :pipe; IO.pipe
        when IO; opt
        when Integer; IO.new(opt, mode)
        when String; File.open(opt, mode)
        when nil; nil
        else raise ArgumentError
      end)
    ].tap do |a|
      mode =~ /^r/ ? a.push(nil) : a.unshift(nil) if a.length == 1
    end
  end

  class Proc
    @@sigchld_lock = Mutex.new

    attr_reader :status

    def initialize(d, cmd, **opts)
      @d = d.fork

      @cmd = cmd

      @stdin_r, @stdin_w = Command.get_io(opts[:stdin], "r")
      @stdout_r, @stdout_w = Command.get_io(opts[:stdout], "w")
      @stderr_r, @stderr_w = Command.get_io(opts[:stderr], "w") unless opts[:stderr] != STDOUT

      fwd_r, fwd_w = IO.pipe

      @d.pos("Proc") do
        @pid = fork do
          begin
            STDIN.reopen(@stdin_r) if @stdin_r
            STDOUT.reopen(@stdout_w) if @stdout_w
            STDERR.reopen(opts[:stderr] == :stdout ? STDOUT : @stderr_w) if @stderr_w

            exec_opts = {close_others: true}

            catch(:break) { exec_opts.merge(opts.fetch(:exec_opts) { throw(:break) }) }
            catch(:break) { exec_opts[:chdir] = opts.fetch(:cd) { throw(:break) } }

            if opts[:no_bin_env]
              av0, avn = *@cmd
              exec([av0] * 2, *avn, exec_opts)
            else
              exec(["/usr/bin/env"] * 2, *cmd, exec_opts)
            end
          rescue Exception => e
            Marshal.dump(e, fwd_w)
            fwd_w.flush
          end

          @d.fatal("call to exec(3) failed (or didn't happen)")
          exit!(1)
        end
      end

      @stdin_r.close if @stdin_r && @stdin_r != opts[:stdin]
      @stdout_w.close if @stdout_w && @stdout_w != opts[:stdout]
      @stderr_w.close if @stderr_w && @stderr_w != opts[:stderr]
      fwd_w.close

      begin
        e = Marshal.load(fwd_r)
        wait
        raise e
      rescue EOFError
      ensure
        fwd_r.close
      end
    end

    def stdin; @stdin_w end
    def stdout; @stdout_r end
    def stderr; @stderr_r end

    def wait(flags = 0)
      begin
        @status ||= [*Process.waitpid2(@pid, flags)][-1]
      rescue Interrupt
        $stderr << "\n"
        @d.info("child process interrupted")
        exit!(1)
      end
    end

    def poll; wait(Process::WNOHANG) end

    def read
      @d.pos("Proc.read") do
        stdin.close if stdin

        cout = ""
        cerr = ""

        out_enc = stdout.external_encoding if stdout
        err_enc = stderr.external_encoding if stderr

        fwd_r, fwd_w = IO.pipe
        open_r = [stdout, stderr, fwd_r].compact

        begin
          old_sigchld = nil
          @@sigchld_lock.synchronize do
            old_sigchld = Signal.trap("SIGCHLD") do
              begin
                fwd_w.write_nonblock("\x00")
              rescue Errno::EWOULDBLOCK, Errno::EAGAIN, Errno::EPIPE
              ensure
                old_sigchld.() unless old_sigchld.is_a?(String)
              end
            end
          end

          loop do
            break if poll && open_r == [fwd_r]

            sel_r, _ = select(open_r, nil)

            sel_r.each do |io|
              case io
                when stdout
                  open_r.delete(stdout) if Command.read_to_end(stdout, cout)
                when stderr
                  rpen_r.delete(stderr) if Command.read_to_end(stderr, cerr)
                when fwd_r;
                  raise @d.fatal_r("Whoops, an internal pipe got closed!") if Command.read_to_end(fwd_r, nil)
                  false
              end
            end
          end

          @@sigchld_lock.synchronize do
            Signal.trap("SIGCHLD", old_sigchld || "DEFAULT")
          end
        end

        wait

        fwd_r.close
        fwd_w.close

        cout.force_encoding(out_enc) if out_enc
        cerr.force_encoding(err_enc) if err_enc

        [cout, cerr]
      end
    end
  end

  private_class_method def self.resplit(cmd)
    return cmd unless cmd.is_a?(String)

    state = :init
    s = StringScanner.new(cmd)
    out = []
    curr = ""

    until s.eos?
      case state
        when :init
          if s.scan(/\s+/)
            out << curr unless curr.empty?
            curr = ""
          elsif s.scan(/\\/) then curr << s.scan(/./)
          elsif s.scan(/'/)
            out << curr unless curr.empty?
            curr = ""
            state = :sq
          elsif s.scan(/"/)
            out << curr unless curr.empty?
            curr = ""
            state = :dq
          else curr << s.scan(/./)
          end
        when :sq
          if s.scan(/\\/) then curr << s.scan(/./)
          elsif s.scan(/'/)
            out << curr
            curr = ""
            state = :init
          else curr << s.scan(/./)
          end
        when :dq
          if s.scan(/\\/) then curr << s.scan(/./)
          elsif s.scan(/"/)
            out << curr
            curr = ""
            state = :init
          else curr << s.scan(/./)
          end
      end
    end

    out << curr unless curr.empty?

    case state
      when :sq; raise @@d.fatal_r("unterminated single quote in #{@d.hl(cmd)}")
      when :dq; raise @@d.fatal_r("unterminated double quote in #{@d.hl(cmd)}")
    end

    out
  end

  def self.run(cmd, **opts)
    @@d.pos("Command.run") do
      cmd = resplit(cmd)

      Proc.new(@@d, cmd, **opts).wait
    end
  end

  def self.read(cmd, **opts)
    @@d.pos("Command.read") do
      cmd = resplit(cmd)

      opts[:stdout] = :pipe

      proc = Proc.new(@@d, cmd, **opts)

      ret, _ = proc.read

      raise @@d.fatal("Child process #{@@d.hl(cmd.join(" "))} #{@@d.hl(proc.status)}") unless proc.wait.success?

      ret
    end
  end
end