require_relative "diag"

module RNinja
  module Core
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

    def self.run_diag
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
  end
end