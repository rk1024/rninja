require "pathname"

class File
  def self.change_ext(path, ext)
    unless block_given?
      case ext
      when /\A\.[^\.]*\z/, '', nil;
      else raise 'Invalid extension format.'
      end
    end

    dot = path.rindex('.')

    if dot.nil?
      dot = path.size
    else
      slash = path.rindex(Regexp.union(*[File::SEPARATOR, File::ALT_SEPARATOR].compact))
      slash = -1 if slash.nil?

      # Ignore file/.path
      if dot < slash + 2
        dot = path.size
      end
    end

    "#{path[0...dot]}#{block_given? ? yield(path[dot..-1]) : ext}"
  end

  def self.rel_path(path, rel)
    Pathname.new(path).relative_path_from(Pathname.new(rel)).to_s
  end
end
