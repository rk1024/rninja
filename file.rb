require "pathname"

class File
  def self.change_ext(path, ext)
    case ext
      when /\A\..*\z/, "", nil;
      else raise "Invalid extension format."
    end

    path.sub(/(?<=\A|#{File::SEPARATOR}|#{File::ALT_SEPARATOR})[^#{File::SEPARATOR}#{File::ALT_SEPARATOR}]+\z/) do |base|
      idx = base.rindex(".")

      (!idx || idx == 0 ? base : base[0...idx]) << ext
    end
  end

  def self.rel_path(path, rel)
    Pathname.new(path).relative_path_from(Pathname.new(rel)).to_s
  end
end