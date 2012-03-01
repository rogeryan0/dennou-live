class File
  DIR_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-."

  class << self

=begin
    alias :_rename :rename
    private :_rename
    def rename(from, to)
      raise("cannot move file") if File.exist?(to)
      _rename(from,to)
    end
=end

    def temp_name(path, suffix = nil, length = 32, count = 0)
      path += "/" unless path[-1..-1] == "/"
      #  name = [Digest::MD5.digest(salt)].pack("m").gsub!(/+/,"_").gsub!(/\n/,"").gsub!(/=/,"")
      max = DIR_CHARS.length
      r = rand(max - 2)
      name = DIR_CHARS[r..r] # the first character should not be "-" nor ".".
      (length - 1).times{|i|
        r = rand(max)
        name << DIR_CHARS[r..r]
      }
      name = path + name
      name << suffix if suffix
      if File.exist?(name)
        if count < 10
          return temp_name(path, suffix, length, count+1)
        else
          raise "cannot create temporary path name"
        end
      end
      return name
    end

  end
end

