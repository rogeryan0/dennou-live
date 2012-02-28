# = Handling of multi-byte characters (or Japanese).
#
# Assume that $KCONV has been set appropriately

class String

  # see http://www.ruby-lang.org/ja/man/ chapter 10 (method jleft)
  def jleft_w_dots(len)
    len0 = self.length
    if len0 <= len
      self.dup
    else
      str = self[0,len]
      str[-1,1] = '' if /.\z/ !~ str
      str[-1,1] = '' if /.\z/ !~ str  # don't know why, but needed two times
      str + '..'
    end
  end

end
