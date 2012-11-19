require "numru/gfdnavi_data/base"

module NumRu::GfdnaviData
  class Variable < NumRu::GfdnaviData::Base

    def analysis(*args)
      ary = NumRu::GfdnaviData::Array[self]
      ary.user = @user
      ary.analysis(*args)
    end

    def plot(*args)
      ary = NumRu::GfdnaviData::Array[self]
      ary.user = @user
      ary.plot(*args)
    end
  end
end
