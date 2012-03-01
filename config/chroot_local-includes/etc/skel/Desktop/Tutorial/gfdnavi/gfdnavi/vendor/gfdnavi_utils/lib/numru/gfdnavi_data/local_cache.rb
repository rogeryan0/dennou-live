module NumRu
  module GfdnaviData
    class LocalCache
      DEFAULT_MAX_SIZE = 100000
      DEFAULT_MAX_NUMBER = 10000

      def initialize(max_size = DEFAULT_MAX_SIZE, max_num = DEFAULT_MAX_NUMBER)
        @max_size = max_size
        @max_num = max_num
        @hash = Hash.new
        @size = NArray.int(max_num)
        @score = NArray.int(max_num)
        @keys = ::Array.new
        @mutex = Mutex.new
      end

      def push(key, val, size=0, ctime=0)
        if @hash[key]
          return nil
        else
          return nil if size > @max_size/2
          @mutex.synchronize {
            if (@hash.length < @max_num-1) && (@size.sum + size < @max_size)
              id = @hash.length
            else
              id = pruning(size)
            end
            @hash[key] = {:val => _dump(val), :id => id, :calc_time => ctime, :time => Time.now, :count => 1}
            @keys[id] = key
            update_score(id)
          }
          return self
        end
      end

      def get(key)
        ret = nil
        @mutex.synchronize {
          if h = @hash[key]
            h[:time] = Time.now
            h[:count] += 1
            update_score(h[:id])
            ret = _load(h[:val])
          end
        }
        return ret
      end

      private

      if Rails.configuration.cache_classes
        def _load(obj)
          return obj
        end
        def _dump(obj)
          return obj
        end
      else
        def _load(obj)
          ::Directory
          ::Variable
          ::Image
          ::Knowledge
          ::ValueType
          ::DrawMethod
          DrawMethodOption
          ::Function
          FunctionArgument
          FunctionOutput
          return Marshal.load(obj)
        end
        def _dump(obj)
          return Marshal.dump(obj)
        end
      end

      def pruning(size)
        i = @score.eq(@score.min).where[0]
        @hash.delete(@keys[i])
        @keys[i] = nil
        @size[i] = 0
        @score[i] = 0
        if @size.sum + size > @max_size && @hash.length > 1
          pruning(size)
        end
        return i
      end

      def update_score(id)
        h = @hash[@keys[id]]
        @score[id] = h[:time].to_i + h[:count]*100 + h[:calc_time]
      end
    end # end of LocalCache class
  end
end
