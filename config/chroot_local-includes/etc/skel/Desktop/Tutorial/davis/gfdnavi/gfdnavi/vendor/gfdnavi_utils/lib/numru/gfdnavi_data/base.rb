module NumRu
  module GfdnaviData

    class Base
   
      def initialize(hash={})
        hash = hash.dup
        @user = hash.delete("user")
        @object = hash.delete("object")

        if /Remote/ =~ self.class.name 
          self.extend(NumRu::GfdnaviData::Remote)
          $remote=1
        elsif /Local/ =~ self.class.name
          self.extend(NumRu::GfdnaviData::Local)
          $local=1
          #add
        elsif /Array/ =~ self.class.name
          if $local
            self.extend(NumRu::GfdnaviData::Local)
          elsif $remote
            self.extend(NumRu::GfdnaviData::Remote)
          end
        else
          raise "this is abstract class"
        end
        self.init(hash)
        unless hash.empty?
          raise "#{hash.keys.join(', ')} are invalid arguments"
        end
      end

      def path
        get_attribute("path")
      end

      def name
        get_attribute("name")
      end

      def user
        @user
      end

      def parent
        attr = get_attribute("parent")
        create(attr)
      end

      def children
        attr = get_attribute("children")
        create(attr)
      end

      def variables
        attr = get_attribute("variable_nodes")
        create(attr)
      end

      def images
        attr = get_attribute("image_nodes")
        create(attr)
      end

      def attributes=(hash)
        hash.each do |k,v|
          @object.send("#{k}=", v)
        end
      end

      [["+","addtion"],
       ["-","subtraction"],
       ["*","multiplication"],
       ["/","division"]
      ].each do |m,f|
        eval <<-EOL
        def #{m}(other)
          case other
          when NumRu::GfdnaviData::Variable
            # do nothing
          when NumRu::GfdnaviData::Array
            if other.length == 1
              other = other[0]
            else
              raise "other.length must be 1"
            end
          else
            raise "other must be kind of GfdanviData::Base"
          end
            ary = NumRu::GfdnaviData::Array[self, other]
            ary.user = @user
            ary.analysis("#{f}")
        end
        EOL
      end

    end
  end
end
