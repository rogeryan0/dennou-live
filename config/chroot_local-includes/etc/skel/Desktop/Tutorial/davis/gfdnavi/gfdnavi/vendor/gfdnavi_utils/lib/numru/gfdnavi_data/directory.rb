require "numru/gfdnavi_data/base"

module NumRu::GfdnaviData
  class Directory < NumRu::GfdnaviData::Base

    %w(variable image).each do |name|
      eval <<-EOL
        def #{name}_nodes
          create(get_attribute("#{name}_nodes"))
        end
      EOL
    end
  end
end
