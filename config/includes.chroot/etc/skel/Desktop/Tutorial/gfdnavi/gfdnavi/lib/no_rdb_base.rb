module ActiveRecord #:nodoc:
  class NoRdbBase < Base
    class << self # Class methods
      %w(find exists?
         create update delete destroy update_all destroy_all
         count_by_sql
         table_exists?
         construct_finder_sql
         add_order! add_limit! add_lock! add_joins! add_conditions! type_condition
         determine_finder determine_deprecated_finder determine_instantiator extract_attribute_names_from_match
         attribute_condition expand_id_conditions
         sanitize_sql sanitize_sql_hash sanitize_sql_array sanitize_conditions
         validate_find_options
      ).each{|name|
        undef_method(name) if instance_methods.include?(name)
      }

      def connection
        raise "this is NoRdbBase class. No connection exists"
      end

      def push_column(column)
        @columns ||= Array.new
        @columns.push column
      end

      def columns
        @columns
      end

    end

    %w(save save! destroy reload create_or_update update create
       interpolate_sql
    ).each{|name|
      undef_method(name) if self.instance_methods.include?(name)
    }

  end
end
