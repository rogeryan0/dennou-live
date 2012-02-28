def add_conditions(conditions, new_condition, andor="AND")
  if conditions.nil?
    conditions = ["(#{new_condition})"]
  elsif Array === conditions
    conditions[0] += " #{andor} (#{new_condition})"
  elsif String === conditions
    conditions += " #{andor} (#{new_condition})"
  elsif Hash === conditions
    new_cond = Array.new
    conditions = conditions.each{|k,v|
      new_cond[0] = new_cond[0] || ""
      new_cond[0] += "#{k}=?"
      new_cond << v
    }
    conditions = new_cond
  else
    raise "bug"
  end
  conditions
end


def concat(*ary)
  if ActiveRecord::Base.connection.adapter_name=="MySQL"
    "concat(#{ary.join(",")})"
  else
    "#{ary.join("||")}"
  end
end


def boolean_condition(name)
  if ActiveRecord::Base.connection.adapter_name=="SQLite"
    "#{name}=='t'"
  else
    name
  end
end

def temp_storage 
  if ActiveRecord::Base.connection.adapter_name=="MySQL"
    #return "ENGINE=MEMORY"
    return "ENGINE=InnoDB"
  else
    return ""
    end
end

