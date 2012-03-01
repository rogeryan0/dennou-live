def plus10(num)
   raise "Not a Numeric" if ! num.is_a?(Numeric)
   num + 10
end
def plus_str10(num)
   num + "10"
end

s = "20"
begin
   p plus10(s)
rescue
   p plus_str10(s)
end

