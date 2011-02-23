def plus10(num)
   raise "Not a Numeric" if ! num.is_a?(Numeric)
   num + 10
end
def call_plus10(num)
   plus10(num)
end
p call_plus10("hi!")

