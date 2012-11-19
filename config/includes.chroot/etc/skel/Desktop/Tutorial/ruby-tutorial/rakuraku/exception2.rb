def power(x,y)
 if Numeric===x
   return x**y
 else
   raise "x must be a kind of Numeric"
 end
end
p power(4,2)
p power("A",2)
