a = Array.new(2)
begin
 file = File.open("hello.txt")
 a[0] = file.readline
 a[1] = file.readline
rescue
 a[1] = "file is end"
ensure
 file.close if file
end
p a
