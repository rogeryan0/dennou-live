class PathNode
  attr_accessor :path,:entries,:children,:count,:parent
  def initialize
    @path=""
    @entries=Array.new
    @parent=nil
    @children=Hash.new
    @count=0
  end

  def addpath_r(strary)
    @count=@count+1
    if strary.size==0 then
      return self
    else
      cur = strary.shift()
      if @children.key?(cur) then
	@children[cur].addpath_r(strary)
      else
	newnode=PathNode.new
        newnode.path=cur
        @children[cur]=newnode
	newnode.addpath_r(strary)
      end
    end
  end

  def find_child(str)
    if @children then
      @children.each{|c|
        if c.path==str then
          return c
        end
      }
    end
    return
  end
  
  def addpath(n)
    if @path=="/" then
     strary=n.path.split(/\//)
     strary.delete_at(0)
     #entry=strary[strary.size-1]
     strary.pop
     node=addpath_r(strary)
     node.entries.push(n)
    end
  end

  
end


#root=PathNode.new
#root.path="/"
#addpath("/a0/b0/c0/d0",root)
#addpath("/a0/b1/c0/d0",root)
#addpath("/a0/b1/c0/d1",root)
#addpath("/a1/b1/c0/d1",root)
#root.printpath(0)
