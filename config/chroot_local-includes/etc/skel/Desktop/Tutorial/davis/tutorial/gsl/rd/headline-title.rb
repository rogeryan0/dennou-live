module RD
  module HeadLineTitle
    module_function
    def title_init
      @headline_called = false
    end
    
    def make_title(title)
      unless @headline_called
        @title = title.join.strip
        @headline_called = true
      end
    end
  end
end
