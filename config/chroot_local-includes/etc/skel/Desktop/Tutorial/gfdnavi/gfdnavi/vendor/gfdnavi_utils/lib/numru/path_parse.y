# -*- coding: japanese-cp932 -*-
class PathParser
  prechigh
    nonassoc slice
    left method
    left path
  preclow
rule
  target: resource { result = [val[0]] unless result.kind_of?(Array) }

  resource: path { result = val[0] }
          | resource method
             { if val[0].kind_of?(String) # "path"
                 result = [val[0], val[1]]
               elsif val[0][-1].kind_of?(String) # [res, "path"]
                 result = [val[0], val[1]]
               elsif val[0][-1][0].kind_of?(Symbol) # [res,[:method,opt]]
                 result << val[1]
               else # [res,[res,[:method,opt]]]
                 result = [val[0], val[1]]
               end
             }
          | BRACKETOPEN resources BRACKETCLOSE { result = val[1] }
          | FIND argstring ")" { result = ["/", [val[0].to_sym, val[1]]] }
          | path FIND argstring ")" { result = [val[0], [val[1].to_sym, val[2]]] }

  resources: resource
           | resources "," resource
             { if val[0].kind_of?(String) # "path"
                 result = [val[0], val[2]]
               elsif val[0][-1].kind_of?(String) # [res, "path"]
                 result << val[2]
               elsif val[0][-1][0].kind_of?(Symbol) # [res, [:method,opt]]
                 result = [val[0], val[2]]
               elsif val[0][-1][0] # [res, [res, [:method,opt]]]
                 result << val[2]
               end
             }
  method: METHOD ")" { result = [val[0].to_sym, nil] }
        | METHOD argstring ")" { result = [val[0].to_sym, val[1]] }
        | METHOD argstring ";" ")" { result = [val[0].to_sym, val[1..-2].join] }
        | METHOD argstring ";" argstring ")" { result = [val[0].to_sym, val[1..-2].join] }
        | SLICE { result = [:slice, val[0].split(",").map{|s| s.to_i}] }
        | FIND argstring ")" { result = [val[0].to_sym, val[1]] }

  argstring: argsubstring
           | argstring argsubstring { result << val[1] }
           | argstring "," argstring { result = val.join }

  argsubstring: STRING
              | NUMBER

  path: path node { if val[0] == "/" then raise("'//' is not allowed") else result << val[1] end }
      | node
      | "/"

  node: "/" pathstring { result << val[1] }

  pathstring: pathsubstring
            | pathstring pathsubstring { result << val[1] }

  pathsubstring: STRING
               | PATHSTRING
               | NUMBER
               | "["
               | "]"
               | "("
               | ")"
               | ";"
end

---- header
---- inner
  
  METHODS = "(plot|analysis|overlay|cut)"
  def parse(str)
    @yydebug = true
    unless str.kind_of?(String)
      raise "argument must be String"
    end
    @q = []
    until str.empty?
      case str
      when /\A\[\d+(,\d+)?\]/         # '[00,00,...]'
        @q.push [:SLICE, $&[1..-2]]
      when /\A\/\[/                   # '/['
        @q.push [:BRACKETOPEN, "["]
      when /\A\]\]/                   # ']]'
        @q.push [:BRACKETCLOSE, "]"]
        @q.push [:BRACKETCLOSE, "]"]
      when /\A\]\[\d+(,\d+)?\]/       # '][00,00,...]'
        @q.push [:BRACKETCLOSE, "]"]
        @q.push [:SLICE, $&[2..-2]] 
      when /\A\]\/#{METHODS}\(/o      # ']/method('
        @q.push [:BRACKETCLOSE, "]"]
        @q.push [:METHOD, $&[2..-2]]
      when /\A\],\//                  # '],/'
                                      # '],' ‚ÅI‚í‚énode–¼‚Í‹ÖŽ~
        @q.push [:BRACKETCLOSE, "]"]
        @q.push [",", ","]
        @q.push ["/", "/"]
      when /\A\]\Z/                   # ']' ÅŒã
        @q.push [:BRACKETCLOSE, "]"]
      when /\A\/#{METHODS}\(/o        # '/method('
        @q.push [:METHOD, $&[1..-2]]
      when /\A\/find\(/               # '/find('
        @q.push [:FIND, $&[1..-2]]
      when /\A[\/;,\[\]\)]/           # '/;,[])' ‚Ì‚¢‚¸‚ê‚©
        @q.push [$&, $&]
      when /\A\d+/                    # '00...'
        @q.push [:NUMBER, $&]
      when /\A[\w=>\-\.\s&]+/
        @q.push [:STRING, $&]
      when /\A[<+_@#^!~]+/
        @q.push [:PATHSTRING, $&]
      end
      str = $'
    end
    @q.push [false, '$end']
    do_parse
  end

  def next_token
    @q.shift
  end

---- footer

