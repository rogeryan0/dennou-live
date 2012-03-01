module RD
  HEAD_FILTER = Filter.new(:target) do |inn, out|
    $Visitor.head = inn.to_s
    out.print " "
  end
end

$RC["filter"]["head"] = RD::HEAD_FILTER
