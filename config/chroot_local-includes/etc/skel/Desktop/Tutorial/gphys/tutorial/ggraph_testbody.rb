  require "numru/ggraph"
  include NumRu

  # < read command line option if any >

  if ARGV.length == 1
    iws = ARGV[0].to_i
  else
    iws = 1
  end

  # < show default parameters >
  xdummy = ydummy = nil
  begin
    print "** GGraph::fig options **\n"
    GGraph.fig(xdummy, ydummy, 'help'=>true)
  rescue
  end
  begin
    print "** GGraph::axes options **\n"
    GGraph.axes(xdummy, ydummy, 'help'=>true)
  rescue
  end
  gp_dummy = nil
  begin
    print "** GGraph::line options **\n"
    GGraph.line(gp_dummy,true,'help'=>true)
  rescue
  end
  begin
    print "** GGraph::mark options **\n"
    GGraph.mark(gp_dummy,true,'help'=>true)
  rescue
  end
  begin
    print "** GGraph::contour options **\n"
    GGraph.contour(gp_dummy,true,'help'=>true)
  rescue
  end
  begin
    print "** GGraph::tone options **\n"
    GGraph.tone(gp_dummy,true,'help'=>true)
  rescue
  end

  #< graphic test / demonstration >
  file = 'T.jan.nc'
  gphys = GPhys::NetCDF_IO.open(file, 'T')
  DCL.gropn(iws)
  DCL.sldiv('y',2,2)
  DCL.sgpset('lcntl', false)
  DCL.sgpset('lfull',true)
  DCL.sgpset('lfprop',true)
  DCL.uzfact(0.7)
  #/ graph 1 /
  GGraph.set_fig('viewport'=>[0.25,0.75,0.12,0.62])
  GGraph.line(gphys.cut(true,35,true).mean(0), true)
  #/ graph 2 /
  GGraph.next_fig('itr'=>2)
  GGraph.next_axes('yunits'=>'','xunits'=>'')
  GGraph.line(gphys.cut(true,35,true).mean(0), true, 
	      'exchange'=>true, 'index'=>3, 'title'=>'TEMERATURE', 'annotate'=>false)
  GGraph.mark(gphys.cut(true,35,true).mean(0), false, 
	      'exchange'=>true, 'type'=>3)
  #/ graph 3 /
  GGraph.contour(gphys)
  #/ graph 4 /
  GGraph.next_fig('itr'=>2)
  GGraph.contour(gphys.cut(135,true,true))
  #/ graph 5 /
  GGraph.set_axes('xunits'=>'', 'yunits'=>'')
  GGraph.contour(gphys,true, 'min'=>0, 'coloring'=>true)
  #/ graph 6 /
  GGraph.set_fig('viewport'=>[0.2,0.8,0.15,0.55])
  GGraph.contour(gphys,true, 'nozero'=>true)
  #/ graph 7 /
  GGraph.contour(gphys,true, 'min'=>10, 'int'=>3)
  DCL.udpset('lmsg',false)
  GGraph.contour(gphys,false, 'max'=>-10, 'int'=>3)
  DCL.udpset('lmsg',true)
  #/ graph 8 /
  GGraph.set_contour_levels('levels'=>[-10,-5,0,5,10],'index'=>[3,1,3,1,3],
			'line_type'=>2)
  GGraph.contour(gphys)
  GGraph.clear_contour_levels
  #/ graph 9 /
  GGraph.contour(gphys, true, 'levels'=>[0,10,20], 'index'=>3)
  #/ graph 10 /
  GGraph.set_linear_contour_options('nlev'=>24)
  GGraph.next_linear_contour_options('coloring'=>true)
  GGraph.contour(gphys)
  #/ graph 11 /
  GGraph.tone(gphys, true, 'min'=>0)
  #/ graph 12 /
  GGraph.tone(gphys, true, 'ltone'=>false)
  GGraph.contour(gphys, false)
  #/ graph 13 /
  GGraph.next_fig('itr'=>3)
  GGraph.contour(gphys[1..-1,true,true])
  #/ graph 14 /
  GGraph.tone(gphys, true, 'levels'=>[-10,10],'patterns'=>[20999,50999,80999])
  #/ graph 15 /
  GGraph.tone(gphys, true, 'levels'=>[-10,10],'patterns'=>[50999,80999])
  #/ graph 16 /
  GGraph.tone(gphys, true, 'levels'=>[-10,10],'patterns'=>[50999])
  DCL.grcls
