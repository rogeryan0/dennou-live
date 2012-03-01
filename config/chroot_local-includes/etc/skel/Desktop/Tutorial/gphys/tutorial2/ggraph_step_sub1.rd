tone のオプションの全体

 irb(main):001:0> tone nil, true, "help"=>true
  *** MESSAGE (SWDOPN) ***  GRPH1 : STARTED / IWS =  1.                         
 << Description of options >>
   option name	default value	# description:
   "title"	nil	# Title of the figure(if nil, internally
                         # determined)
   "annotate"	true	# if false, do not put texts on the right
                         # margin even when newframe==true
   "ltone"	true	# Same as udpack parameter ltone
   "auto"	false	# Swith DCL.uetone and DCL.uetonf depending on the
                         # data size
   "tonf"	false	# Use DCL.uetonf instead of DCL.uetone
   "tonb"	false	# Use DCL.uetonb instead of DCL.uetone
   "tonc"	false	# Use DCL.uetonc instead of DCL.uetone
   "clr_min"	nil	# if an integer (in 10..99) is specified, used as
                         # the color number for the minimum data values.
                         # (the same can be done by setting the uepack
                         # parameter "icolor1")
   "clr_max"	nil	# if an integer (in 10..99) is specified, used as
                         # the color number for the maximum data values.
                         # (the same can be done by setting the uepack
                         # parameter "icolor2")
   "map_axes"	false	# [USE IT ONLY WHEN itr=10 (cylindrical)] If
                         # true, draws axes by temprarilly switching to
                         # itr=1 and calling GGraph::axes.
   "keep"	false	# Use the tone levels and patterns used previously
   "color_bar"	false	# Add a color bar: THIS IS ONLY FOR QUICK
                         # LOOK. Use the GGraph::color_bar method explicitly
                         # for full option control
   "min"	nil	# minimum tone level
   "max"	nil	# maximum tone level
   "nlev"	nil	# number of levels
   "interval"	nil	# contour interval
   "help"	false	# show help message if true
   "log"	nil	# approximately log-scaled levels (by using
                         # DCLExt::quasi_log_levels)
   "log_cycle"	3	# (if log) number of levels in one-order (1 or 2
                         # or 3)
   "levels"	nil	# tone levels  (Array/NArray of Numeric). Works
                         # together with patterns
   "patterns"	nil	# tone patters (Array/NArray of Numeric). Works
                         # together with levels
   "exchange"	false	# whether to exchange x and y axes
   "transpose"	false	# if true, exchange x and y axes
   "xintv"	1	# interval of data sampling in x
   "yintv"	1	# interval of data sampling in y
   "xcoord"	nil	# Name of the coordinate variable for x-axis
   "ycoord"	nil	# Name of the coordinate variable for y-axis
   "slice"	nil	# An Array to be pathed to the GPhys#[] method to
                         # subset the data before plotting (order applied:
                         # slice -> cut -> mean)
   "cut"	nil	# An Array or Hash to be pathed to the GPhys#cut
                         # method to subset the data before plotting (order
                         # applied: slice -> cut -> mean)
   "mean"	nil	# An Array to be pathed to the GPhys#mean method to
                         # take mean of the data before plotting (order
                         # applied: slice -> cut -> mean)
