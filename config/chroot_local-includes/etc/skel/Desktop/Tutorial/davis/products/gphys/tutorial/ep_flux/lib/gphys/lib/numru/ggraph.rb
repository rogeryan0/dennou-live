require "numru/gphys"
require "numru/dcl"
require "numru/misc"

############################################################

=begin
=module NumRu::GGraph and others in ggraph.rb

==Index
* ((<module NumRu::GGraph>))
  * ((<title>))
    Shows title by (({DCL.uxmttl('t',string,0.0)})).
    Graphic methods such as ((<contour>)) calls this by default.
  * ((<annotate>))
    Show texts on the right margin of the viewport.
    Graphic methods such as ((<contour>)) calls this by default.
  * ((<fig>))
    Define a figure by setting viewport, window, and coordinate transform 
    id (itr) from two 1D VArrays (({xax})) and (({yax})). 
  * ((<set_fig>))
    Change the default option values for ((<fig>)).
  * ((<next_fig>))
    Set the option values effective only in the next call of ((<fig>))
    (cleared then).
  * ((<axes>))
    Draw axes using (by default) info in (({xax})) and (({yax})) if non-nil.
  * ((<set_axes>))
    Change the default option values for ((<axes>)).
  * ((<next_axes>))
    Set the option values effective only in the next call of ((<axes>))
    (cleared then).
  * ((<line>))
    Plot a poly-line by selecting the first dimension (with GPhys#first1D)
    if (({gphys})) is more than 2D.
  * ((<mark>))
    Similar to ((<line>)) but plots marks instead of drawing a poly-line.
  * ((<contour>))
    Contour plot by selecting the first 2 dimensions (with GPhys#first2D)
    if (({gphys})) is more than 3D.
  * ((<set_contour_levels>))
    Set contour levels for ((<contour>)) explicitly by values with the option (({levels})).
  * ((<clear_contour_levels>))
    Clear contour levels set by ((<set_contour_levels>)).
  * ((<set_linear_contour_options>))
    Change the default option values regarding linear contour level
    setting in ((<contour>)).
  * ((<next_linear_contour_options>))
    Similar to ((<set_linear_contour_options>)) but the setting
    is effective only for the next call of ((<contour>)).
  * ((<tone>))
    Color tone or shading by selecting the first 2 dimensions 
    (with GPhys#first2D) if (({gphys})) is more than 3D.
  * ((<set_tone_levels>))
    Set tone levels for ((<tone>)) explicitly by values.
  * ((<clear_tone_levels>))
    Clear tone levels set by ((<set_tone_levels>)).
  * ((<set_linear_tone_options>))
    Change the default option values regarding linear tone level
    setting in ((<tone>)).
  * ((<next_linear_tone_options>))
    Similar to ((<set_linear_tone_options>)) but the setting
    is effective only for the next call of ((<tone>)).
* ((<module NumRu::DCLExt>))
  * ((<gl_set_params>))
    Calls (({DCL.glpset})) multiple times (for each key and val of (({hash}))).
  * ((<sg_set_params>))
    Calls (({DCL.sgpset})) multiple times (for each key and val of (({hash}))).
  * ((<sl_set_params>))
    Calls (({DCL.slpset})) multiple times (for each key and val of (({hash}))).
  * ((<sw_set_params>))
    Calls (({DCL.swpset})) multiple times (for each key and val of (({hash}))).
  * ((<uz_set_params>))
    Calls (({DCL.uzpset})) multiple times (for each key and val of (({hash}))).
  * ((<ul_set_params>))
    Calls (({DCL.ulpset})) multiple times (for each key and val of (({hash}))).
  * ((<uc_set_params>))
    Calls (({DCL.ucpset})) multiple times (for each key and val of (({hash}))).
  * ((<uu_set_params>))
    Calls (({DCL.uupset})) multiple times (for each key and val of (({hash}))).
  * ((<us_set_params>))
    Calls (({DCL.uspset})) multiple times (for each key and val of (({hash}))).
  * ((<ud_set_params>))
    Calls (({DCL.udpset})) multiple times (for each key and val of (({hash}))).
  * ((<ud_set_linear_levs>))
    Set contour levels with a constant interval
  * ((<ud_set_contour>))
    Set contours of at specified levels.
  * ((<ud_add_contour>))
    Same as ((<ud_set_contour>)), but does not clear the contour levels that have
    been set.
  * ((<ue_set_params>))
    Calls (({DCL.uepset})) multiple times (for each key and val of (({hash}))).
  * ((<ue_set_linear_levs>))
    Set tone levels with a constant interval
  * ((<ue_set_tone>))
    Set tone levels and patterns.
  * ((<ue_add_tone>))
    Same as ((<ue_set_tone>)), but does not clear the tone levels that have
    been set.
  * ((<ug_set_params>))
    Calls (({DCL.ugpset})) multiple times (for each key and val of (({hash}))).
    See ((<gl_set_params>)) for usage.
  * ((<um_set_params>))
    Calls (({DCL.umpset})) multiple times (for each key and val of (({hash}))).


=module NumRu::GGraph

A graphic library for GPhys using RubyDCL.

This module uses GPhys but is not a part of it.
More specifically, even though this module is included in
the GPhys distribution, the class NumRu::GPhys knows nothing about it,
and GGraph accesses GPhys only though public methods.
So GGraph is not an insider, and you can make another graphic 
library if you like.

==Module Functions

---title(string)
    Shows title by (({DCL.uxmttl('t',string,0.0)})).
    Graphic methods such as ((<contour>)) calls this by default.

    RETURN VALUE
    * nil

---annotate(str_ary)
    Show texts on the right margin of the viewport.
    Graphic methods such as ((<contour>)) calls this by default.

    ARGUMENTS
    * str_ary (Array of String) : 

    RETURN VALUE
    * nil

---fig(xax, yax, options=nil)
    Define a figure by setting viewport, window, and coordinate transform 
    id (itr) from two 1D VArrays (({xax})) and (({yax})). 

    (({DCL.grfrm})) or (({DCL.grfig})) is called depending options provided.

    ARGUMENTS
    * xax (VArray): a VArray representing the x (horizontal) coordinate
      of the figure.
      The x range of the window (UXMIN & UYMAX in DCL) are determined
      with the max and min of (({xax.val})). By default, 
      the min and max values are assigned to the left and right
      boundaries, respectively, but it is reversed if (({xax})) has
      a 'positive' attribute and its value is 'down' etc (see (({options}))).
    * yax (VArray): similar to (({xax})) but for the y (vertical) coordinate
      of the figure.
    * options (Hash) : options to change the default behavior if specified.
      It is a Hash with option names (String) as keys and their values.
      Options are interpreted by a NumRu::Misc::KeywordOptAutoHelp,
      so you can shorten the keys (by omitting tails) as long as it is 
      unambiguous.
       option name   default value   # description:
       "new_frame"   true    # whether to define a new frame by DCL.grfrm
                             # (otherwise, DCL.grfig is called)
       "window"      nil     # [uxmin, uxmax, uymin, uymax]
       "viewport"    [0.2, 0.8, 0.2, 0.8]    # [vxmin, vxmax, vymin, vymax]
       "itr"         1       # coordinate transformation number
       "xreverse"    "positive:down,units:hPa"       # Assign max value to
                             # UXMIN and min value to UXMAX if condition is
                             # satisfied (nil:never, true:always, String: when
                             # an attribute has the value specified
                             # ("key:value,key:value,..")
       "yreverse"    "positive:down,units:hPa"       # Assign max value to
                             # UYMIN and min value to UYMAX if condition is
                             # satisfied (nil:never, true:always, String: when
                             # an attribute has the value specified
                             # ("key:value,key:value,..")
       "help"        false   # show help message if true

    RETURN VALUE
    * nil

    POSSIBLE EXCEPTIONS
    * those from NumRu::DCL if any / TypeError if any
    * options has a key that does not match any of the option names.
    * options has a key that is ambiguous

---set_fig(options)
    Change the default option values for ((<fig>)).

    ARGUMENTS
    * options (Hash) : The usage is the same as (({options})) for ((<fig>)).

    RETURN VALUE
    * a Hash containing the values replaced (the ones before calling this
      method)

    POSSIBLE EXCEPTIONS
    * see ((<fig>)).

---next_fig(options)
    Set the option values effective only in the next call of ((<fig>))
    (cleared then).

    These value are overwritten if specified explicitly in the next
    call of ((<fig>)).

    ARGUMENTS
    * options (Hash) : The usage is the same as (({options})) for ((<fig>)).

    RETURN VALUE
    * nil

    POSSIBLE EXCEPTIONS
    * see ((<fig>)).

---axes(xax=nil, yax=nil, options=nil)
    Draw axes using (by default) info in (({xax})) and (({yax})) if non-nil.

    ARGUMENTS
    * xax (nil or VArray): if non-nil, attributes 'long_name' and 'units' 
      are read to define (({xtitle})) and (({xunits})) (see below).
      These are overwritten by explicitly specifying (({xtitle})) and 
      (({xunits})).
    * yax (nil or VArray): if non-nil, attributes 'long_name' and 'units' 
      are read to define (({ytitle})) and (({yunits})) (see below).
      These are overwritten by explicitly specifying (({ytitle})) and 
      (({yunits})).
    * options (Hash) : options to change the default behavior if specified.
      It is a Hash with option names (String) as keys and their values.
      Options are interpreted by a NumRu::Misc::KeywordOptAutoHelp,
      so you can shorten the keys (by omitting tails) as long as it is 
      unambiguous.
       option name   default value   # description:
       "xside"       "tb"    # Where to draw xaxes (combination of t, b and u)
       "yside"       "lr"    # Where to draw yaxes (combination of l, r and u)
       "xtitle"      nil     # Title of x axis (if nil, internally determined)
       "ytitle"      nil     # Title of y axis (if nil, internally determined)
       "xunits"      nil     # Units of x axis (if nil, internally determined)
       "yunits"      nil     # Units of y axis (if nil, internally determined)
       "xtickint"    nil     # Interval of x axis tickmark
                             #                 (if nil, internally determined)
       "ytickint"    nil     # Interval of y axis tickmark
                             #                 (if nil, internally determined)
       "xlabelint"   nil     # Interval of x axis label
                             #                 (if nil, internally determined)
       "ylabelint"   nil     # Interval of y axis label
                             #                 (if nil, internally determined)
       "help"        false   # show help message if true

    RETURN VALUE
    * nil

    POSSIBLE EXCEPTIONS
    * those from NumRu::DCL if any / TypeError if any
    * options has a key that does not match any of the option names.
    * options has a key that is ambiguous

---set_axes(options)
    Change the default option values for ((<axes>)).

    ARGUMENTS
    * options (Hash) : The usage is the same as (({options})) for ((<axes>)).

    RETURN VALUE
    * a Hash containing the values replaced (the ones before calling this
      method)

    POSSIBLE EXCEPTIONS
    * see ((<axes>)).

---next_axes(options)
    Set the option values effective only in the next call of ((<axes>))
    (cleared then).

    These value are overwritten if specified explicitly in the next
    call of ((<axes>)).

    ARGUMENTS
    * options (Hash) : The usage is the same as (({options})) for ((<axes>)).

    RETURN VALUE
    * nil

    POSSIBLE EXCEPTIONS
    * see ((<axes>)).

---line(gphys, newframe=true, options=nil)
    Plot a poly-line by selecting the first dimension (with GPhys#first1D)
    if (({gphys})) is more than 2D.

    ARGUMENTS
    * gphys (GPhys) : a GPhys whose data is plotted.
    * newframe (true/false) : if true, calls ((<fig>)), ((<axes>)),
      ((<title>)), and ((<annotate>)) internally; if false, only
      the poly-line is drawn (overlaid to the exiting figure).
    * options (Hash) : options to change the default behavior if specified.
      It is a Hash with option names (String) as keys and their values.
      Options are interpreted by a NumRu::Misc::KeywordOptAutoHelp,
      so you can shorten the keys (by omitting tails) as long as it is 
      unambiguous.
       option name   default value   # description:
       "title"       nil     # Title of the figure(if nil, internally
                             # determined)
       "annotate"    true    # if false, do not put texts on the right
                             # margin even when newframe==true
       "exchange"    false   # whether to exchange x and y axes
       "index"       1       # line/mark index
       "type"        1       # line type
       "help"        false   # show help message if true

    RETURN VALUE
    * nil

---mark(gphys, newframe=true, options=nil)
    Similar to ((<line>)) but plots marks instead of drawing a poly-line.

    ARGUMENTS
    * gphys (GPhys) : a GPhys whose data is plotted.
    * newframe (true/false) : if true, calls ((<fig>)), ((<axes>)),
      ((<title>)), and ((<annotate>)) internally; if false, only
      the poly-line is drawn (overlaid to the exiting figure).
    * options (Hash) : options to change the default behavior if specified.
      It is a Hash with option names (String) as keys and their values.
      Options are interpreted by a NumRu::Misc::KeywordOptAutoHelp,
      so you can shorten the keys (by omitting tails) as long as it is 
      unambiguous.
       option name   default value   # description:
       "title"       nil     # Title of the figure(if nil, internally
                             # determined)
       "annotate"    true    # if false, do not put texts on the right
                             # margin even when newframe==true
       "exchange"    false   # whether to exchange x and y axes
       "index"       1       # mark index
       "type"        2       # mark type
       "size"        0.01    # marks size
       "help"        false   # show help message if true

    RETURN VALUE
    * nil

---contour(gphys, newframe=true, options=nil)
    Contour plot by selecting the first 2 dimensions (with GPhys#first2D)
    if (({gphys})) is more than 3D.

    Contour levels are determined as follows:
    * contour levels are set in this method if not set by
      ((<set_contour_levels>)) or the option (({"levels"})) is specified 
      explicitly.
    * When contour levels are set in this method, the option (({"levels"})) 
      has the highest precedence. If it is specified, options
      (({"index"})), (({"line_type"})), (({"label"})), and (({"label_height"}))
      are used.
      If (({"levels"})) are not specified, contour levels with a linear
      increment are set by using the options (({"min"})), (({"max"})),
      (({"nlev"})), (({"interval"})), (({"nozero"})), (({"coloring"})), 
      (({"clr_min"})), and (({"clr_max"})), which are interpreted by
      DCLExt::((<ud_set_linear_levs>)). The default values 
      of the linear level setting can be changed with
      ((<set_linear_contour_options>)).

    ARGUMENTS
    * gphys (GPhys) : a GPhys whose data is plotted.
    * newframe (true/false) : if true, calls ((<fig>)), ((<axes>)),
      ((<title>)), and ((<annotate>)) internally; if false, only
      the poly-line is drawn (overlaid to the exiting figure).
    * options (Hash) : options to change the default behavior if specified.
      It is a Hash with option names (String) as keys and their values.
      Options are interpreted by a NumRu::Misc::KeywordOptAutoHelp,
      so you can shorten the keys (by omitting tails) as long as it is 
      unambiguous.
       option name   default value   # description:
       "title"       nil     # Title of the figure(if nil, internally
                             # determined)
       "annotate"    true    # if false, do not put texts on the right
                             # margin even when newframe==true
       "min"         nil     # minimum contour value
       "max"         nil     # maximum contour value
       "nlev"        nil     # number of levels
       "interval"    nil     # contour interval
       "nozero"      nil     # delete zero contour
       "coloring"    false   # set color contours with ud_coloring
       "clr_min"     13      # (if coloring) minimum color id
       "clr_max"     100     # (if coloring) maximum color id
       "help"        false   # show help message if true
       "levels"      nil     # contour levels (Array/NArray of Numeric)
       "index"       nil     # (if levels) line index(es) (Array/NArray of
                             # integers, Integer, or nil)
       "line_type"   nil     # (if levels) line type(s) (Array/NArray of
                             # integers, Integer, or nil)
       "label"       nil     # (if levels) contour label(s) (Array/NArray of
                             # String, String, true, false, nil). nil is
                             # recommended.
       "label_height"  nil   # (if levels) label height(s) 
                             # (Array/NArray of Numeric, Numeric, or nil).
                             #  nil is recommended.

    RETURN VALUE
    * nil

---set_contour_levels(options)
    Set contour levels for ((<contour>)) explicitly by values with the option (({levels})).

    ARGUMENTS
    * options (Hash) : options to change the default behavior.
      The option (({"levels"})) is mandatory (so it is not optional!).
      Supported options are (({"levels"})), (({"index"})), 
      (({"line_type"})), (({"label"})), and (({"label_height"})).
      See ((<contour>)) for their description.

---clear_contour_levels
    Clear contour levels set by ((<set_contour_levels>)).

---set_linear_contour_options(options)
    Change the default option values regarding linear contour level
    setting in ((<contour>)).

    ARGUMENTS
    * options (Hash) : The usage is the same as (({options})) 
      for ((<contour>)) but supported options here are limited to
      (({"min"})), (({"max"})), (({"nlev"})), (({"interval"})), 
      (({"nozero"})), (({"coloring"})), (({"clr_min"})), and (({"clr_max"})). 

    RETURN VALUE
    * a Hash containing the values replaced (the ones before calling this
      method)

---next_linear_contour_options(options)
    Similar to ((<set_linear_contour_options>)) but the setting
    is effective only for the next call of ((<contour>)).

---tone(gphys, newframe=true, options=nil)
    Color tone or shading by selecting the first 2 dimensions 
    (with GPhys#first2D) if (({gphys})) is more than 3D.

    Tone levels are determined as follows:
    * Tone levels are set in this method if not set by
      ((<set_tone_levels>)) or the option (({"levels"})) (and
      optionally, (({"patterns"}))) is (are) specified explicitly.
    * When contour levels & patterns are set in this method, 
       * (({"levels"})) has the highest precedence. If it is specified,
         tone levels and patterns are determined by DCLExt::((<ue_set_tone>)).
         Here, tone patterns can be specified with the option (({"patterns"})).
       * Currently, option (({"patterns"})) is effective only if (({"levels"}))
         is specified. Otherwise, it is ignored and patterns are
         determined internally (by using DCL.uegtlb).
       * If not, a linear tone levels are set if (({"ltone"=true}))
         (this is the default), or shading is made if (({"ltone"=false})).
         Shading is determined by following the parameters in the UDPACK
         in DCL. Therefore, coloring is made if DCL.udpget('ltone')==true
         regardless the option (({"ltone"=false})) here.
         When linear levels are set in this method, options
         (({"min"})), (({"max"})), (({"nlev"})), and (({"interval"}))
         are used if specified, which are interpreted by 
         DCLExt::((<ue_set_linear_levs>)). 
         Their default values can be changed by
         ((<set_linear_tone_options>)).

    ARGUMENTS
    * gphys (GPhys) : a GPhys whose data is plotted.
    * newframe (true/false) : if true, calls ((<fig>)), ((<axes>)),
      ((<title>)), and ((<annotate>)) internally; if false, only
      the poly-line is drawn (overlaid to the exiting figure).
    * options (Hash) : options to change the default behavior if specified.
      It is a Hash with option names (String) as keys and their values.
      Options are interpreted by a NumRu::Misc::KeywordOptAutoHelp,
      so you can shorten the keys (by omitting tails) as long as it is 
      unambiguous.
       option name   default value   # description:
       "title"       nil     # Title of the figure(if nil, internally
                             # determined)
       "annotate"    true    # if false, do not put texts on the right
                             # margin even when newframe==true
       "ltone"       true    # Same as udpack parameter ltone
       "clr_min"     nil     # if an integer (in 10..99) is specified, used as
			     # the mimimum color number. (the same can be done
			     # by setting the uepack parameter "icolor1")
       "clr_max"     nil     # if an integer (in 10..99) is specified, used as
			     # the maximum color number. (the same can be done
			     # by setting the uepack parameter "icolor2")
       "min"         nil     # minimum contour value
       "max"         nil     # maximum contour value
       "nlev"        nil     # number of levels
       "interval"    nil     # contour interval
       "help"        false   # show help message if true
       "levels"      nil     # tone levels  (Array/NArray of Numeric). Works
                             # together with patterns
       "patterns"    nil     # tone patters (Array/NArray of Numeric). Works
                             # together with levels

    RETURN VALUE
    * nil

---set_tone_levels(options)
    Set tone levels for ((<tone>)) explicitly by values.

    ARGUMENTS
    * options (Hash) : options to change the default behavior.
      Supported options are (({"levels"})) and (({"patterns"})).
      Both of them must be specified explicitly (so they are 
      not optional!).

---clear_tone_levels
    Clear tone levels set by ((<set_tone_levels>)).

---set_linear_tone_options(options)
    Change the default option values regarding linear tone level
    setting in ((<tone>)).

    ARGUMENTS
    * options (Hash) : The usage is the same as (({options})) 
      for ((<tone>)) but supported options here are limited to
      (({"min"})), (({"max"})), (({"nlev"})), and (({"interval"})).

    RETURN VALUE
    * a Hash containing the values replaced (the ones before calling this
      method)

---next_linear_tone_options(options)
    Similar to ((<set_linear_tone_options>)) but the setting
    is effective only for the next call of ((<tone>)).

=module NumRu::DCLExt

Collection of various compound DCL functions for convenience.
This module is to be separated but temporarily included in ggraph.rb
while it is premature.

==Index
MATH1
* ((<glpack>))
GRPH1
* ((<sgpack>))
* ((<slpack>))
* ((<swpack>))
GRPH2
* ((<uzpack>))
* ((<ulpack>))
* ((<ucpack>))
* ((<uupack>))
* ((<uspack>))
* ((<udpack>))
* ((<uepack>))
* ((<ugpack>))
* ((<umpack>))

==Module Functions

===glpack
---gl_set_params(hash)
    Calls (({DCL.glpset})) multiple times (for each key and val of (({hash}))).

    ARGUMENTS
    * hash (Hash) : combinations of parameter names and values for udpset

    RETURN VALUE
    * a Hash containing the parameter names and their old values that were 
      replaced.

    EXAMPLES
    * You can modify parameters temporarily as follows.

        before = DCLExt.gl_set_params({'lmiss'=>true,'rmiss'=>9999.0})
        ....
        DCLExt.gl_set_params(before)     # reset the change

===sgpack
---sg_set_params(hash)
    Calls (({DCL.sgpset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

===slpack
---sl_set_params(hash)
    Calls (({DCL.slpset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

===swpack
---sw_set_params(hash)
    Calls (({DCL.swpset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

===uzpack
---uz_set_params(hash)
    Calls (({DCL.uzpset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

===ulpack
---ul_set_params(hash)
    Calls (({DCL.ulpset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

===ucpack
---uc_set_params(hash)
    Calls (({DCL.ucpset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

===uupack
---uu_set_params(hash)
    Calls (({DCL.uupset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

===uspack
---us_set_params(hash)
    Calls (({DCL.uspset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

===udpack
---ud_set_params(hash)
    Calls (({DCL.udpset})) multiple times (for each key and val of (({hash}))).

    ARGUMENTS
    * hash (Hash) : combinations of parameter names and values for udpset

    RETURN VALUE
    * a Hash containing the parameter names and their old values that were 
      replaced.

    EXAMPLES
    * You can modify parameters temporarily as follows.

        before = DCLExt.ud_set_params('indxmj'=>4,'lmsg'=>false)
        DCL.udcntz(data)
        DCLExt.ud_set_params(before)     # reset the change

---ud_set_linear_levs(v, options)
    Set contour levels with a constant interval

    ARGUMENTS
    * v : Data values to be fed to udcnt[rz]
    * options (Hash) : option specification by keys and values. Available
      options are 
          name   default value   description
          'min'      nil       minimum contour value (Numeric)
          'max'      nil       maximum contour value (Numeric)
          'nlev'     nil       number of levels (Integer)
          'interval' nil       contour interval (Numeric)
          'nozero'   nil       delete zero contour (true/false)
          'coloring' false     set color contours with ud_coloring (true/false)
          'clr_min'  13        (if coloring) minimum color id (Integer)
          'clr_max'  100       (if coloring) maximum color id (Integer)
      Here, (({interval})) has a higher precedence over (({nlev})).
      Since all the default values are nil, only those explicitly specified
      are interpreted. If no option is provided, the levels generated will
      be the default ones set by udcnt[rz] without any level specification.

---ud_set_contour(levels,index=nil,line_type=nil,label=nil,label_height=nil)
    Set contours of at specified levels.

    Normally you do not have to specify (({label})) and (({label_height})).

    It calls DCL.udsclv for each level. So the arguments are basically
    the same as DCL.udsclv, but only levels are mandatory here.

    ARGUMENTS
    * levels (Array, NArray, or Numeric) : contour levels to be set.
      If Numeric, a single level is set.
    * index (Array of integers, Integer, or nil) : 
      index(es) of the contours. If it is an Array and its length is
      shorter than that of (({levels})), the same Array is repeated (so 
      for instance [1,1,3] is interpreted as [1,1,3,1,1,3,1,1,3,...]).
      If it is a single Integer, all the contour will have the same index. 
     If nil, the value of 'indxmn' is used.
    * line_type (Array of integers, Integer, or nil) : 
      line type(s) of the contours. If it is an Array and its length is
      shorter than that of (({levels})), the same Array is repeated.
      the length must agree with that of (({levels})).
      If it is a single Integer, all the contour will have the same type. 
      If nil, set to be 1.
    * label (Array of String, String, true, false, nil) :
      Label(s) of the contours. If it is an Array and its length is
      shorter than that of (({levels})), the same Array is repeated.
      the length must agree with that of (({levels})).
      If  it is a single String, all the contour will have the same label. 
      If true, all the contours will have the labels representing the levels.
      If false, no label will be drawn (set to "").
      If nil, same as true for the contours whose index is equal to "INDXMJ",
      and same as false otherwise.
    * label_height (Array of Numeric, Numeric, or nil) :
      Heigh of Labels. Normally you do not have to use this.
      If it is an Array and its length is
      shorter than that of (({levels})), the same Array is repeated.
      If nil, the default value ("RSIZEL") is used for non-empty labels.
      If a single Numeric, the same value is used for all the contours.
      Note that it is recommended to not to use this parameter but
      use DCL.udpset('RZISEL'. label_height), since a positive value
      here always means to draw labels even when the label is empty.

    RETURN VALUE
    * nil

---ud_add_contour(levels,index=nil,line_type=nil,label=nil,label_height=nil)
    Same as ((<ud_set_contour>)), but does not clear the contour levels that have
    been set.

===uepack
---ue_set_params(hash)
    Calls (({DCL.uepset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

---ue_set_linear_levs(v, options)
    Set tone levels with a constant interval

    ARGUMENTS
    * v : Data values to be fed to udcnt[rz]
    * options (Hash) : option specification by keys and values. Available
      options are 
          name   default value   description
          'min'      nil         minimum tone level (Numeric)
          'max'      nil         maximum tone level (Numeric)
          'nlev'     nil         number of levels (Integer)
          'interval' nil         tone-level interval (Numeric)
      Here, (({interval})) has a higher precedence over (({nlev})).
      Since all the default values are nil, only those explicitly specified
      are interpreted. If no option is provided, the levels generated will
      be the default ones set by udcnt[rz] without any level specification.

---ue_set_tone(levels, patterns)
    Set tone levels and patterns.

    patterns are set between levels as follows:

     when (levels.length == patterns.length+1)

       levels[0]  |  levels[1]  |  levels[2]  ...  |  levels[-2]  |  levels[-1]
              patterns[0]   patterns[1]   ...  patterns[-2]   patterns[-1]

     when (levels.length == patterns.length)

       levels[0]  |  levels[1]  |  levels[2]  ...  |  levels[-1]  |  +infty
              patterns[0]   patterns[1]   ...  patterns[-2]   patterns[-1]

     when (levels.length == patterns.length-1)

       -infty  |  levels[0]  |  levels[1]  ...  |  levels[-1]  |  +infty
           patterns[0]   patterns[1]   ...  patterns[-2]   patterns[-1]

     else
       error (exception raised)

    ARGUMENTS
    * levels (Array or NArray of Numeric) : tone levels. Its length must be
      either 1 larger than, equal to, or 1 smaller than the length of patterns
    * patterns (Array or NArray of Numeric) : tone patterns

    RETURN VALUE
    * nil

---ue_add_tone(levels, patterns)
    Same as ((<ue_set_tone>)), but does not clear the tone levels that have
    been set.

===ugpack
---ug_set_params(hash)
    Calls (({DCL.ugpset})) multiple times (for each key and val of (({hash}))).
    See ((<gl_set_params>)) for usage.

===umpack
---um_set_params(hash)
    Calls (({DCL.umpset})) multiple times (for each key and val of (({hash}))).

    See ((<gl_set_params>)) for usage.

=end
############################################################

module NumRu

  module DCLExt
    # to be included in the RubyDCL distribution

    module_function

    #<<< for many packages >>>

    %w!gl sg sl sw uz ul uc uu us ud ue ug um!.each do |pkg|
      eval <<-EOS, nil, __FILE__, __LINE__+1
        def #{pkg}_set_params(hash)
          before = Hash.new
          hash.each{|k,v|
            before[k]=DCL.#{pkg}pget(k)
            if(v.is_a? String) then
              DCL.#{pkg}cset(k,v)
            else
              DCL.#{pkg}pset(k,v)
            end
          }
          before
        end
      EOS
    end

    #<<< module data >>>

    @@empty_hash = Hash.new

    #<<< udpack >>>

    def ud_coloring(clr_min=13, clr_max=100)
      # change the colors of existing contours to make a gradation
      # (rainbow colors with the default color map).
      nlev = DCL.udqcln
      cont_params = Array.new
      for i in 1..nlev
        cont_params.push( DCL.udqclv(i) )   # => [zlev,indx,ityp,clv,hl]
      end
      DCL.udiclv     # clear the contours

      colors = clr_min + 
               NArray.int(nlev).indgen! * (clr_max-clr_min) / nlev

      cont_params.sort!
      for i in 0...nlev
        cont_params[i][1] += colors[i]*10   # indx += colors[i]*10
        DCL.udsclv(*cont_params[i])
      end
    end

    def ud_set_linear_levs(v, options=nil)
      #Accepted options
      #  name        default  description
      #  'min'       nil      minimum contour value (Numeric)
      #  'max'       nil      maximum contour value (Numeric)
      #  'nlev'      nil      number of levels (Integer)
      #  'interval'  nil      contour interval (Numeric)
      #  'nozero'    false    delete zero contour (true/false)
      #  'coloring'  false    set color contours with ud_coloring (true/false)
      #  'clr_min'   13       (if coloring) minimum color id (Integer)
      #  'clr_max'   100      (if coloring) maximum color id (Integer)
      options = @@empty_hash if !options
      raise TypeError, "options must be a Hash" if !options.is_a?(Hash)
      min = options['min']
      max = options['max']
      nlev = options['nlev']
      interval = options['interval']
      nozero = options['nozero']
      if interval
        dx = interval
      elsif nlev
        dx = -nlev
      else
        dx = 0
      end
      if min || max
        min = v.min if !min
        max = v.max if !max
        DCL.udgcla(min, max, dx)
      else
        DCL.udgclb(v, dx)
      end
      if nozero
        DCL.uddclv(0.0)
      end
      if options['coloring']
        clr_min = ( options['clr_min'] || 13 )
        clr_max = ( options['clr_max'] || 100 )
        ud_coloring( clr_min, clr_max )
      end
    end

    def ud_set_contour(*args)
      DCL.udiclv
      ud_add_contour(*args)
    end

    def ud_add_contour(levels,index=nil,line_type=nil,label=nil,label_height=nil)

      # < check levels >
      case levels
      when Array, NArray
        # This is expected. Nothing to do.
      when Numric
        levels = [levels]
      else
        raise ArgumentError, "invalid level specification (#{levels})"
      end

      nlev = levels.length

      # < index >
      case index
      when Array
        raise ArgumentError, "index is an empty array" if index.length == 0
        while (index.length < nlev )
          index += index
        end
      when Numeric
        index = [index]*nlev
      when nil
        index = [DCL.udpget('indxmn')]*nlev
      else
        raise ArgumentError, "unsupported index type (#{index.class})"
      end

      # < line_type >
      case line_type
      when Array
        raise ArgumentError, "line_type is an empty array" if line_type.length == 0
        while (line_type.length < nlev )
          line_type += line_type
        end
      when Numeric
        line_type = [line_type]*nlev
      when nil
        line_type = [1]*nlev
      else
        raise ArgumentError, "unsupported index type (#{index.class})"
      end

      # < label >
      case label
      when Array
        raise ArgumentError, "label is an empty array" if label.length == 0
        while (label.length < nlev )
          label += label
        end
      when String
        label = [label]*nlev
      when false
        label = [""]*nlev
      when true
        label = (0...nlev).collect{|i| 
            DCL.udlabl(levels[i])
        }
      when nil
        indxmj = DCL.udpget('indxmj')
        label = (0...nlev).collect{|i| 
          if index[i]==indxmj
            DCL.udlabl(levels[i])
          else
            ""
          end
        }
      else
        raise ArgumentError, "unsupported index type (#{index.class})"
      end

      # < label_height >
      case label_height
      when Array
        raise ArgumentError, "label_height is an empty array" if label_height.length == 0
        while (label_height.length < nlev )
          label_height += label_height
        end
      when Numeric
        label_height = [label_height]*nlev
      when nil
        label_height = label.collect{|lv| lv=="" ? 0.0 : DCL.udpget('rsizel')}
      else
        raise ArgumentError, "unsupported index type (#{index.class})"
      end

      # < set levels >

      for i in 0...nlev
        DCL.udsclv(levels[i],index[i],line_type[i],label[i],label_height[i])
      end
      nil
    end

    #<<< uepack >>>

    def ue_set_linear_levs(v, options=nil)
      #  'min'       nil      minimum tone level (Numeric)
      #  'max'       nil      maximum tone level (Numeric)
      #  'nlev'      nil      number of levels (Integer)
      #  'interval'  nil      tone-level interval (Numeric)
      options = @@empty_hash if !options
      raise TypeError, "options must be a Hash" if !options.is_a?(Hash)
      min = options['min']
      max = options['max']
      nlev = options['nlev']
      interval = options['interval']
      if interval
        dx = interval
      elsif nlev
        dx = -nlev
      else
        dx = 0
      end
      if min || max
        min = v.min if !min
        max = v.max if !max
        DCL.uegtla(min, max, dx)
      else
        DCL.uegtlb(v, dx)
      end
    end

    def ue_set_tone(levels, patterns)
      DCL.ueitlv
      ue_add_tone(levels, patterns)
    end

    def ue_add_tone(levels, patterns)

      # < check types >

      if !levels.is_a?(Array) && !levels.is_a?(NArray)
        raise TypeError, "levels: Array or NArray expected (#{levels.inspect})"
      end
      if !patterns.is_a?(Array) && !patterns.is_a?(NArray)
        raise TypeError, "patterns: Array or NArray expected (#{patterns.inspect})"
      end

      # < set levels >

      nlev = levels.length
      npat = patterns.length

      case (nlev - npat)
      when 1
        for i in 0...nlev-1
          DCL.uestlv(levels[i],levels[i+1],patterns[i])
        end
      when 0
        for i in 0...nlev-1
          DCL.uestlv(levels[i],levels[i+1],patterns[i])
        end
        DCL.uestlv(levels[-1],DCL.glpget('rmiss'),patterns[-1])
      when -1
        DCL.uestlv(DCL.glpget('rmiss'),levels[0],patterns[0])
        for i in 1...nlev
          DCL.uestlv(levels[i-1],levels[i],patterns[i])
        end
        DCL.uestlv(levels[-1],DCL.glpget('rmiss'),patterns[-1])
      else
        raise ArgumentError, 
          "lengths of levels(#{nlev}) and patterns(#{npat}) are inconsistent"
      end
      nil
    end

    ############################################################
    # RELATIVELY INDEPENDENT OF DCL SUBLIBRARIES
    ############################################################

    # < flow vector package >

    def __truncate(float, order=2)
      # truncate (round) a floating number with the number digits
      # specified by "order".
      # e.g., if order=3, -0.012345 => -0.0123;  6.6666 => 6.67
      exponent = 10**(-Math::log10(float.abs).floor+order-1)
      (float * exponent).round.to_f/exponent
    end

    @@unit_vect_options = Misc::KeywordOptAutoHelp.new(
      ['vxunit', 0.05, "x unit vect len in V coord. Used only when fxunit is omitted (default)"],
      ['vyunit', 0.05, "y unit vect len in V coord. Used only when fyunit is omitted (default)"],
      ['vxuloc', nil, "Starting x position of unit vect"],
      ['vyuloc', nil, "Starting y position of unit vect"],
      ['vxuoff', 0.05, "Specify vxuloc by offset from right-bottom corner"],
      ['vyuoff', 0.0, "Specify vyuloc by offset from right-bottom corner"],
      ['inplace',true, "Whether to print labels right by the unit vector (true) or below the x axis (false)"],
      ['rsizet', nil, "Label size(default taken from uz-parameter 'rsizel1')"],
      ['index',  3," Line index of the unit vector"]
    )

    def set_unit_vect_options(options)
      @@unit_vect_options.set(options)
    end

    @@next_unit_vect_options = nil
    def next_unit_vect_options(options)
      if options.is_a?(Hash)
        @@next_unit_vect_options = options
      else
        raise TypeError,"Hash expected"
      end
      nil
    end

    def unit_vect( vxfxratio,   # (V cood length)/(actual length) in x
		   vyfyratio,   # (V cood length)/(actual length) in y
		   fxunit=nil,  # If specified, x unit vect len
		   fyunit=nil,  # If specified, y unit vect len
		   options=nil )
      #< options >
      if @@next_unit_vect_options
        options = ( options ? @@next_unit_vect_options.update(options) : 
                              @@next_unit_vect_options )
        @@next_unit_vect_options = nil
      end
      opt = @@unit_vect_options.interpret(options)
      vxunit = opt['vxunit']
      vyunit = opt['vyunit']
      vxuloc = opt['vxuloc']
      vyuloc = opt['vyuloc']
      rsizet = opt['rsizet']
      index = opt['index']
      
      #< unit vector >
      if fxunit
	vxunit = vxfxratio * fxunit
      else
	fxunit = vxunit / vxfxratio
      end
      if fyunit
	vyunit = vyfyratio * fyunit
      else
	fyunit = vyunit / vyfyratio
      end
      fxunit = __truncate( (uxusv=fxunit) )
      fyunit = __truncate( (uyusv=fyunit) )
      vxunit = vxunit * (fxunit/uxusv)
      vyunit = vyunit * (fyunit/uyusv)
      if !(vxuloc && vyuloc)
	vx0,vx1,vy0,vy1 = DCL.sgqvpt
	vxuloc = vx1 + opt['vxuoff'] if !vxuloc
	vyuloc = vy0 + opt['vyuoff'] if !vyuloc
      end
      DCL.sglazv( vxuloc, vyuloc,  vxuloc+vxunit, vyuloc,        1, index )
      DCL.sglazv( vxuloc, vyuloc,  vxuloc,        vyuloc+vyunit, 1, index )

      #< labelling >
      sfxunit = sprintf("%.2g",fxunit)
      sfyunit = sprintf("%.2g",fyunit)
      rsizet = DCL.uzpget('rsizel1') if !rsizet 
      if opt['inplace']
	DCL.sgtxzv(vxuloc, vyuloc-1.2*rsizet,
		   sfxunit, rsizet, 0, -1, index)
	DCL.sgtxzv(vxuloc+1.2*rsizet, vyuloc+0.5*rsizet,
		   sfyunit, rsizet, 90, -1, index)
      else
	msg= "UNIT VECTOR X:#{sfxunit} Y:#{sfyunit}"
	before = uz_set_params({'rsizec1'=>rsizet})
	DCL.uxsttl('b',' ',0.0)
	DCL.uxsttl('b',msg,0.0)
	uz_set_params(before)
      end
    end

    def flow_vect( fx, fy, factor=1.0, xintv=1, yintv=1)
      raise ArgumentError,"Expect 2D arrays" if fx.rank != 2 || fy.rank != 2
      raise ArgumentError,"fx.shape != fy.shape" if fx.shape != fy.shape
      raise ArgumentError,"xintv must be a positive integer" if xintv < 0
      raise ArgumentError,"yintv must be a positive integer" if yintv < 0
      nx, ny = fx.shape
      if xintv >= 2
	idx = NArray.int(nx/xintv).indgen!*xintv  # [0,xintv,2*xintv,..]
	fx = fx[idx, true]
	fy = fy[idx, true]
      end
      if yintv >= 2
	idx = NArray.int(ny/yintv).indgen!*yintv  # [0,yintv,2*yintv,..]
	fx = fx[true, idx]
	fy = fy[true, idx]
      end
      nx, ny = fx.shape  # again, because of xintv & yintv 
      vx0,vx1,vy0,vy1 = DCL.sgqvpt
      ux0,ux1,uy0,uy1 = DCL.sgqwnd
      dvx = (vx1-vx0)/nx
      dvy = (vy1-vy0)/ny
      ax = (vx1-vx0)/(ux1-ux0)   # factor to convert from U to V coordinate
      ay = (vy1-vy0)/(uy1-uy0)   # factor to convert from U to V coordinate
      fxmx = fx.abs.max
      fymx = fy.abs.max
      raise "fx has no data or all zero" if fxmx == 0
      raise "fy has no data or all zero" if fymx == 0
      cn = [ dvx/(ax*fxmx),  dvy/(ay*fymx) ].min  # normarization constant
      vxfxratio = factor*cn*ax
      vyfyratio = factor*cn*ay
      before = ug_set_params( {'LNRMAL'=>false, 'LMSG'=>false,
			       'XFACT1'=>1.0, 'YFACT1'=>1.0} )
      DCL.ugvect( vxfxratio*fx, vyfyratio*fy )
      ug_set_params( before )
      unit_vect_info = [ vxfxratio, vyfyratio, fxmx, fymx ]
      return unit_vect_info
    end
  end


  module GGraph

    module_function

    def title(string)
      DCL.uxmttl('t',string,0.0) if string
      nil
    end

    def annotate(str_ary)
      charsize = 0.7 * DCL.uzpget('rsizec1')
      dvx = 0.01
      dvy = charsize*1.5
      raise TypeError,"Array expected" if ! str_ary.is_a?(Array)
      vxmin,vxmax,vymin,vymax = DCL.sgqvpt
      vx = vxmax + dvx
      vy = vymax - charsize/2
      str_ary.each{|str|
        DCL::sgtxzv(vx,vy,str,charsize,0,-1,1)
        vy -= dvy
      }
      nil
    end

    @@fig = Misc::KeywordOptAutoHelp.new(
      ['new_frame',true,      'whether to define a new frame by DCL.grfrm (otherwise, DCL.grfig is called)'],
      ['window',  nil,               '[uxmin, uxmax, uymin, uymax]'],
      ['viewport',[0.2,0.8,0.2,0.8], '[vxmin, vxmax, vymin, vymax]'],
      ['itr',     1,     'coordinate transformation number'],
      ['xreverse','positive:down,units:hPa', 'Assign max value to UXMIN and min value to UXMAX if condition is satisfied (nil:never, true:always, String: when an attibute has the value specified ("key:value,key:value,..")'],
      ['yreverse','positive:down,units:hPa', 'Assign max value to UYMIN and min value to UYMAX if condition is satisfied (nil:never, true:always, String: when an attibute has the value specified ("key:value,key:value,..")']
    )
    def set_fig(options)
      @@fig.set(options)
    end

    @@next_fig = nil
    def next_fig(options)
      if options.is_a?(Hash)
        @@next_fig = options
      else
        raise TypeError,"Hash expected"
      end
      nil
    end

    def fig(xax, yax, options=nil)
      if @@next_fig
        options = ( options ? @@next_fig.update(options) : @@next_fig )
        @@next_fig = nil
      end
      opts = @@fig.interpret(options)
      if (xreverse=opts['xreverse']).is_a?(String)
        atts = opts['xreverse'].split(',').collect{|v| v.split(':')}
        xreverse = false
        atts.each{|key,val| 
          xreverse = ( xax.get_att(key) == val )
          break if xreverse
        }
      end
      if (yreverse=opts['yreverse']).is_a?(String)
        atts = opts['yreverse'].split(',').collect{|v| v.split(':')}
        yreverse = false
        atts.each{|key,val| 
          yreverse = ( yax.get_att(key) == val )
          break if yreverse
        }
      end
      if !opts['window']
        if xreverse
          xrange = [ xax.max, xax.min ]
        else
          xrange = [ xax.min, xax.max ]
        end
        if yreverse
          yrange = [ yax.max, yax.min ]
        else
          yrange = [ yax.min, yax.max ]
        end
        window=[xrange[0], xrange[1], yrange[0], yrange[1]]
      else
        window=opts['window']
      end
      if opts['new_frame']
        DCL.grfrm
      else
        DCL.grfig
      end
      DCL.grsvpt(*opts['viewport'])
      DCL.grswnd(*window)
      DCL.grstrn(opts['itr'])
      DCL.grstrf
      nil
    end

    @@axes = Misc::KeywordOptAutoHelp.new(
      ['xside',  'tb',  'Where to draw xaxes (combination of t, b and u)'],
      ['yside',  'lr',  'Where to draw yaxes (combination of l, r and u)'],
      ['xtitle',  nil,  'Title of x axis (if nil, internally determined)'],
      ['ytitle',  nil,  'Title of y axis (if nil, internally determined)'],
      ['xunits',  nil,  'Units of x axis (if nil, internally determined)'],
      ['yunits',  nil,  'Units of y axis (if nil, internally determined)'],
      ['xtickint',  nil,
            'Interval of x axis tickmark (if nil, internally determined)'],
      ['ytickint',  nil,
            'Interval of y axis tickmark (if nil, internally determined)'],
      ['xlabelint',  nil,
               'Interval of x axis label (if nil, internally determined)'],
      ['ylabelint',  nil,
               'Interval of y axis label (if nil, internally determined)']
    )
    def set_axes(options)
      @@axes.set(options)
    end

    @@next_axes = nil
    def next_axes(options)
      if options.is_a?(Hash)
        @@next_axes = options
      else
        raise TypeError,"Hash expected"
      end
      nil
    end

    def axes(xax=nil, yax=nil, options=nil)
      if @@next_axes
        options = ( options ? @@next_axes.update(options) : @@next_axes )
        @@next_axes = nil
      end
      opts = @@axes.interpret(options)
      if opts['xside'].length > 0
        xai = _get_axinfo(xax)
        DCL.uscset('cxttl', (opts['xtitle'] || xai['title']) )
        DCL.uscset('cxunit',(opts['xunits'] || xai['units']) )
        DCL.uspset('dxt', opts['xtickint'])  if(opts['xtickint'])
        DCL.uspset('dxl', opts['xlabelint']) if(opts['xlabelint'])
        opts['xside'].split('').each{|s|     # scan('.') also works
	  DCL.usxaxs(s)
	}
      end
      if opts['yside'].length > 0
        yai = _get_axinfo(yax)
        DCL.uscset('cyttl', (opts['ytitle'] || yai['title']) )
        DCL.uscset('cyunit', (un=(opts['yunits'] || yai['units'])) )
        DCL.uspset('dxt', opts['ytickint'])  if(opts['ytickint'])
        DCL.uspset('dxl', opts['ylabelint']) if(opts['ylabelint'])
        opts['yside'].split('').each{|s|   # scan('.') also works
	  DCL.usyaxs(s)
	  if s=='l' && un && un != ''
	    DCL.uzpset('roffxt', DCL.uzpget('rsizec1')*1.5 )
	  end
	}
      end
      nil
    end

    def _get_axinfo(vary)
      if vary.nil?
        hash = {
                'title'=>'',
                'units'=>''
               }
      else
        raise "Not 1D" if vary.rank!=1
        hash = {
                'title'=>(vary.get_att('long_name') || vary.name), #.gsub('_','' )
                'units'=>(vary.get_att('units') || '')             #.gsub('_',' ')
               }
      end
      hash 
    end

    def line(gphys, newframe=true, options=nil)
      if newframe!=true && newframe!=false
        raise ArgumentError, "2nd arg (newframe) must be true or false"
      end
      if ! defined?(@@line_options)
        @@line_options = Misc::KeywordOptAutoHelp.new(
          ['title', nil, 'Title of the figure(if nil, internally determined)'],
          ['annotate', true, 'if false, do not put texts on the right margin even when newframe==true'],
          ['exchange', false, 'whether to exchange x and y axes'],
          ['index', 1, 'line/mark index'],
          ['type', 1, 'line type'],
          ['label', nil, 'if a String is given, it is shown as the label']
        )
      end
      opts = @@line_options.interpret(options)
      gp = gphys.first1D
      if !opts['exchange']
        x = gp.coord(0)
        y = gp.data
      else
        y = gp.coord(0)
        x = gp.data
      end
      if newframe
        fig(x, y)
        axes(x, y)
        title( (opts['title'] || gp.data.get_att('long_name')) )
        annotate(gp.lost_axes) if opts['annotate']
      end
      if opts['label']
	lcharbk = DCL.sgpget('lchar')
	DCL.sgpset('lchar',true)
	DCL.sgsplc(opts['label'])
      end
      DCL.uulinz(x.val, y.val, opts['type'], opts['index'])
      DCL.sgpset('lchar',lcharbk) if opts['label']
      nil
    end

    def mark(gphys, newframe=true, options=nil)
      if newframe!=true && newframe!=false
        raise ArgumentError, "2nd arg (newframe) must be true or false"
      end
      if ! defined?(@@mark_options)
        @@mark_options = Misc::KeywordOptAutoHelp.new(
          ['title', nil, 'Title of the figure(if nil, internally determined)'],
          ['annotate', true, 'if false, do not put texts on the right margin even when newframe==true'],
          ['exchange', false, 'whether to exchange x and y axes'],
          ['index', 1, 'mark index'],
          ['type', 2, 'mark type'],
          ['size', 0.01, 'marks size']
        )
      end
      opts = @@mark_options.interpret(options)
      gp = gphys.first1D
      if !opts['exchange']
        x = gp.coord(0)
        y = gp.data
      else
        y = gp.coord(0)
        x = gp.data
      end
      if newframe
        fig(x, y, opts['fig_params'])
        axes(x, y)
        title( (opts['title'] || gp.data.get('long_name')) )
        annotate(gp.lost_axes) if opts['annotate']
      end
      DCL.uumrkz(x.val, y.val, opts['type'], opts['index'], opts['size'])
      nil
    end

    @@linear_contour_options =  Misc::KeywordOptAutoHelp.new(
      ['min',      nil,       'minimum contour value'],
      ['max',      nil,       'maximum contour value'],
      ['nlev',     nil,       'number of levels'],
      ['interval', nil,       'contour interval'],
      ['nozero',   nil,       'delete zero contour'],
      ['coloring', false,     'set color contours with ud_coloring'],
      ['clr_min',  13,        '(if coloring) minimum color id'],
      ['clr_max',  100,       '(if coloring) maximum color id']
    )
    def set_linear_contour_options(options)
      @@linear_contour_options.set(options)
    end
    @@next_linear_contour_options = nil
    def next_linear_contour_options(options)
      if options.is_a?(Hash)
        @@next_linear_contour_options = options
      else
        raise TypeError,"Hash expected"
      end
      nil
    end

    @@contour_levels =  Misc::KeywordOptAutoHelp.new(
      ['levels',   nil,   'contour levels (Array/NArray of Numeric)'],
      ['index',    nil,   '(if levels) line index(es) (Array/NArray of integers, Integer, or nil)'],
      ['line_type',nil,   '(if levels) line type(s) (Array/NArray of integers, Integer, or nil)'],
      ['label',    nil,   '(if levels) contour label(s) (Array/NArray of String, String, true, false, nil). nil is recommended.'],
      ['label_height',nil,'(if levels) label height(s) (Array/NArray of Numeric, Numeric, or nil). nil is recommended.']
    )
    @@set_contour_levels=false
    def set_contour_levels(options)
      opts = @@contour_levels.interpret(options)
      _set_contour_levels_(opts)
    end
    def _set_contour_levels_(opts)
      raise ArgumentError, "'levels' must be explicitly set" if !opts['levels']
      DCLExt.ud_set_contour(opts['levels'],opts['index'],opts['line_type'],
                  opts['label'],opts['label_height'])
      @@set_contour_levels=true
      nil
    end

    def clear_contour_levels
      @@set_contour_levels=false
      DCL.udiclv   # this may not be needed
      nil
    end

    def contour(gphys, newframe=true, options=nil)
      if newframe!=true && newframe!=false
        raise ArgumentError, "2nd arg (newframe) must be true or false"
      end
      if ! defined?(@@contour_options)
        @@contour_options = Misc::KeywordOptAutoHelp.new(
          ['title', nil, 'Title of the figure(if nil, internally determined)'],
          ['annotate', true, 'if false, do not put texts on the right margin even when newframe==true'],
          ['transpose', false, 'if true, exchange x and y axes'],
          @@linear_contour_options,
          @@contour_levels
        )
      end
      if @@next_linear_contour_options
        options = ( options ? @@next_linear_contour_options.update(options) : 
                              @@next_linear_contour_options )
        @@next_linear_contour_options = nil
      end
      opts = @@contour_options.interpret(options)
      gp = gphys.first2D
      gp = gp.transpose(1,0) if opts['transpose']
      xax = gp.coord(0)
      yax = gp.coord(1)
      if newframe
        fig(xax, yax)
        axes(xax, yax)
        title( (opts['title'] || gp.data.get_att('long_name')) )
        annotate(gp.lost_axes) if opts['annotate']
      end
      if opts['levels']
        backup = @@set_contour_levels
        _set_contour_levels_(opts)
        @@set_contour_levels = backup
      elsif !@@set_contour_levels
        DCLExt.ud_set_linear_levs(gp.data.val, opts)
      end
      DCL.uwsgxa(xax.val)
      DCL.uwsgya(yax.val)
      DCL.udcntz(gp.data.val)
      nil
    end

    @@linear_tone_options =  Misc::KeywordOptAutoHelp.new(
      ['min',      nil,       'minimum contour value'],
      ['max',      nil,       'maximum contour value'],
      ['nlev',     nil,       'number of levels'],
      ['interval', nil,       'contour interval']
    )
    def set_linear_tone_options(options)
      @@linear_tone_options.set(options)
    end
    @@next_linear_tone_options = nil
    def next_linear_tone_options(options)
      if options.is_a?(Hash)
        @@next_linear_tone_options = options
      else
        raise TypeError,"Hash expected"
      end
      nil
    end

    @@tone_levels =  Misc::KeywordOptAutoHelp.new(
      ['levels',   nil,   'tone levels  (Array/NArray of Numeric). Works together with patterns'],
      ['patterns', nil,   'tone patters (Array/NArray of Numeric). Works together with levels']
    )
    @@set_tone_levels=false
    def set_tone_levels(options)
      opts = @@tone_levels.interpret(options)
      _set_tone_levels_(opts)
    end
    def _set_tone_levels_(opts)
      if !opts['levels'] && !opts['patterns']
      end
      if opts['levels']
	levels = opts['levels']
      else
	raise ArgumentError,"'levels' must be explicitly set"
      end
      if opts['patterns']
	patterns = opts['patterns']
      else
	nlev = levels.length - 1
	itpat = DCL.ueiget('itpat')
	iclr1 = DCL.ueiget('icolor1')
	iclr2 = DCL.ueiget('icolor2')
	patterns = (0...nlev).collect{|i|
	  (((iclr2-iclr1)/(nlev-1.0)*i+iclr1).round) * 1000 + itpat
	}
      end
      DCLExt.ue_set_tone(levels,patterns)
      @@set_tone_levels=true
      nil
    end
    def clear_tone_levels
      @@set_tone_levels=false
      nil
    end

    def tone(gphys, newframe=true, options=nil)
      if newframe!=true && newframe!=false
        raise ArgumentError, "2nd arg (newframe) must be true or false"
      end
      if ! defined?(@@tone_options)
        @@tone_options = Misc::KeywordOptAutoHelp.new(
          ['title', nil, 'Title of the figure(if nil, internally determined)'],
          ['annotate', true, 'if false, do not put texts on the right margin even when newframe==true'],
          ['ltone', true, 'Same as udpack parameter ltone'],
          ['tonf', true, 'Use DCL.uetonf instead of DCL.uetone'],
          ['tonc', true, 'Use DCL.uetonc instead of DCL.uetone'],
          ['clr_min', nil,  'if an integer (in 10..99) is specified, used as the mimimum color number. (the same can be done by setting the uepack parameter "icolor1")'],
          ['clr_max', nil,  'if an integer (in 10..99) is specified, used as the maximum color number. (the same can be done by setting the uepack parameter "icolor2")'],
          ['transpose', false, 'if true, exchange x and y axes'],
          @@linear_tone_options,
          @@tone_levels
        )
      end
      opts = @@tone_options.interpret(options)

      if opts['clr_min']
	icolor1_bak = DCL.uepget('icolor1')
	DCL.uepset('icolor1', opts['clr_min'])
      end
      if opts['clr_max']
	icolor2_bak = DCL.uepget('icolor2')
	DCL.uepset('icolor2', opts['clr_max'])
      end
      gp = gphys.first2D
      gp = gp.transpose(1,0) if opts['transpose']
      xax = gp.coord(0)
      yax = gp.coord(1)
      if newframe
        fig(xax, yax)
      end
      DCL.uwsgxa(xax.val)
      DCL.uwsgya(yax.val)
      if opts['levels'] 
        backup = @@set_tone_levels
        _set_tone_levels_(opts)
        @@set_tone_levels = backup
	if opts['tonf']
	  DCL.uetonf(gp.data.val)
	elsif opts['tonc']
	  DCL.uetonc(gp.data.val)
	else
	  DCL.uetone(gp.data.val)
	end
      elsif opts['patterns']
	raise "option 'patterns' is not effective unless 'levels' is specified"
      elsif !@@set_tone_levels && opts['ltone']
        DCLExt.ue_set_linear_levs(gp.data.val, opts)
	if opts['tonf']
	  DCL.uetonf(gp.data.val)
	elsif opts['tonc']
	  DCL.uetonc(gp.data.val)
	else
	  DCL.uetone(gp.data.val)
	end
        DCL.ueitlv
      else
	if opts['tonf']
	  DCL.uetonf(gp.data.val)
	elsif opts['tonc']
	  DCL.uetonc(gp.data.val)
	else
	  DCL.uetone(gp.data.val)
	end
      end
      if newframe
        axes(xax, yax)  # axes should be drawn after uetone
        title( (opts['title'] || gp.data.get_att('long_name')) )
        annotate(gp.lost_axes) if opts['annotate']
      end
      DCL.uepset('icolor1', icolor1_bak) if opts['clr_min']
      DCL.uepset('icolor2', icolor2_bak) if opts['clr_max']
      nil
    end

    # < vector >

    def set_unit_vect_options(options)
      DCLExt.set_unit_vect_options(options)
    end
    def next_unit_vect_options(options)
      DCLExt.next_unit_vect_options(options)
    end

    def vector(fx, fy, newframe=true, options=nil)
      if newframe!=true && newframe!=false
        raise ArgumentError, "2nd arg (newframe) must be true or false"
      end
      if ! defined?(@@vector_options)
        @@vector_options = Misc::KeywordOptAutoHelp.new(
          ['title', nil, 'Title of the figure(if nil, internally determined)'],
          ['annotate', true, 'if false, do not put texts on the right margin even when newframe==true'],
          ['transpose', false, 'if true, exchange x and y axes'],
          ['flow_vect', true, 'If true, use DCLExt::flow_vect to draw vectors; otherwise, DCL::ugvect is used.'],
          ['xintv', 1, '(Effective only if flow_vect) interval of data sampling in x'],
          ['yintv', 1, '(Effective only if flow_vect) interval of data sampling in y'],
          ['factor', 1.0, '(Effective only if flow_vect) scaling factor to strech/reduce the arrow lengths'],
          ['unit_vect', false, 'Show the unit vector'],
          ['max_unit_vect', false, '(Effective only if flow_vect && unit_vect) If true, use the maximum arrows to scale the unit vector; otherwise, normalize in V coordinate.']
        )
      end
      opts = @@vector_options.interpret(options)
      fx = fx.first2D.copy
      fy = fy.first2D.copy
      sh = fx.shape
      if sh != fy.shape
	raise ArgumentError, "shapes of fx and fy do not agree with each other"
      end
      fx = fx.transpose(1,0) if opts['transpose']
      fy = fy.transpose(1,0) if opts['transpose']
      if ((xi=opts['xintv']) >= 2)
	idx = NArray.int(sh[0]/xi).indgen!*xi     # [0,xi,2*xi,..]
	fx = fx[idx, true]
	fy = fy[idx, true]
      end
      if ((yi=opts['xintv']) >= 2)
	idx = NArray.int(sh[1]/yi).indgen!*yi     # [0,yi,2*yi,..]
	fx = fx[true, idx]
	fy = fy[true, idx]
      end
      xax = fx.coord(0)
      yax = fy.coord(1)
      if newframe
        fig(xax, yax)
        axes(xax, yax)
	if opts['title']
	  ttl = opts['title']
	else
	  fxnm = fx.data.get_att('long_name') || fx.name
	  fynm = fy.data.get_att('long_name') || fy.name
	  ttl =   '('+fxnm+','+fynm+')'
	end
        title( ttl )
        annotate(fx.lost_axes) if opts['annotate']
      end
      DCL.uwsgxa(xax.val)
      DCL.uwsgya(yax.val)
      if opts['flow_vect']
	uninfo = DCLExt.flow_vect(fx.val, fy.val, opts['factor'] )
	if opts['unit_vect']
	  if opts['max_unit_vect']
	    DCLExt.unit_vect(*uninfo)
	  else
	    DCLExt.unit_vect(*uninfo[0..1])
	  end
	end
      else
	before=DCLExt.ug_set_params({'lunit'=>true}) if opts['unit_vect']
	DCL.ugvect(fx.val, fy.val)
	DCLExt.ug_set_params(before) if opts['unit_vect']
      end
      nil
    end


  end

end

if $0 == __FILE__
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
  file = '../../testdata/T.jan.nc'
  gphys = GPhys::NetCDF_IO.open(file, 'T')
  DCL.gropn(iws)
  DCL.sldiv('y',2,2)
  DCL.sgpset('lcntl', false)
  DCL.sgpset('lfull',true)
  DCL.sgpset('lfprop',true)
  DCL.uzfact(0.9)
  #/ graph 1 /
  GGraph.set_fig('viewport'=>[0.25,0.75,0.12,0.62])
  GGraph.line(gphys.cut(true,35,true).average(0), true)
  #/ graph 2 /
  GGraph.next_fig('itr'=>2)
  GGraph.next_axes('yunits'=>'','xunits'=>'')
  GGraph.line(gphys.cut(true,35,true).average(0), true, 
              'exchange'=>true, 'index'=>3, 'title'=>'TEMERATURE', 'annotate'=>false)
  GGraph.mark(gphys.cut(true,35,true).average(0), false, 
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
  GGraph.set_contour_levels('levels'=>[-10,-5,0,5,10],'index'=>[3,1],
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
  #/ graph 17 /
  GGraph.set_fig('viewport'=>[0.3,0.7,0.1,0.6])
  GGraph.contour(gphys,true, 'min'=>0, 'coloring'=>true, 'transpose'=>true)
  GGraph.tone(gphys, false, 'levels'=>[10,20], 'patterns'=>[80999], 'transpose'=>true)
  #/ graph 19 /
  GGraph.set_fig('viewport'=>[0.25,0.75,0.15,0.6])
  GGraph.vector(gphys+10,gphys+10,true,
	{'unit_vect'=>true, 'xintv'=>2, 'yintv'=>2})
  #/ graph 18 /
  GGraph.next_unit_vect_options({'inplace'=>false,'vxuloc'=>0.17,'vyuloc'=>0.07})
  GGraph.vector(gphys+10,gphys-10,true,
	{'unit_vect'=>true, 'max_unit_vect'=>true, 'xintv'=>2, 'yintv'=>2})
  #/ graph 19 /
  GGraph.vector(gphys+10,gphys+10,true,
	{'unit_vect'=>true, 'flow_vect'=>false, 'xintv'=>2, 'yintv'=>2})

  DCL.grcls

end
