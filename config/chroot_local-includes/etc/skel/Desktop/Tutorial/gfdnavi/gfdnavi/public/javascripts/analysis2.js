/* global variables */
var diagrams = new Array();
var num_diagrams = 0;
var selectedDiagram = 0;

var DimsForOperation = Class.create();
DimsForOperation.prototype = {
    initialize: function(div, axes, update){
        this.div = div;
        this.axes = axes;
        this.update = update;
        var html = '<span style="white-space:nowrap">Dims for operation: ';
        for(var i = 0; i < axes.length; i++){
            html += '<input id="' + div.id + '_' + i + '" type="checkbox"' + ((i==0) ? 'checked="checked"' : "") + '>';
            html += '<label for="' + div.id + '_' + i + '">' + axes[i]["name"] + '</label>';
        }
        html += '</span>';
        div.innerHTML = html;
        this.dimCheck = [];
        for(var i = 0; i < axes.length; i++){
            this.dimCheck[i] = $(div.id + '_' + i);
            this.dimCheck[i].observe("change", this.valueChanged.bind(this)); /* event handler */
        }
    },
    getDims: function(){
        var ary = [];
        for(var i = 0; i < this.axes.length; i++)
            if(this.dimCheck[i].checked) ary.push(this.axes[i]["name"]);
        return ary;
    },
    setDims: function(ary){
        if(!(ary.each)) ary = ary.split(","); /* String to Array */
        this.dimCheck.each(function(dc){dc.checked = false;});
        for(var j = 0; j < ary.length; j++)
            for(var i = 0; i < this.axes.length; i++)
                if(this.axes[i]["name"] == ary[j])
                    this.dimCheck[i].checked = true;
        this.checkValue(this.div.id + '_0'); /* dim0 is given as a sample */
    },
    checkValue: function(id){
        var count = 0;
        this.dimCheck.each(function(dc){if(dc.checked) count++;}, this);

        var num = id.match(/_(\d+)$/)[1]; 
        if(count == 0) /* at least one dimension should be selected */
            this.dimCheck[(num + 1) % this.axes.length].checked = true;
        if(count == this.axes.length) /* all axes cannot be selected at once */
            this.dimCheck[(num + 1) % this.axes.length].checked = false;
    },
    valueChanged: function(e){
        this.checkValue(e.target.id);
        this.update();
    }
};

var FuncData = Class.create();
FuncData.prototype = {
    initialize: function(divDOM, obj, topax, updater){
        this.div    = divDOM; /* div to insert the object */
        this.update = updater;
        this.index  = [0];

        var select_id = this.div.id + '_select';
        var div_vars_id = this.div.id + '_vars';
        var html = '<div id="' + div_vars_id + '"></div>'
            + '<select id="' + select_id + '" class="select_input">'
            + analFuncs.map(
                function(fn){
                    return '<option value="' + fn.name + (fn.user ? "," + fn.user : "") + '" title="' + (fn.description || fn.name) + '">'
                        + fn.name
                        + '</option>';
                }).join()
            + '</select>'
            + '<div id="' + this.div.id + '_arg_div"></div>';

        this.div.innerHTML = html; /* update HTML */
        this.select = $(select_id);
        this.div_vars = $(div_vars_id);

        /* create HTML for input data */
        this.vars = new InputArray(this.div_vars, this.inputChanged.bind(this), false, true);

        this.select.observe("change", this.funcChanged.bind(this)); /* event handler */
        this.setFunc(obj, topax);
    },
    setFunc: function(fn, topax){
        /* fn:    JSON object of parameters
         * topax: TreeOfPathAndAxes */
        if(fn.functions && fn.functions.func)
            this.select.value = fn.functions.func;
        if(fn.functions && fn.functions.index)
            this.index = fn.functions.index; /* an Array */

        this.setNumOfVar(fn, topax);
        this.createDimsForOperation();

        /* set variables, functions, and cut */

        if(fn.functions && fn.functions.args && this.argEnabled){
            if(this.argDims){
                this.dimsForOp = new DimsForOperation($(this.div.id + '_arg_dim'), this.vars.input(0).axes(),
                                                      this.argChanged.bind(this));
                this.dimsForOp.setDims(fn.functions.args[0]);
                /* store args */
                this.currentArgDims = this.dimsForOp.getDims();
            }
            else
                $(this.div.id + '_arg').value = fn.functions.args[0].join(',');
        }
    },
    setNumOfVar: function(obj, topax){
        this.nvars = analFuncs[this.select.selectedIndex].nvars;
        this.vars.setNumOfInputs(this.nvars, null, obj, topax, false);
    },
    createDimsForOperation: function(){
        var html = "";
        var fnid = this.select.selectedIndex;
        var oldArgEnabled = this.argEnabled;
        var oldArgDims = this.argDims;

        this.argEnabled = ((analFuncs[fnid].args[0]) ? true : false);
        if(this.argEnabled)
            this.argDims = (analFuncs[fnid].args[0].description.match("dim")) && (analFuncs[fnid].args[0].type == "array_string");
        if((this.argEnabled != oldArgEnabled) || (this.argEnabled && (this.argDims != oldArgDims))){
            /* change input object for arguments */
            if(this.argEnabled){
                /* num of args is assumed to be one for all the functions */
                if(this.argDims)
                    html = '<div id="' + this.div.id + '_arg_dim"></div>';
                else{
                    html = '<label title="' + analFuncs[fnid].args[0].name + '">Arguments: </label>' 
                        + '<input id="' + this.div.id + '_arg" type="text" value="' + analFuncs[fnid].args[0]["default"] + '">';
                }
            }
            $(this.div.id + '_arg_div').innerHTML = html; /* update HTML */
            if(this.argEnabled){
                if(this.argDims){
                    this.dimsForOp = new DimsForOperation($(this.div.id + '_arg_dim'), this.vars.input(0).axes(),
                                                          this.argChanged.bind(this));
                    /* restore dimension settings if any */
                    if(this.currentArgDims) this.dimsForOp.setDims(this.currentArgDims);
                }
                else
                    $(this.div.id + '_arg').observe('change', this.argChanged.bind(this));
            }
        }
    },
    funcChanged: function(e){
        this.setNumOfVar();
        this.createDimsForOperation();
        if(e) this.update();
    },
    argChanged: function(){
        /* save dimension settings if any */
        if(this.argDims) this.currentArgDims = this.dimsForOp.getDims();
        this.update();
    },
    inputChanged: function(){
        if(this.argEnabled && this.argDims)
            this.dimsForOp = new DimsForOperation($(this.div.id + '_arg_dim'), this.vars.input(0).axes(),
                                                  this.argChanged.bind(this));
        this.update();
    },
    path: function(){
        var path = this.vars.paths().join(',');
        if(this.nvars>1) path = '/[' + path + ']';
        path += '/analysis(' + this.select.value;
        if($(this.div.id + '_arg_dim')) /* add dimension options if any */
            path += ';' + this.dimsForOp.getDims().join(',');
        if($(this.div.id + '_arg')) /* add other options if any */
            path += ';' + $(this.div.id + '_arg').value.replace(/\s/g,""); /* remove white spaces */
        path += ')'
            + '[' + this.index.join(',') + ']'; /* index */
        return path;
    },
    toHash: function(){
        var ary = new Hash;
        var fn = new Hash;
        fn.set("func", this.select[this.select.selectedIndex].value);
        fn.set("index", [this.index]);
        var args = [];
        if($(this.div.id + '_arg_dim')) /* dimensions for operation */
            args.push(this.dimsForOp.getDims());
        if($(this.div.id + '_arg') && ($(this.div.id + '_arg').value != ''))
            args.push($(this.div.id + '_arg').value.split(","));
        fn.set("args", args);
        ary.set("functions", fn);
        ary.set("input", this.vars.arrayOfHash());
        return ary;
    }
};

var InputData = Class.create();
InputData.prototype = {
    initialize: function(div, idx, execute, enableAxisBox, obj, topax){
        this.div = div;               /* div to insert the object */
        this.idx = idx;               /* variable index */
        this.execute = execute;       /* call this when updated */
        this.enableAxisBox = enableAxisBox; /* enable axis box */
        var input_id = div.id + '_input';
        var axes_id = div.id + '_axes';
        var name = div.id + '_type';
        var html = '';
        html += '<div id="' + input_id + '"></div>'; /* div for child */
        if(enableAxisBox)
            html += '<input id="' + div.id + '_axes_button" type="button" value="Show axes">';
        if(enableAxisBox)
            html += '<div id="' + axes_id + '"></div>'; /* div for axes */
        html += '<input type="button" id="' + div.id + '_appfunc_button" value="Function" title="Apply function to this">';
        html += '<input type="button" id="' + div.id + '_delfunc_button" value="Delete" title="Delete this function">';

        div.innerHTML = html; /* update HTML */

        this.div_input = $(input_id);
        this.div_axes = $(axes_id);
        this.appFuncButton = $(div.id + '_appfunc_button');
        this.delFuncButton = $(div.id + '_delfunc_button');
        if(enableAxisBox)
            this.axes_button = $(div.id + '_axes_button');

        this.delFuncButton.style.display = "none";

        this.setInput(obj, topax);

        /* event handlers */
        this.appFuncButton.observe("click", this.appFuncButtonClicked.bind(this));
        this.delFuncButton.observe("click", this.delFuncButtonClicked.bind(this));
        if(enableAxisBox)
            this.axes_button.observe("click", this.axesButtonClicked.bind(this));
    },
    appFuncButtonClicked: function(e){
        var currentObj = {"input":[Object.toJSON(this.toHash()).evalJSON(true)]};
        var topax = [this.treeOfPathAndAxes()];
        this.setFunc(currentObj, topax);
        if(e && this.enableAxisBox) this.createAxisBox();
        if(e) this.update();
    },
    delFuncButtonClicked: function(e){
        this.setInput(Object.toJSON(this.func.vars.vars[0].toHash()).evalJSON(true),
                      this.func.vars.vars[0].treeOfPathAndAxes());
        if(e) this.update();
    },
    createAxisBox: function(axesWithoutCut){
        if(!(this.enableAxisBox)) return;
        this.axisBox = new Axes(this.div_axes, [axesWithoutCut || this.axesWithoutCut()], 1, (this.ndims || 0), 0, this.update.bind(this));
        this.axisBox.hide();
        this.axes_button.value = "Show axes";
    },
    setInput: function(input, topax){
        if(topax){
            this.oldPath = topax[0];
            this.oldAxes = topax[1];
            this.oldPathWithoutCut = topax[2];
            this.oldAxesWithoutCut = topax[3];
        }
        if(input && input['functions'] && input['functions']['func'])
            this.setFunc(input, topax ? topax[4] : null); /* topax[4] is for Function */
        else
            this.setVar(input);
        if(this.enableAxisBox){
            this.createAxisBox(topax ? topax[3] : null); /* topax[3] is a path before applying cut */
            if(input && input['functions'] && input['functions']['cut'])
                this.setCut(input['functions']['cut']);
        }
    },
    setVar: function(obj){
        /* obj: JSON of parameters */
        if(!this.select){ /* create variable selector */
            var html = '<select id="' + this.div_input.id + '_select" class="select_input">';
            variables.each(
                function(v){
                    var tmp = v.split('/');
                    html += '<option value="' + v + '" title="' + v + '">' + tmp[tmp.length - 1] + '</option>';});
            html += '</select>';
            this.div_input.innerHTML = html; /* update HTML */
            this.select = $(this.div_input.id + '_select');
            this.func = null;
            this.select.observe("change", this.varChanged.bind(this)); /* event handler */
        }
        if(obj && obj.input && obj.input[0])
            this.select.value = obj.input[0];
        this.delFuncButton.style.display = "none";
    },
    setFunc: function(obj, topax){
        /* obj: JSON of parameters 
         * topax: tree of path and axes */
        if(!this.func){ /* create function selector */
            this.select = null;
            this.func = new FuncData(this.div_input, obj, topax, this.funcUpdated.bind(this));
        }
        else 
            this.func.setFunc(obj, topax);
        this.delFuncButton.style.display = "";
    },
    setCut: function(cut){
        if(this.enableAxisBox) this.axisBox.importCut(cut);
    },
    setNumOfDims: function(ndims){
        this.ndims = ndims;
        this.createAxisBox();
    },
    axesButtonClicked: function(){
        var shown = (this.axes_button.value == "Hide axes");
        this.axes_button.value = shown ? "Show axes" : "Hide axes";
        shown ? this.axisBox.hide() : this.axisBox.show();
    },
    varChanged: function(){
        this.createAxisBox();        
        this.update();
    },
    funcUpdated: function(){
        if(this.createAxisBox && this.enableAxisBox) this.createAxisBox();
        this.update();
    },
    update: function(e){if(this.execute) (this.execute)();},
    pathWithoutCut: function(){
        return this.func ? this.func.path() : this.select.value;
    },
    path: function(){
        var path = this.pathWithoutCut();
        if(this.enableAxisBox && this.axisBox){ /* set cut */
            var ary = this.axisBox.cut.map(function(cut){
                var tmpcut = cut.key + '=>' + cut.value[0];
                if(cut.value[1] && (parseFloat(cut.value[0]) != parseFloat(cut.value[1])))
                    tmpcut += '..' + cut.value[1];
                return tmpcut;
            });
            path += '/cut(' + ary.join(',') + ')';
        }
        return path;
    },
    axes: function(){
        var path = this.path();
        if(this.oldPath == path) return this.oldAxes; /* read cache */
        var axes = this.getAxesFromServer(path);
        this.oldPath = path;
        this.oldAxes = axes; /* make cache */
        return axes;
    },
    axesWithoutCut: function(){
        var pathWithoutCut = this.pathWithoutCut();
        if(this.oldPathWithoutCut == pathWithoutCut) return this.oldAxesWithoutCut; /* read cache */
        var axesWithoutCut = this.getAxesFromServer(pathWithoutCut);
        this.oldPathWithoutCut = pathWithoutCut;
        this.oldAxesWithoutCut = axesWithoutCut; /* make cache */
        return axesWithoutCut;
    },
    getAxesFromServer: function(path){
        var a = new Ajax.Request(gfdnaviUrlRoot + "/analysis2/get_axes",
                                 {asynchronous:false,
                                  evalScripts:true,
                                  parameters: 'path=' + encodeURIComponent(path),
                                  onFailure: function(){alert('failed');},
                                  method: 'get'});
        /* Using eval directly is not safe.
         * JSON.parse may not work on old browsers.
         * String#evalJSON is defined in prototype.js.
         * option: sanitize (This must be true when remote data is given.) */
        return a.transport.responseText.evalJSON(true);        
    },
    treeOfPathAndAxes: function(){
        var exax = [this.path(), this.axes(), this.pathWithoutCut(), this.axesWithoutCut()];
        if(this.func) exax.push(this.func.vars.treeOfPathAndAxes());
        return exax;
    },
    toHash: function(){
        if(this.func){
            return this.func.toHash();
        }
        else{
            /* cut information */
            var ary = new Hash;
            if(this.enableAxisBox){
                var fn = new Hash;
                fn.set("cut", this.axisBox.toHash());
                ary.set("functions", fn);
            }
            /* input variable */
            ary.set("input", [this.select[this.select.selectedIndex].value]);
            return ary;
        }

    }
};

var InputArray = Class.create();
InputArray.prototype = {
    initialize: function(divDOM, updater, enableCancel, enableAxisBox, ndims){
        this.div = divDOM;
        this.updater = updater;
        this.enableCancel = enableCancel;
        this.enableAxisBox = enableAxisBox;
        this.ndims = ndims;

        var id_var_apply_span    = divDOM.id + '_var_apply_span';
        var id_var_apply_button  = divDOM.id + '_var_apply_button';
        var id_var_cancel_button = divDOM.id + '_var_cancel_button';
        var id_vars              = divDOM.id + '_vars';

        var html = "";
        if(enableCancel){
            html += '<span id="' + id_var_apply_span + '" style="display:none">'
                + '<input type="button" id="' + id_var_apply_button + '" value="Apply changes">'
                + '<input type="button" id="' + id_var_cancel_button + '" value="Cancel">'
                + '</span>';
        }
        html += '<div id="' + id_vars + '"></div>';
        divDOM.innerHTML = html;

        if(enableCancel){
            this.var_apply_span = $(id_var_apply_span);
            $(id_var_apply_button).observe("click", this.inputApply.bind(this));
            $(id_var_cancel_button).observe("click", this.inputCancel.bind(this));
        }
        this.div_vars = $(id_vars);
    },
    setInputArray: function(obj, topax, approved){
        /* obj: JSON of parameters
         * topax: Tree Of Path and Axes
         * approved: boolean flag if approval is required to take effect of changes */
        if(obj && obj.input && obj.input[0]){
            var input = obj.input;
            for(var i = 0; i < this.nvars; i++)
                this.vars[i].setInput(input[i] || input[i - 1], /* if missing, use previous item */
                                      topax ? (topax[i] || topax[i - 1]) : null); /* assume input.length == topax.length */
            if(approved) this.savePaths();
        }
    },
    setNumOfInputs: function(nvars, ndims, obj, topax, approved){
        if((!this.vars) || (nvars != this.vars.length) || obj){
            if(obj)
                obj = obj.input;
            else if(this.vars){
                obj = this.vars.map(
                    function(v){return Object.toJSON(v.toHash()).evalJSON(true);});
                topax = this.vars.map(
                    function(v){return v.treeOfPathAndAxes();});
            }
            if(nvars) this.nvars = nvars;
            /* set variables */
            var html = '<table class="table_input"><tr>';
            for(var i = 0; i < this.nvars; i++)
                html += '<td class="td_input"><div id="' + this.div_vars.id + i + '"></div></td>';
            html += '</tr></table>';
            this.div_vars.innerHTML = html; /* update HTML */

            this.vars = new Array;
            for(var i = 0; i < this.nvars; i++){
                var tmpobj, tmptopax;
                if(obj && obj[0]){
                    /* retain existing variables, or copy the last element to the new elements */
                    tmpobj = obj[i] || obj[i - 1];
                    if(topax && topax[0]) tmptopax = topax[i] || topax[i - 1];
                }
                this.vars[i] = new InputData($(this.div_vars.id + i), i,
                                             this.inputUpdated.bind(this),
                                             this.enableAxisBox,
                                             tmpobj, tmptopax);
                if(ndims) this.vars[i].setNumOfDims(ndims, approved);
            }
            if(approved) this.savePaths(); /* save new vars */
        }
    },
    setNumOfDims: function(ndims, approved){
        if(ndims){
            this.ndims = ndims;
            this.vars.each(function(v){v.setNumOfDims(ndims);}, this);
            if(approved) this.savePaths();
        }
    },
    savePaths: function(){
        /* save current configuration for input data */
        this.approvedPaths = this.paths();
        this.approvedVarsHash = this.arrayOfHash();
        this.approvedVarsJSON = this.approvedVarsHash.map(function(v){return Object.toJSON(v);});
        this.approvedVarsObj = this.approvedVarsJSON.map(function(v){return v.evalJSON(true);});
        this.approvedAxes = this.vars.map(function(v){return v.axes();});
        this.approvedTreeOfPathAndAxes = this.treeOfPathAndAxes();
    },
    inputUpdated: function(){
        if(this.enableCancel)
            this.var_apply_span.style.display = ""; /* show buttons */
        else
            this.inputApply();
    },
    inputApply: function(){
        if(this.enableCancel)
            this.var_apply_span.style.display = "none"; /* hide buttons */
        this.savePaths();
        (this.updater)();
    },
    inputCancel: function(){
        this.var_apply_span.style.display = "none"; /* hide buttons */
        this.setNumOfInputs(this.approvedVarsObj.length, 
                            this.approvedTreeOfPathAndAxes, 
                            {"input":this.approvedVarsObj}, null, false);
    },
    input: function(i){return this.vars[i];},
    paths: function(){return this.vars.map(function(v){return v.path();});},
    arrayOfHash: function(){return this.vars.map(function(v){return v.toHash();});},
    arrayOfAxes: function(){return this.vars.map(function(v){return v.axes();});},
    treeOfPathAndAxes: function(){return this.vars.map(function(v){return v.treeOfPathAndAxes();});},
    length: function(){return this.vars.length;}
};

var OptWindows = Class.create();
OptWindows.prototype = {
    initialize: function(divDOM){
        this.div = divDOM;
        this.winDIVs = [];
    },
    addWindow: function(html, title, button){
        var num = this.winDIVs.length;
        var id = this.div.id + '_' + num;
        var body = '<div id="' + id + '" class="opts_window">' /* window */
            + '<div>' /* title bar */
            + '<span class="opts_window_title">' + title + '</span>'
            + '<input type="button" id="' + id + '_close" value="X" class="corner_button corner_button_close" title="Close">'
            + '</div></br>'
            + '<div id="' + id + '_contents" class="opts_window_body">' + html + '</div>' /* body */
            + '</div>';
        this.div.insert(body); /* update HTML */
        this.winDIVs.push($(id));
        this.winDIVs[num].style.display = "none";
        button.observe("click", this.open.bind(this, num));
        $(id + '_close').observe("click", this.open.bind(this, num));
    },
    getContentsDIV: function(num){return $(this.winDIVs[num].id + '_contents');},
    open: function(num){
        /* toggle window status */
        if(this.winDIVs[num].style.display == "")
            this.winDIVs[num].style.display = "none";
        else
            for(var i = 0; i < this.winDIVs.length; i++)
                this.winDIVs[i].style.display = (i == num) ? "" : "none";
    },
    closeAll: function(){this.winDIVs.each(function(div){div.style.display = "none";});}
};

var DrawGeneralOptions = Class.create();
DrawGeneralOptions.prototype = {
    initialize: function(diagram, divDOM, opts){
        /* options */
        this.diagram = diagram;
        this.div = divDOM; /* DOM of DIV element for general options of draw methods */

        this.opts = [];
        this.optNames = ['projection','map_fit','map_axis','map_radius','map_window',
                         'size','viewport','colormap'];

        this.ids = [];
        this.optNames.each(function(nm){this.ids[nm] = this.div.id + '_' + nm;}, this);

        /* create HTML */
        var html = '';
        html += '<input type="button" id="' + this.div.id + '_reset" value="Reset"><br>';

        /* PROJECTION */
        html += 'Projection: '
            + '<select id="' + this.ids['projection'] + '">';
        for(var i = 0; i < projections.length; i++){
            html += '<option value="' + projections[i].itr + '">'
                + projections[i].description
                + '</option>';
        }
        html += '</select>';
        html += '<br />';
        html += '<div class="div_indent">';
        html += this.createCheckBox({'id':this.ids['map_fit'], 'label':'Map fit', 'checked':'', 'title':'Fit window to data'});
        html += this.createTextBox({'id':this.ids['map_axis'], 'label':'Map axis (lon, lat, rot in deg):', 'value':'', 'size':20, 'title':"Comma-separated values of longitude, latitude, and rotation in degrees for the map projection (for example, '180.0, 45.0, 30.0')"});
        html += '<span id="' + this.ids['map_radius'] + '_span">'
            + '<table><tr><td>Map radius (deg): </td><td><div id="' + this.ids['map_radius'] + '"></div></td></tr></table>' 
            + '</span>';
        html += this.createTextBox({'id':this.ids['map_window'], 'label':'Map window (lon0, lon1, lat0, lat1 in deg):', 'value':'', 'size':20, 'title':"Comma-separated minimum and maximum values of longitude and latitude in degrees (for example, '0.0, 360.0, -90.0, 90.0')"});
        html += '</div>';

        /* DIAGRAM SIZE */
        html += 'Diagram size:';
        var dgsName = this.ids['size'] + '_name';
        drawSizeList.each(
            function(hash){
                html += '<input type="radio" id="' + this.ids['size'] + '_' + hash.name + '" name="' + dgsName + '">'
                    + '<label for="' + this.ids['size'] + '_' + hash.name + '">' + hash.name + '</label>';
            }, this);
        html += '<br />';

        /* VIEWPORT */
        var vptids = [this.div.id + '_vptx', this.div.id + '_vpty'];
        html += '<table>';
        for(var i = 0; i < 2; i++)
            html += '<tr><td>Viewport in ' + ['x', 'y'][i] + ':</td><td><div id="' + vptids[i] + '"></div></td></tr>';
        html += '</table>';

        /* COLOR MAP */
        html += 'Color map: ';
        html += '<select id="' + this.ids['colormap'] + '">';
        for(var i = 0; i < colorMapNames.length; i++){
            html += '<option value="' + (i + 1) + '">'
                + (i + 1) + ': ' + colorMapNames[i]
                + '</option>';
        }
        html += '</select>';
        /*
        for(var i = 0; i < colorMapNames.length; i++)
            html += '<div id="' + this.ids['colormap'] + '_popup_' + i + '" style="display:none"></div>';
         */

        /* update HTML */
        this.div.innerHTML = html;

        /* Sliders for map radius */
        var mapRadAry = [1];
        for(var i = 0; i < 178; i++) mapRadAry.push(parseInt(mapRadAry[mapRadAry.length - 1]) + 1);
	this.mapRadSlider = new Slider(this.ids['map_radius'],
                                       mapRadAry.length, /* 1 px = 1 deg */
                                       4, mapRadAry, false, null, null);
	this.mapRadSlider.leftHandler = this.setMapProj.bind(this);

        /* Sliders for viewport */
	this.vptSliders = [];
        var vptary = [0];
        for(var i = 0; i < 100; i++) vptary.push(parseInt(vptary[vptary.length - 1]) + 1);
        vptary = vptary.map(function(m){return parseFloat(m) * 0.01;});
	for(var i = 0; i < 2; i++){
	    this.vptSliders[i] = new Slider(vptids[i], 100, 4, vptary, true, null, null);
	    this.vptSliders[i].leftHandler = this.setViewport.bind(this);
	    this.vptSliders[i].rightHandler = this.setViewport.bind(this);
	}

        $(this.ids['projection']).observe("change", this.setProjection.bind(this));
        $(this.ids['map_fit']).observe("click", this.setMapProj.bind(this));
        $(this.ids['map_axis']).observe("change", this.setMapProj.bind(this));
        $(this.ids['map_window']).observe("change", this.setMapProj.bind(this));
        drawSizeList.each(function(hash){
                              $(this.ids['size'] + '_' + hash.name).observe(
                                  "click", this.setSize.bind(this));}, this);
        $(this.ids['colormap']).observe("change", this.setColorMap.bind(this));
        /*
        for(var i = 0; i < colorMapNames.length; i++){
            var op = $(this.ids['colormap']).options[i];
            op.observe("mouseover", this.showColormapSample.bind(this, i));
            op.observe("mouseout", this.hideColormapSample.bind(this, i));
        }
         */
        /* reset button */
        $(this.div.id + '_reset').observe("click", this.reset.bind(this));

        this.setOpts(drawOptsDefault, true); /* default values */
        if(opts) this.setOpts(opts); /* given values */
        this.setProjection();
    },
    setOpts: function(opts, force){
        if(!opts) return;
        var tmpopt;
        for(var i = 0; i < this.optNames.length; i++){
            tmpopt = opts[this.optNames[i]];
            if(tmpopt || tmpopt == 0 || typeof(tmpopt) == "boolean" || force) /* check existence */
                this.opts[this.optNames[i]] = opts[this.optNames[i]];
        }

        /* PROJECTION */
        var projNum = itrToProjNum(this.opts['projection']);
        $(this.ids['projection'])[projNum].selected = true;
        $(this.ids['map_fit']).checked = this.opts['map_fit'];
        $(this.ids['map_axis']).value = this.opts['map_axis'] ? this.opts['map_axis'].join(',') : "";
        this.mapRadSlider.setLeft(this.opts['map_radius']
                                  || projections[projNum].map_proj && projections[projNum].map_radius['default'] 
                                  || 90); /* specifying value when not enabled is problematic */
        $(this.ids['map_window']).value = this.opts['map_window'] ? this.opts['map_window'].join(',') : "";

        /* DIAGRAM SIZE */
        for(var i = 0; i < drawSizeList.length; i++)
            if(this.opts['size'][0] == drawSizeList[i].size[0])
                $(this.ids['size'] + '_' + drawSizeList[i].name).checked = true;

        /* VIEWPORT */
        var vpt = this.opts['viewport'];
        if(!Object.isArray(vpt))
            vpt = vpt.split(',');
        for(var i = 0; i < 2; i++){
            this.vptSliders[i].setLeft(vpt[i * 2] || drawOptsDefault['viewport'][i * 2]);
            this.vptSliders[i].setRight(vpt[i * 2 + 1] || drawOptsDefault['viewport'][i * 2 + 1]);            
        }

        /* COLOR MAP */
        for(var i = 0; i < colorMapNames.length; i++)
            if(this.opts['colormap'] == i + 1) $(this.ids['colormap'])[i].selected = true;
    },
    reset: function(e){
        this.setOpts(drawOptsDefault, true); /* set default values */
        this.setProjection();
        if(e) this.diagram.execute();
    },
    createTextBox: function(cfg){
        return '<span id="' + cfg['id'] + '_span"' 
            + (cfg['title'] ? ' title="' + cfg['title'] + '"' : '') 
            + '>'
            + cfg['label'] + ' '
            + '<input id="' + cfg['id'] + '" type="text" size="' + cfg['size'] + '" value="' + cfg['value'] + '"><br>'
            + '</span>';
    },
    createCheckBox: function(cfg){
        return '<span id="' + cfg['id'] + '_span"'
            + (cfg['title'] ? ' title="' + cfg['title'] + '"' : '') 
            + '>'
            + '<input id="' + cfg['id'] + '" type="checkbox" ' + cfg['checked'] + '>'
            + '<label for="' + cfg['id'] + '">' + cfg['label'] + '</label><br>'
            + '</span>';
    },
    setProjection: function(e){
        var diagram = this.diagram;
        var i;
        /* check if map projection is applicable */
        var select = $(this.ids['projection']);
        var ifMap = (diagram.ndims >= 2)
                && (diagram.newAxisBox.axisName(0).toLowerCase().match(/^lon/i)
                || diagram.newAxisBox.axisObject(0).units.toLowerCase().match(/^deg.*e/i))
                && (diagram.newAxisBox.axisName(1).toLowerCase().match(/^lat/i)
                || diagram.newAxisBox.axisObject(1).units.toLowerCase().match(/^deg.*n/i));
        for(i = 0; i < projections.length; i++)
            select[i].disabled = (projections[i].itr >= 10 && (!ifMap));
        /* check if log axis is applicable to abscissa and/or ordinate */
        var ifLon = [true, true]; /* applicability of log axis for abscissa and ordinate */
        var win = diagram.draw_window ? diagram.draw_window.map(function(w){return parseFloat(w.value);}) : [];
        /* decide if y axis should be checked or not */
        var jmax = (((diagram.ndims > 1) || ((!isNaN(win[2]) && !isNaN(win[3])))) ? 1 : 0);
        if(diagram.ndims > 0){
            for(var j = 0; j <= jmax; j++){
                var cut = diagram.newAxisBox.axisCut(j);
                var axmin = (!isNaN(win[j * 2    ])) ? win[j * 2    ] : cut[0];
                var axmax = (!isNaN(win[j * 2 + 1])) ? win[j * 2 + 1] : cut[1];
                var k = (diagram.spOpts.toHash().get('exchange') ? 1 - j : j);
                ifLon[k] = (axmin * axmax > 0)
                    && ((axmin / axmax > 5) || (axmin / axmax < 0.2));
                select[itrToProjNum(3 - k)].disabled = (!ifLon[k]);
            }
        }
        /* check if log-log coordinate is applicable */
        select[itrToProjNum(4)].disabled = (!ifLon[0]) || (!ifLon[1]);
        /* reset projection type if necessary */
        if(select[select.selectedIndex].disabled) select.selectedIndex = 0;

        this.opts['projection'] = select.value;
        this.setMapProj();
        if(e) diagram.execute();
    },
    setMapProj: function(e){
        var itr = this.opts['projection'];
        var proj = itrToProj(itr);
        var enableOpts = [];
        var fit_checked = $(this.ids['map_fit']).checked;
        if(proj.map_proj){
            enableOpts['map_fit'] = proj.map_fit.enable;
            if(!(enableOpts['map_fit'] && fit_checked))
                ['map_radius', 'map_axis', 'map_window'].each(
                    function(opt){enableOpts[opt] = proj[opt].enable;}, this);
        }
        ['map_fit', 'map_radius', 'map_axis', 'map_window'].each(
            function(opt){
                $(this.ids[opt] + '_span').style.display = ((proj.map_proj && enableOpts[opt]) ? "" : "none");
            }, this);
        
        /* update options */
        this.opts['map_fit'] = fit_checked;
        this.opts['map_axis'] = ($(this.ids['map_axis']).value) ? $(this.ids['map_axis']).value.split(',').map(parseFloat) : null;
        this.opts['map_radius'] = this.mapRadSlider.getLeft();
        this.opts['map_window'] = ($(this.ids['map_window']).value) ? ($(this.ids['map_window']).value.split(',').map(parseFloat)) : null;

        /* check values */
        if(itr >= 20 && itr <= 23 && this.opts['map_axis'] && this.opts['map_axis'][1] == 0) this.opts['map_axis'][1] = 90;

        /* update values */
        $(this.ids['map_axis']).value = this.opts['map_axis'] ? this.opts['map_axis'].join(',') : "";
        $(this.ids['map_window']).value = this.opts['map_window'] ? this.opts['map_window'].join(',') : "";

        if(e) this.diagram.execute();
    },
    setSize: function(e){
        for(var i = 0; i < drawSizeList.length; i++)
            if($(this.ids['size'] + '_' + drawSizeList[i].name).checked)
                this.opts['size'] = drawSizeList[i].size;
        if(e) this.diagram.execute();
    },
    setViewport: function(event){
        var ary = [];
        for(var i = 0; i < 2; i++){
            ary.push(this.vptSliders[i].getLeft());
            ary.push(this.vptSliders[i].getRight());
            if(event){ /* check values */
                if(ary[i * 2] == ary[i * 2 + 1]){
                    ary[i * 2] = drawOptsDefault.viewport[i * 2];
                    ary[i * 2 + 1] = drawOptsDefault.viewport[i * 2 + 1];
                    this.vptSliders[i].setLeft(ary[i * 2]);
                    this.vptSliders[i].setRight(ary[i * 2 + 1]);
                } else if(ary[i * 2] > ary[i * 2 + 1]){
                    var tmp = ary[i * 2 + 1];
                    ary[i * 2 + 1] = ary[i * 2];
                    ary[i * 2] = tmp;
                    this.vptSliders[i].setLeft(ary[i * 2]);
                    this.vptSliders[i].setRight(ary[i * 2 + 1]);
                }
            }
        }
        this.opts['viewport'] = ary;
        if(event) this.diagram.execute();
    },
    setColorMap: function(e){
        this.opts['colormap'] = $(this.ids['colormap']).value;
        if(e) this.diagram.execute();
    },
    options: function(){
        var result = this.optNames.map(
            function(opn){
                var val = this.opts[opn]; 
                switch(typeof(val)){
                case "number":
                case "string":
                    break; /* do nothing */
                case "boolean":
                    val = (val ? 1 : 0);
                    break;
                case "object": 
                    if(Object.isArray(val)) val = val.join(','); /* Array */
                    else if(!val) val = ""; /* return "" if null */
                    break;
                }
                return (opn + '=' + val);
            }, this);
        if(this.diagram.anim) result.push('anim=' + this.diagram.anim);

        var ax_prefix = ["x", "y", "z", "a", "b", "c"]; /* a, b, c are dummy */
        for(var i = 0; i < this.diagram.ndims; i++)
            result.push(ax_prefix[i] + '_axis=' + this.diagram.newAxisBox.selectedAxes[i]);
        return result.join(',');
    },
    toHash: function(){
        var result = new Hash();
        this.optNames.each(function(opn){result.set(opn, this.opts[opn]);}, this);
        return result;
    }/*,
    showColormapSample: function(num, event){

        var popup = $(this.ids['colormap'] + '_popup_' + num);
        if(popup.innerHTML == "")
            popup.innerHTML = '<img src="' + gfdnaviUrlRoot + '/images/colormap_samples/dcl_colormap_sample_' + (num + 1) + '.png">';
        popup.style.display = "";
        popup.style.position = "fixed";
        popup.style.left = (event.pointerX() + 10) + "px";
        popup.style.top = (event.pointerY() + 10) + "px";
        popup.style.zIndex = 10;
    },
    hideColormapSample: function(num, event){
        $(this.ids['colormap'] + '_popup_' + num).style.display = "none";
    }*/
};

var DrawSpecificOptions = Class.create();
DrawSpecificOptions.prototype = {
    initialize: function(diagram, divDOM, dmid){
        this.diagram = diagram;
        this.dmid = dmid; /* draw method id */
        this.div = divDOM; /* DOM of DIV element for specific options */

        $(divDOM.id + 'Title').innerHTML = drawMethods[dmid].name;

        var html = '<input id="' + divDOM.id + '_reset" type="button" value="Reset"><br>';
        for(var i = 0; i < drawMethods[dmid].opts.length; i++){
            html += '<nobr title="' + (drawMethods[dmid].opts[i].description || '') + '">';
            if(drawMethods[dmid].opts[i].type == "boolean")
                html += '<input id="' + this.optId(i) + '" type="checkbox">' + '<label for="' + this.optId(i) + '">' + drawMethods[dmid].opts[i].name + '</label>';
            else
                html += drawMethods[dmid].opts[i].name + ': <input id="' + this.optId(i) + '">';
            html += '</nobr><br>';
        }
        divDOM.innerHTML = html; /* update HTML */

        this.defaultOpts = [];
        drawMethods[dmid].opts.each(function(dmop){this.defaultOpts[dmop.name] = dmop["default"];}, this);
        this.setOptions(this.defaultOpts); /* set default values */

        for(var i = 0; i < drawMethods[dmid].opts.length; i++){
            var eventType = (drawMethods[dmid].opts[i].type == "boolean") ? "click" : "change";
            $(this.optId(i)).observe(eventType, this.update.bind(this));
        }
        $(divDOM.id + '_reset').observe("click", this.reset.bind(this));
    },
    optId: function(num){return this.div.id + '_spOpt_' + num;},
    reset: function(e){
        this.setOptions(this.defaultOpts, true); /* set default values */
        this.update();
    },
    update: function(event){
        /* validation */
        if(event){
            var target = event.target;
            var opt = target.id.match(/_spOpt_([0-9]+)$/);
            if(opt[0]){
                var optnum = opt[1];
                var optobj = drawMethods[this.dmid].opts[optnum];
                if((optobj.type != "boolean") && (target.value != "")){
                    if(optobj.min && (parseFloat(target.value) < parseFloat(optobj.min)))
                        target.value = optobj.min;
                    if(optobj.max && (parseFloat(target.value) > parseFloat(optobj.max)))
                        target.value = optobj.max;
                }
            }
        }
        this.diagram.generalOpts.setProjection();
        this.diagram.execute();
    },
    setOptions: function(opts, force){
        /* opts: Hash of javascript, not that of prototype.js */
        for(var i = 0; i < drawMethods[this.dmid].opts.length; i++){
            var val = opts[drawMethods[this.dmid].opts[i].name];
            if(val || typeof(val) == "boolean" || val == 0 || force){
                if(drawMethods[this.dmid].opts[i].type == "boolean")
                    $(this.optId(i)).checked = val;
                else
                    $(this.optId(i)).value = val;
            }
        }
    },
    toHash: function(){
        var result = new Hash();
        var val;
        for(var i = 0; i < drawMethods[this.dmid].opts.length; i++){
            switch(drawMethods[this.dmid].opts[i].type){
            case "boolean":
                val = ($(this.optId(i)).checked ? 1 : 0);
                break;
            default:
                val = $(this.optId(i)).value;
                break;
            }
            result.set(drawMethods[this.dmid].opts[i].name, val);
        }
        return result;
    },
    options: function(){
        var result = [];
        this.toHash().each(function(pair){result.push(pair.key + '=' + pair.value);});
        return result.join(',');
    }
};

var DiagramLayer = Class.create();
DiagramLayer.prototype = {
    initialize: function(diagramWindow, layerId, obj, slave){
        this.diagramWindow = diagramWindow;
        this.layerId = layerId;
        this.slave = slave || false;
        this.baseId = diagramWindow.div.id + '_layer' + layerId;

        this.createHTML();
        this.setParams(obj);
    },
    createHTML: function(){
        var id_dm_dims = this.baseId + '_dm_dims';
        var id_dm_dims_new = this.baseId + '_dm_dims_new';
        var id_var_apply_span = this.baseId + '_var_apply_span';
        var id_var_apply_button = this.baseId + '_var_apply_button';
        var id_var_cancel_button = this.baseId + '_var_cancel_button';
        var id_vars = this.baseId + '_vars';
        var id_dgo = this.baseId + '_dgo'; /* general options for draw methods */

        /* initialize windows */
        var optHTML = [];
        /* window for draw method */
        optHTML[1] = '<div id="' + this.baseId + '_win0">'
            + 'Draw method: '
            + '<select id="' + this.baseId + '_dm" class="draw_method">';
        drawMethods.each(
            function(dm){
                optHTML[1] += '<option value="' + dm.name + (dm.user ? "," + dm.user : "") + '" title="' + (dm.description || dm.name) + '">'
                    + dm.name
                    + '</option>';
            });
        optHTML[1] += '</select>'
            + '<div class="div_indent">'
            + '<input id="' + this.baseId + '_anim_check" type="checkbox">'
            + '<label for="' + this.baseId + '_anim_check">Animation</label>'
            + '<input id="' + this.baseId + '_anim_button" type="button" value="Play" style="display:none">'
            + '</div>'
            + '<div id="' + id_dm_dims_new + '"></div>'
            + (this.slave ? '<span style="display:none">' : '')
            + '<div>Window of diagram: <input id="' + this.baseId + '_window_reset" type="button" value="Reset">'
            + '<div class="div_indent">'
            + '<nobr>X min: <input id="' + this.baseId + '_window_0" size="8"> max: <input id="' + this.baseId + '_window_1" size="8"></nobr><br>'
            + '<nobr>Y min: <input id="' + this.baseId + '_window_2" size="8"> max: <input id="' + this.baseId + '_window_3" size="8"></nobr>'
            + '</div>'
            + '</div>'
            + (this.slave ? '</span>' : '')
            + '</div>';
        /* window for input data */
        optHTML[0] = '<div id="' + this.baseId + '_win1"></div>'; /* !!!! optHTML[0] and [1] are swapped. IDs are not wet swapped. !!!! */
        /* window for options */
        optHTML[2] = '<div id="' + this.baseId + '_win2">'
            + (this.slave ? '<span style="display:none">' : '')
            + 'General options:'
            + '<input id="' + this.baseId + '_dgo_showhide" type="button" class="showhidebutton" value="Hide"><br style="clear:both">'
            + '<div id="' + id_dgo + '"class="div_indent"></div>' /* DIV for general options */
            + '<hr>'
            + (this.slave ? '</span>' : '')
            + 'Specific options for <label id="' + this.baseId + '_spOptsTitle"></label>:'
            + '<div id="' + this.baseId + '_spOpts" class="div_indent"></div>' /* DIV for specific options */
            + '</div>'; /* end of id_opts */

        for(var i = 0; i < optHTML.length; i++)
            this.diagramWindow.optWinLayerSelectors[i].tabBody(this.layerId).innerHTML = optHTML[i];

        this.div_dm_dims = $(id_dm_dims);
        this.div_dm_dims_new = $(id_dm_dims_new);
        this.drawMethod = $(this.baseId + '_dm');
        this.anim_check = $(this.baseId + '_anim_check');
        this.anim_button = $(this.baseId + '_anim_button');
        this.draw_window = [];
        for(i = 0; i < 4; i++)
            this.draw_window[i] = $(this.baseId + '_window_' + i);
        this.draw_window_reset = $(this.baseId + '_window_reset');

        /* set evevnt listener */
        this.drawMethod.observe("change", this.dmSelected.bind(this));
        this.anim_check.observe("click", this.setAnim.bind(this));
        this.anim_button.observe("click", this.execAnim.bind(this));
        for(i = 0; i < 4; i++)
            this.draw_window[i].observe("change", this.drawWindowChanged.bind(this));
        this.draw_window_reset.observe("click", this.resetDrawWindow.bind(this));
        if(!this.slave) $(this.baseId + '_dgo_showhide').observe("click", this.genOptShowHideClicked.bind(this));

        /* create HTML for input data */
        this.vars = new InputArray($(this.baseId + '_win1'), this.inputArrayUpdated.bind(this), true, false);

        if(this.slave) /* follow base diagram if slave */
            this.generalOpts = this.diagramWindow.layers[0].generalOpts;
        else /* create HTML for general options if not slave */
            this.generalOpts = new DrawGeneralOptions(this, $(id_dgo), null);
    },
    setParams: function(obj){ /* obj: JSON object which contains parameters of a diagram */
        /* set draw slice */
        this.drawSlice = ((obj && obj.draw_slice) ? obj.draw_slice : 0);

        /* set draw method and specific options */
        this.setDrawMethod((obj ? obj.draw_method : null), (obj ? obj.draw_opts : null));

        /* animation status */
        this.anim_check.checked = ((obj && obj.draw_opts) ? obj.draw_opts.anim : false);
        this.setAnim();

        if(obj && obj['input']){
            this.vars.setInputArray(obj, null, true);
            this.createAxes();
	    if(obj.draw_opts){
		/* set axes */
		var axNames = ['x_axis', 'y_axis', 'z_axis'];
		for(var i = 0; i < this.ndims; i++)
                    this.newAxisBox.setAxes(i, obj.draw_opts[axNames[i]]);
	    }
            if(obj.input[0].functions && obj.input[0].functions.cut)
                this.newAxisBox.importCut(obj.input[0].functions.cut);
            if(obj.draw_opts && obj.draw_opts['window'] && this.draw_window){
                for(var i = 0; i < 4; i++)
                    if(Object.isNumber(obj.draw_opts['window'][i]))
                        this.draw_window[i].value = obj.draw_opts['window'][i];
                this.drawWindowChanged();               
            }
        }
        else
            this.createAxes();

        /* set general options for draw methods */
        if(obj && !this.slave) this.generalOpts.setOpts(obj.draw_opts);

        this.newAxisBox.setCut();
        if(!this.slave) this.generalOpts.setProjection();
    },
    setDrawMethod: function(draw_method, opts){
        if(draw_method) this.drawMethod.value = draw_method;

        this.dmSelected();
        if(opts) this.spOpts.setOptions(opts);
    },
    dmSelected: function(e){
        this.ndims = drawMethods[this.drawMethod.selectedIndex].ndims;
        this.nvars = drawMethods[this.drawMethod.selectedIndex].nvars;

        if((!this.vars) || (this.nvars != this.vars.length))
            this.vars.setNumOfInputs(this.nvars, (this.ndims || 0), null, null, true);

        /* For the moment, create a new instance every time.
         * In future, created instances must be retained. */
        if((!this.spOpts) || (this.spOpts.dmid != this.drawMethod.selectedIndex))
            this.spOpts = new DrawSpecificOptions(this, $(this.baseId + '_spOpts'), this.drawMethod.selectedIndex);

        if(e){
            this.createAxes();
            this.newAxisBox.setCut();
            this.resetDrawWindow();
            if(!this.slave) this.generalOpts.setProjection();
            this.execute();
        }
    },
    inputArrayUpdated: function(){
        this.createAxes();
        this.newAxisBox.setCut();
        if(!this.slave) this.generalOpts.setProjection();
        this.execute();
    },
    setAnim: function(e){
        this.anim = (this.anim_check.checked ? 1 : 0);
        this.anim_button.style.display = (this.anim_check.checked ? "" : "none");
        if(this.anim_check.checked) globalAnim.register(this.diagramWindow);
        else globalAnim.unregister(this.diagramWindow);
        if (e){
            var oldAxes = this.newAxisBox.selectedAxes.clone(); /* save current axes */
            this.createAxes(); /* this initializes all axes */
            for(var i = 0; i < this.ndims; i++)
                this.newAxisBox.setAxes(i, oldAxes[i]); /* set current axes again */
            this.newAxisBox.setCut();
            if(!this.slave) this.generalOpts.setProjection();
            this.execute();
        }
    },
    execAnim: function(event){
        if(!this.anim_local){
            this.anim_check.disabled = true;
            if(event){ /* only when called from each diagram, not from left panel  */
                this.anim_button.value = "Stop";
                this.anim_local = true;
                setTimeout(this.stepAnim.bind(this, true), globalAnim.interval());
            }
            /* find the current position */
            this.anim_index = this.newAxisBox.axisObject(this.ndims)['ary'].indexOf(parseFloat(this.newAxisBox.axisCut(this.ndims)[0]));
            if(this.anim_index < 0) /* if the current position is not found */
                this.anim_index = 0;
        } else {
            this.anim_button.value = "Play";
            this.anim_local = false;
            this.anim_check.disabled = false;
        }
    },
    stepAnim: function(calledLocal){
        if((calledLocal && !this.anim_local) || (!calledLocal && this.anim_local)) return; /* animation is stopped */
        var ifLocal = (calledLocal && this.anim_local); /* true if not called from left panel */
        if(ifLocal && this.diagramWindow.imageLoading){ /* if the previous image is not yet loaded */
            setTimeout(this.stepAnim.bind(this, true), 10); /* wait 10 ms */
            return;
        }
        var axis = this.newAxisBox.axisObject(this.ndims)['ary'];
        var pos = this.newAxisBox.axisBoxes[this.ndims].getFree();
        if((++this.anim_index) == axis.length) this.anim_index = 0;
        this.newAxisBox.axisBoxes[this.ndims].setFree(axis[this.anim_index]);
        this.newAxisBox.setCut();
        this.execute();
        if(ifLocal) setTimeout(this.stepAnim.bind(this, true), globalAnim.interval());
    },
    createAxes: function(e){
        this.newAxisBox = new Axes(this.div_dm_dims_new, this.vars.approvedAxes, this.nvars, this.ndims, this.anim,
                                   this.axisChanged.bind(this), this);
    },
    axisChanged: function(){
        if(!this.slave) this.generalOpts.setProjection();
        this.execute();
    },
    getWindow: function(){return "window=" + (this.draw_window_ary ? this.draw_window_ary.join(",") : "");},
    drawWindowChanged: function(e){
        this.draw_window_ary = this.draw_window.map(function(w){return w.value != "" ? w.value : " ";});
        if(!this.slave) this.generalOpts.setProjection();
        if(e) this.execute();        
    },
    resetDrawWindow: function(e){
        this.draw_window.each(function(w){w.value = "";});
        this.drawWindowChanged(e);
    },
    genOptShowHideClicked: function(){
        var button = $(this.baseId + '_dgo_showhide');
        this.generalOpts.div.style.display = ((button.value == "Hide") ? "none" : "");
        button.value = ((button.value == "Hide") ? "Show" : "Hide");
    },
    path: function(){
        var path = this.vars.paths().join(',');
        if(this.nvars > 1) path = '/[' + path + ']';
        var ary = new Array();
        var tmpcut;
        this.newAxisBox.cut.each(
            function(cut){ary.push(cut.key + '=>' + cut.value.join(".."));});
        path += '/cut(' + ary.join(',') + ')';
        path += '/plot(' + this.drawMethod.value + ';' + [this.generalOpts.options(), this.getWindow(), this.spOpts.options()].without('').join(',')  + ')';
        path += '[' + this.drawSlice + ']';
        return path;
    },
    execute: function(){this.diagramWindow.execute();},
    toHash: function(){
        var result = new Hash(); /* defined in prototype.js */
        var draw_opts = this.generalOpts.toHash().merge(this.spOpts.toHash());
        var axNames = ['x_axis', 'y_axis', 'z_axis'];
        for(var i = 0; i < this.ndims; i++)
            draw_opts.set(axNames[i], this.newAxisBox.selectedAxes[i]);
        result.set('draw_method', this.drawMethod[this.drawMethod.selectedIndex].value);
        result.set('draw_opts', draw_opts);
        result.set('draw_slice', this.drawSlice);
        var input_ary = this.vars.approvedVarsHash.clone(); /* an array */
        var cut = this.newAxisBox.toHash();
        input_ary.each(
            function(input){
                var func = (input.get("functions") || $H());
                func.set("cut", cut);
                input.set("functions", func);
            }, this);
        result.set("input", input_ary);
        return result;
    }
};

var LayerSelector = Class.create();
LayerSelector.prototype = {
    initialize: function(div){
        this.div = $(div);
        var html = '<div class="layer_tabtops">'
            + '<span id="' + this.div.id + '_tabtops"></span>'
            + '<span class="layer_tabtop add_button" id="' + this.div.id + '_addbutton"><label class="add_button">+</label></span>'
            + '</div>'
            + '<div id="' + this.div.id + '_tabbodies" class="layer_tabbodies"></div>';
        Element.insert(this.div, html);
        this.addButton = $(this.div.id + '_addbutton');
        this.tabtops = $(this.div.id + '_tabtops');
        this.tabbodies = $(this.div.id + '_tabbodies');
        this.length = 0;
        this.active = 0;
    },
    addLayer: function(title, contents, removable, remover){
        var newId = this.length;
        var top = '<span id="' + this.tabTopSpanId(newId) + '" class="layer_tabtop">'
            + '<label id="' + this.tabTopId(newId) + '">' + title + '</label>';
        top += '</span>';
        Element.insert(this.tabtops, top);
        var body = '<span id="' + this.tabBodyId(newId) + '" style="display:none">'
            + contents
            + '</span>';
        Element.insert(this.tabbodies, body);
        this.length++;
        this.tabTopSpan(newId).observe("click", this.tabClicked.bind(this));
        if(removable && remover)
            this.addRemover(newId, remover);
        if(this.active == newId) this.activate(newId);
    },
    tabTop: function(num){return $(this.tabTopId(num));},
    tabTopId: function(num){return this.div.id + '_tabtop_' + num;},
    tabTopSpan: function(num){return $(this.tabTopSpanId(num));},
    tabTopSpanId: function(num){return this.div.id + '_tabtopspan_' + num;},
    tabBody: function(num){return $(this.tabBodyId(num));},
    tabBodyId: function(num){return this.div.id + '_tabbody_' + num;},
    tabClicked: function(event){this.activate(event.target.id.match(/_tabtop[a-z]*_([0-9]+)/)[1]);},
    activate: function(num){
        for(var i = 0; i < this.length; i++)
            if(i != num){
                this.tabTopSpan(i).className = "layer_tabtop";
                this.tabBody(i).style.display = "none";
            }
        this.tabTopSpan(num).className = "layer_tabtop layer_tabtop_active";
        this.tabBody(num).style.display = "";
    },
    removeAllLayers: function(){
        $A(this.tabtops.childNodes).each(
            function(child){
                if(child.id.match(/_tabtopspan_/)) 
                    this.tabtops.removeChild(child);
            }, this);
        this.tabbodies.innerHTML = '';
        this.length = 0;
    },
    addRemover: function(num, remover){
        Element.insert(this.tabTopSpanId(num), 
                       '<label id="' + this.tabTopId(num) + '_remove" class="layer_remove_button" title="Remove this layer">X</label>');
        $(this.tabTopId(num) + "_remove").observe("click", remover.bind(this));
    }
};

var DiagramWindow = Class.create();
DiagramWindow.prototype = {
    initialize: function(div, idx, obj){
        /* div: DIV object to place the diagram
         * idx: index number of the diagram
         * obj: array of parameters */
        this.div = div; /* div to insert the object */
        this.idx = idx; /* diagram index */
        this.anim = 0;  /* still: 0, animation: 1 */

        this.layers = [];

        var id_dl_rb = div.id + '_dl_rb';
        var id_link2dg = div.id + '_link2dg';
        var id_ctrl = div.id + '_ctrl';
        var id_clone = div.id + '_clone';
        //var winNames = ['dm', 'inputs', 'opts', 'menu'];
        //var winTitles = ['Method & Axis', 'Input', 'Options', 'Action'];
        var winNames = ['inputs', 'dm', 'opts', 'menu'];
        var winTitles = ['Input', 'Method & Axis', 'Options', 'Action'];
        var winIds = winNames.map(function(nm){return div.id + "_" + nm + "_div";});

        var html = '<div class="diagram_top_buttons">' /* DIV for top buttons */
            + '<span id="' + id_ctrl + '" class="opts_window_buttons">';
        for(var i = 0; i < winNames.length; i++)
            html += '<input type="button" value="' + winTitles[i] + '" id="' + div.id + '_' + winNames[i] + '_window_button">';
        html += '</span>'
            + '<input type="button" value="X" id="' + div.id + '_del_button" class="corner_button corner_button_close" title="Delete">' /* define buttons from right to left*/
            + '<span id="' + div.id + '_move_buttons" style="display:none">'
            + '<input type="button" value=">|" class="corner_button" title="Move to the last" onclick="moveDiagram(' + "'" + div.id + "', 'last'" + ')">'
            + '<input type="button" value=">" class="corner_button" title="Move right" onclick="moveDiagram(' + "'" + div.id + "', 'right'" + ')">'
            + '<input type="button" value="<" class="corner_button" title="Move left" onclick="moveDiagram(' + "'" + div.id + "', 'left'" + ')">'
            + '<input type="button" value="|<" class="corner_button" title="Move to the top" onclick="moveDiagram(' + "'" + div.id + "', 'top'" + ')">'
            + '</span>'
            + '</div><br style="clear: both">'
            + '<div id="' + div.id + '_optwins" class="diagram_and_optwins">' /* DIV for the diagram and the option windows */
            + '<img id="'+ div.id + '_image" src="" class="diagram_fullimage" title="Drag & drop to overlay diagrams">' /* diagram */
            + '</div>'; /* end of diagram and windows */

        div.innerHTML = html; /* update HTML */

        /* initialize windows */
        var optHTML = [];
        /* window for "Method & Axis", "Input", and "Options" */
        for(var i = 0; i < 3; i++) optHTML[i] = '';
            //optHTML[i] = '<div id="' + winIds[i] + '"></div>';
        /* window for "Action" */
        optHTML[3] = '<div id="' + winIds[3] + '">'
            + '<ul>'
            + '<li><a id="' + id_dl_rb + '" href="">Download script</a></li>'
            + '<li><a id="' + id_link2dg + '" href="">Link to this diagram</a></li><br>'
            + '<li><input type="button" id="' + id_clone + '" value="Copy this diagram"></li>'
            + '<li>Move diagram to:<br>'
            + '<input type="button" value="|< Top" onclick="moveDiagram(' + "'" + div.id + "', 'top'" + ')">'
            + '<input type="button" value="< Left" onclick="moveDiagram(' + "'" + div.id + "', 'left'" + ')">'
            + '<input type="button" value="Right >" onclick="moveDiagram(' + "'" + div.id + "', 'right'" + ')">'
            + '<input type="button" value="Last >|" onclick="moveDiagram(' + "'" + div.id + "', 'last'" + ')">'
            + '</ul>'
            + '</div>'; /* end of id_menu */
        this.optWin = new OptWindows($(div.id + '_optwins'));
        for(var i = 0; i < winIds.length; i++)
            this.optWin.addWindow(optHTML[i], winTitles[i], $(div.id + '_' + winNames[i] + '_window_button'));
        this.optWinLayerSelectors = [];
        for(var i = 0; i < 3; i++)
            this.optWinLayerSelectors[i] = new LayerSelector(this.optWin.getContentsDIV(i));
        this.ctrl = $(id_ctrl);
        this.image = $(div.id + '_image');
        this.del_button = $(div.id + '_del_button');
        this.dl_rb = $(id_dl_rb);
        this.link2dg = $(id_link2dg);
        this.a_clone = $(id_clone);

        /* set evevnt listener */
        this.del_button.observe("click", this.del.bind(this));
        this.image.observe("load", this.imageLoaded.bind(this, true));
        this.image.observe("error", this.imageLoaded.bind(this, false));
        this.image.observe("click", this.optWin.closeAll.bind(this.optWin)); /* close all windows when clicked */
        this.div.observe("click", selectDiagram.bind(this, this.idx)); /* change background color of selected diagram */
        this.a_clone.observe("click", this.clone.bind(this));
        for(var i = 0; i < 3; i++){ /* button to add a new layer */
            this.optWinLayerSelectors[i].addButton.observe("click", this.addLayer.bind(this, null, true));
            this.optWinLayerSelectors[i].addButton.title = "Add a new layer";
        }
        this.image.diagramWindow = this;
        new Draggable(this.image, {revert: true, ghosting: false});
        Droppables.add(this.image,
                       {accept: ["diagram_fullimage", "diagram_thumbnail"],
                        hoverclass: "diagram_droppable",
                        onDrop: function(drag, drop){drop.diagramWindow.diagramDropped(drag.diagramWindow);}
                       });

        /* set initial values in opts */
        this.setParams(obj);

        this.execute();
    },
    setParams: function(obj){
        /* obj is supposed to be an Array of layers */
        this.removeAllLayers();
        if(!obj || (obj.length == 0)) obj = [null];
        var slave = 0;
        obj.each(function(layer){this.addLayer(layer, slave++);}, this);
    },
    addLayer: function(obj, slave, event){
        var layerId = this.layers.length;
        /* create HTML elements */
        for(var i = 0; i < 3; i++){
            this.optWinLayerSelectors[i].addLayer('Layer ' + layerId,
                                                  '<div id="' + this.div.id + '_win' + i +  '_layer' + layerId + '"></div>',
                                                  (layerId > 0), this.removeLayer.bind(this, layerId));
            if(layerId == 1) this.optWinLayerSelectors[i].addRemover(0, this.removeLayer.bind(this, 0));
        }
        /* create a DiagramLayer object */
        this.layers.push(new DiagramLayer(this, layerId, obj || (layerId > 0 ? Object.toJSON(this.layers[layerId - 1].toHash()).evalJSON(true) : null), slave));
        if(event) this.execute();
    },
    removeLayer: function(num, event){
        this.layers = this.layers.without(this.layers[num]);
        var tmpObj = this.toJSON().evalJSON(true);
        this.removeAllLayers();
        var slave = 0;
        tmpObj.each(function(obj){this.addLayer(obj, slave++);}, this);
        this.execute();
        Event.stop(event);
    },
    removeAllLayers: function(){
        this.layers = [];
        this.optWinLayerSelectors.each(function(sel){sel.removeAllLayers();});
    },
    diagramDropped: function(diagram){
        var obj = diagram.toJSON().evalJSON(true); /* supposed to be an Array of layers */
        obj.each(function(layer){this.addLayer(layer, true);}, this);
        this.execute();
    },
    setAnimationStatus: function(enabled){
        /* animation will be controlled by the base layer */
        this.layers[0].anim_button.value = (enabled ? "Play" : "Stop");
        this.layers[0].anim_check.disabled = !enabled;
        this.layers[0].anim_button.disabled = !enabled;
    },
    execAnim: function(){this.layers[0].execAnim();}, /* just call animation of the base layer */
    stepAnim: function(flag){this.layers[0].stepAnim(flag);}, /* just call animation of the base layer */
    imageLoaded: function(ok, e){
        this.imageLoading = false;
        document.body.style.cursor = 'default';
        if(ok){
            if(!globalAnim.active) uploadDiagrams(); /* update session */
            this.image.removeAttribute("width"); /* remove attributes set by Draggable */
            this.image.removeAttribute("height");
        } else{
            /* get Rails error message in HTML */
            var a = new Ajax.Request(this.image.src, 
                                     {asynchronous: false,
                                      evalScripts: false,
                                      method: 'get'});
            var res = a.transport.responseText;        
            var frag = document.createDocumentFragment();
            frag.appendChild(document.createElement("div"));
            frag.childNodes[0].innerHTML = res;
            var elems = frag.childNodes[0].childElements();
            for(var i = 0; i < elems.length; i++){
                /* get an element for the error message */
                if(elems[i].nodeName.toLowerCase() == "pre"){
                    var ary = elems[i].innerHTML.split("\n");
                    var newary = [];
                    /* display at most 5 lines of the error message */
                    for(var j = 0; j < (ary.length > 5 ? 5 : ary.length); j++) 
                        newary.push(ary[j].unescapeHTML());
                    alert(newary.join("\n"));
                    break;
                }                
            }
        }
    },
    del: function(){
        if(this.layers[0].anim_check.checked) globalAnim.unregister(this);
        diagrams[this.idx] = null;
        uploadDiagrams(); /* update session */
        Element.remove(this.div.id);
        if(selectedDiagram == this.idx) /* if this is the selected diagram */
            for(var i = 0; i < num_diagrams; i++)
                if(diagrams[i]){
                    selectDiagram(i);
                    break;
                }
    },
    path: function(){
        if(this.layers.length > 1)
            return "/[" + this.layers.map(function(m){return m.path();}).join(",") + "]/overlay()";
        else
            return this.layers[0].path();
    },
    execute: function(){
        var newpath = gfdnaviUrlRoot + '/data' + this.path();
        var newimg = newpath + '.png';
        if(this.currentImage != newimg){
            this.imageLoading = true;
            document.body.style.cursor = 'wait';
            this.image.src = newimg;
            this.currentImage = newimg;
            this.dl_rb.href = newpath + '.rb'; /* "Download script" */
            this.link2dg.href = newpath + '.html'; /* "Link to this diagram" */
        }
    },
    toJSON: function(){
        return Object.toJSON(this.layers.map(function(layer){return layer.toHash();}));
    },
    setThumbnail: function(thumb){
        if(thumb) this.optWin.closeAll(); /* close windows */
        this.ctrl.style.display = thumb ? "none" : ""; /* show/hide control buttons */
        $(this.div.id + '_move_buttons').style.display = thumb ? "" : "none";
        this.image.className = thumb ? "diagram_thumbnail" : "diagram_fullimage"; /* image size is written in CSS */
        this.image.removeAttribute("width"); /* remove attributes set by Draggable */
        this.image.removeAttribute("height");
    },
    clone: function(){addNewDiagram(this.toJSON().evalJSON(true), getDiagramPosition(this.div) + 1);}
};

function itrToProjNum(itr){
    return projections.map(function(prj){return prj.itr;}).indexOf(parseInt(itr));}

function itrToProj(itr){return projections[itrToProjNum(itr)];}

function addNewDiagram(obj, position){
    if (variables.length == 0){
        alert("No variable is selected. Please go to 'Finder' or 'Search' to select.");
        return;
    }
    var id_diagram = 'diagram' + num_diagrams;
    var html = '<div id="' + id_diagram + '" class="diagram_div diagram_div_nonactive"></div>';
    Element.insert('analysis2_diagrams', html);
    if(Object.isNumber(position)){
	var dgs = $('analysis2_diagrams');
	var children = dgs.childNodes;
	dgs.insertBefore(children[getDiagramPosition(id_diagram)], children[position]); /* swap */
    }
    diagrams[num_diagrams] = new DiagramWindow($(id_diagram), num_diagrams, obj);
    if($("thumbnailButton").checked) diagrams[num_diagrams].setThumbnail(true);
    selectDiagram(num_diagrams);
    num_diagrams++;
}

function uploadDiagrams(){
    /* upload current diagrams */
    var children = $('analysis2_diagrams').childNodes;
    var matched;
    var ary = new Array();
    for(var i = 0; i < children.length; i++){ /* in the displayed order */
        if(children[i].nodeType == 1){ /* if Element */
            matched = children[i].id.match(/^diagram([0-9]+)/);
            if(matched && diagrams[matched[1]])
                ary.push(diagrams[matched[1]].path());
        }
    }
    var a = new Ajax.Request(gfdnaviUrlRoot + "/analysis2/set_diagrams",
                             {asynchronous:false,
                              evalScripts:false,
                              onFailure: function(){alert('something wrong on the server...');},
                              parameters: 'paths=' + encodeURIComponent(Object.toJSON(ary)),
                              method: 'get'});
}

function selectDiagram(num){
    /* change background color of selected diagram */
    if(!diagrams[num]) return;
    selectedDiagram = num;
    diagrams.each(function(dg){if(dg) dg.div.className = "diagram_div diagram_div_nonactive";});
    diagrams[num].div.className = "diagram_div diagram_div_active";
}

function getDiagramPosition(diagramDIV){
    return $A($('analysis2_diagrams').childNodes).indexOf($(diagramDIV));
}

function setThumbnails(thumb){ /* show/hide thumbnails */
    diagrams.each(function(dg){if(dg) dg.setThumbnail(thumb);});
}

function moveDiagram(div, place){
    var dgs = $('analysis2_diagrams');
    var children = dgs.childNodes;
    var len = children.length;
    var target = getDiagramPosition(div);
    if((((place == "top") || (place == "left")) && (target == 0)) || (((place == "last") || (place == "right")) && (target == (len - 1))))
        return;
    switch(place){
    case "top":
        dgs.insertBefore(children[target], children[0]); /* swap */
        break;
    case "left":
        dgs.insertBefore(children[target], children[target - 1]); /* swap */
        break;
    case "right":
        dgs.insertBefore(children[target + 1], children[target]); /* swap */
        break;
    case "last":
        dgs.appendChild(children[target]);
        break;
    }
    uploadDiagrams();
}

function createKnowledgeWithAllDiagrams(){
    uploadDiagrams();
    document.location = gfdnaviUrlRoot + "/knowledge/new_from_analysis";
}

function clearAll(){
    $('analysis2_diagrams').innerHTML = "";  /* delete HTML */
    diagrams = [];                           /* delete objects */
    num_diagrams = 0;                        /* cleaer counter */
    variables = [];                          /* delete variables */
    leftPanels.getContents('vars').innerHTML = ""; /* clear variable and axis list */
    globalAnim.clearAll();    
    /* clear session */
    var a = new Ajax.Request(gfdnaviUrlRoot + "/analysis2/clear",
                             {asynchronous:false,
                              evalScripts:false,
                              method: 'get'});
}

function leftPanelButtonClicked(){
    var leftPanelButton = $("leftpanels_button");
    var isOpen = (leftPanelButton.value == "<");
    leftPanelButton.value = (isOpen ? ">" : "<");
    leftPanelButton.title = (isOpen ? "Show" : "Hide");
    $("leftpanels").childElements().each(
        function(node){
            if(node.id != "leftpanels_button")
                node.style.display = (isOpen ? "none" : "");});
}

function leftVariablesNameMouseOver(varNum){
    leftAllAxesIndex[varNum].each(
        function(idx){$('left_axes_' + idx).className = "left_axes_name_highlight";});
}

function leftVariablesNameMouseOut(varNum){
    leftAllAxesIndex[varNum].each(
        function(idx){$('left_axes_' + idx).className = "left_axes_name";});
}

var LeftPanelWindows = Class.create();
LeftPanelWindows.prototype = {
    initialize: function(div){
        this.div = $(div);
        Element.insert(this.div, '<input id="leftpanels_button" type="button" value="<" title="Hide" class="corner_button">');
        this.windows = [];
        this.display = new Object;
    },
    addWindow: function(name, title, contents){
        var html = "";
        if(this.windows.length > 0) html += '<br clear="both">';
        html += '<div id="' + this.div.id + '_' + name + '" class="left_eachpanel">'
            + '<label>' + title + '</label>'
            + '<input id="' + this.div.id + '_' + name + '_showhide" type="button" value="Hide" class="showhidebutton">'
            + '<div id="' + this.div.id + '_' + name + '_contents" class="leftpanel_contents">'
            + contents
            + '</div></div>';
        Element.insert(this.div, html);
        $(this.div.id + '_' + name + '_showhide').observe("click", this.showHideContents.bind(this, name));
        this.windows.push(name);
        this.display[name] = true;
    },
    getContents: function(name){return $(this.div.id + '_' + name + '_contents');},
    showHideContents: function(name){
        $(this.div.id + '_' + name + '_showhide').value = this.display[name] ? "Show" : "Hide";
        this.getContents(name).style.display = this.display[name] ? "none" : "";
        this.display[name] = !this.display[name];
    }
};

var GlobalAnimation = Class.create();
GlobalAnimation.prototype = {
    initialize: function(div){
        this.div = div;
        this.speeds = [500, 1000, 2000]; /* in [ms] */
        this.speed_names = ["0.5 s", "1 s", "2 s"];

        var html = 'Animation:<br><div class="div_indent">' 
            + '<input id="global_anim_button" type="button" value="Start all animations"><br/>';
        html += 'Interval: ';
        html += '<nobr>';
        for(var i = 0; i < this.speeds.length; i++){
            html += '<input type="radio" id="global_anim_speed_' + i + '" name="global_anim_speed">';
            html += '<label for="global_anim_speed_' + i + '">' + this.speed_names[i] + '</label>';
        }
        html += '</nobr>'
            + '</div>';
        this.div.innerHTML = html;

        this.button = $("global_anim_button");
        this.button.disabled = true;
        this.button.observe("click", this.buttonClicked.bind(this));
        $("global_anim_speed_1").checked = true; /* default interval is 1 s */
        this.objects = [];
    },
    register: function(obj){
        this.objects.push(obj);
        this.button.disabled = false;        
    },
    unregister: function(obj){
        if(obj) this.objects = this.objects.without(obj);
        if(this.objects.length == 0){
            this.button.disabled = true;
            if(this.active){
                this.active = false;
                this.button.value = "Start all animations";
            }
        }
    },
    clearAll: function(){
        this.objects = [];
        this.unregister();
    },
    buttonClicked: function(event){
        if(this.active){
            this.active = false;
            this.button.value = "Start all animations";
            this.objects.each(
                function(obj){
                    obj.setAnimationStatus(true);
                });
        }else{
            this.active = true;
            this.button.value = "Stop all animations";            
            this.objects.each(
                function(obj){
                    /* stop individual animation if any */
                    obj.setAnimationStatus(false);
                    /* call execAnim for each diagram */
                    obj.execAnim();
                });
            /* set timer */
            setTimeout(this.stepAllAnim.bind(this), this.interval());
        }
    },
    stepAllAnim: function(){
        if(!this.active) return;
        var wait = false;
        this.objects.each(
            function(obj){
                if(obj.imageLoading){ /* if the previous image is not yet loaded */
                    setTimeout(this.stepAllAnim.bind(this), 10); /* wait 10 ms */
                    wait = true;
                }
            }, this);
        if(wait) return;
        /* all the diagrams are ready for the next step */
        this.objects.each(function(obj){obj.stepAnim(false);});
        setTimeout(this.stepAllAnim.bind(this), this.interval());
    },
    interval: function(){
        for(var i = 0; i < this.speeds.length; i++)
            if($("global_anim_speed_" + i).checked)
                return this.speeds[i];
        return null; /* will be an error */
    }
};
