/* memo:
 * parent.vars
 * parent.nvars
 * parent.ndims
 * parent.anim
 * */

var Axes = Class.create();
Axes.prototype = {
    initialize: function(div, axes, nvars, ndims, anim, callback, parent){
        /* div: DIV object to place the axis boxes
         * axes: [[axis0 of var0, axis1 of var0, ...], [axis0 of var1, ...], ...]
         * nvars: num of vars for the current draw method
         * ndims: num of dims for the current draw method (0 for functions)
         * anim: flag for animation
         * callback: function to be called when a parameter changes
         * parent: parent object
         */
        this.div = div;
        this.axes = axes;
        this.nvars = nvars;
        this.ndims = ndims;
        this.anim = anim;  /* still: 0, animation: 1 */
        this.callback = callback;
        this.parent = parent;

        this.createAxes();
        this.setCut();
    },
    axisId: function(num){return this.div.id + '_axis' + num;},
    axisTDId: function(num){return this.axisId(num) + '_td';},
    axisInfoId: function(num){return this.axisId(num) + '_info';},
    axis: function(num){return $(this.axisId(num));},
    axisInfo: function(num, box){return $(this.axisInfoId(num, box));},
    axisName: function(num){
        if(num < this.ndims + this.anim)
            return this.axis(num).value;
        else
            return this.axis(num).innerHTML;
    },
    axisCut: function(num){
        var val = [];
        if(this.anim && num == this.ndims)
            val[0] = this.axisBoxes[num].getFree();
        else
            for(var k = 0; k <= ((this.ndims == 0 || num < this.ndims) ? 1 : 0); k++)
                val[k] = ((k == 0) ? this.axisBoxes[num].getLeft() : this.axisBoxes[num].getRight());
        return val;
    },
    axisObject: function(num){
        return this.allAxes[this.allAxesIndexOf(this.axisName(num))];
    },
    allAxesIndexOf: function(name){
        return this.allAxes.map(function(m){return m['name'];}).indexOf(name);
    },
    getPosInArray: function(num, value){
        var idx = 0;
        var ary = this.axisObject(num).ary;
        if(ary){
            var tmp = ary[0];
            var diff1, diff2;
            for(var i = 0; i < ary.length; i++){
                diff1 = tmp - ary[i];
                diff2 = value - ary[i];
                if((diff1 * diff1) < (diff2 * diff2)){
                    tmp = ary[i];
                    idx = i;
                }
            }
        }
        return idx;
    },
    checkCurrentSize: function(){
        var size = 1;
        for(var i = 0; i < this.allAxes.length; i++){
            if((i < this.ndims) || (this.ndims == 0))
                size *= (Math.abs(this.getPosInArray(i, this.axisBoxes[i].getLeft()) 
                                  - this.getPosInArray(i, this.axisBoxes[i].getRight())) 
                         + 1); 
        }
        var flag = true;
        [size_limit_1, size_limit_2].each(
            function(lim){if(lim && (size > lim)) flag = false;});
        /* alert for array size */
        $(this.div.id + "_message").innerHTML = (flag ? "" : "Array size exceeds the limit.");
        $(this.div.id + "_message").style.color = (flag ? "" : "red");
        return flag;
    },
    createAxes: function(e){
        var i,j;
        var axes = this.axes[0];
        this.commonAxes = axes.clone();
        this.allAxes = axes.clone();
        if(this.nvars > 1){
            /* allAxes, commonAxes of selected variables */
            var allAxesNames = this.allAxes.map(function(ax){return ax['name'];});
            var ax;
            for(i = 1; i < this.nvars; i++){
                ax = this.axes[i];
                var axNames = ax.map(function(ax2){return ax2['name'];});
                var tmpcmnAxes = this.commonAxes;
                for(j = 0; j < tmpcmnAxes.length; j++)
                    if(axNames.indexOf(tmpcmnAxes[j]['name']) == -1)
                        this.commonAxes = this.commonAxes.without(tmpcmnAxes[j]);
                /* merger of variable ranges is not yet implemented */
                for(j = 0; j < ax.length; j++)
                    if(allAxesNames.indexOf(ax[j]['name']) == -1)
                        this.allAxes.push(ax[j]);
            }
        }

        if(this.parent && this.parent.drawMethod){
            /* check validity of draw method */
            for(i = 0; i < this.parent.drawMethod.options.length; i++)
                this.parent.drawMethod[i].disabled = (drawMethods[i].ndims > this.commonAxes.length);
            if(this.parent.drawMethod[this.parent.drawMethod.selectedIndex].disabled){
                /* change draw method */
                for(i = 0; i < this.parent.drawMethod.options.length; i++)
                    if(!this.parent.drawMethod[i].disabled)
                        this.parent.drawMethod.selectedIndex = i;
                /* initialize again */
                this.parent.dmSelected();
                this.axes = this.parent.vars.approvedAxes;
                this.nvars = this.parent.nvars;
                this.ndims = this.parent.ndims;
                this.anim = this.parent.anim;
                this.createAxes();
                return;
            }
        }

        if(this.parent && this.parent.anim_check){
            /* check availability of animation */
            if(this.allAxes.length > this.ndims){
                this.parent.anim_check.disabled = false;
                this.parent.anim_button.disabled = false;
            }
            else{
                this.parent.anim_check.checked = false;
                this.parent.anim_check.disabled = true;
                this.parent.anim_button.disabled = true;
                this.parent.setAnim();
            }
        }

        /* create html */
        this.nbox = [];
        for(i = 0; i < this.allAxes.length; i++){
            if(this.anim && i == this.ndims) this.nbox[i] = 3;
            else if(this.ndims == 0 || i < this.ndims) this.nbox[i] = 2;
            else this.nbox[i] = 1;
        }
        var html = '<label id="' + this.div.id + '_message"></label><table id="' + this.div.id + '_table">';
        var selected;
        var axisName;
        for(i = 0; i < this.allAxes.length; i++){
            html += '<tr><td style="white-space:nowrap">';
            if(i < this.ndims) html += 'Axis ' + (i + 1) + ':';
            else if(this.anim && i == this.ndims) html += 'Anim:';
            else html += 'Dim:';
            html += '</td><td style="white-space:nowrap">';
            if(i < this.ndims + this.anim){ /* axis selector */
                html += '<select id="' + this.axisId(i) + '">';
                for(j = 0; j < axes.length; j++){
                    selected = ((i == j) ? 'selected' : '');
                    axisName = axes[j]['name'];
                    html += '<option value="' + axisName + '" ' + selected + '>' + axisName + '</option>';
                }
                html += '</select>';
            }
            else /* names of dimensions which are not used for axes */
                html += '<label id="' + this.axisId(i) + '">' + this.allAxes[i]['name'] + '</label>';
            html += '</td>';
            html += '<td><input type="button" value="Info" id="' + this.axisInfoId(i) + '"></td>';
	    html += '<td id="' + this.axisTDId(i) + '"></td>';
            html += '</tr> ';
        }
        html += '</table>';

        this.div.innerHTML = html; /* update HTML */
        this.table = $(this.div.id + '_table');

	this.axisBoxes = [];
	for(i = 0; i < this.allAxes.length; i++) this.addSlider(i);

        for(i = 0; i < this.allAxes.length; i++){ /* set event handlers */
            if(i < this.ndims + this.anim)
                this.axis(i).observe("change", this.axisChanged.bind(this));
            this.axisInfo(i).observe("click", this.showInfo.bind(this, i));
        }
        this.updateAxisBox();
        this.checkCurrentSize();
        //if(e) this.callback();
    },
    addSlider: function(i){
	this.axisBoxes[i] = new Slider(this.axisTDId(i), 150, 4, this.axisObject(i)['ary'], (this.nbox[i] > 1), null, null);
	if(this.anim && i == this.ndims) this.axisBoxes[i].enableFree();
	this.axisBoxes[i].leftHandler = this.sliderChanged.bind(this);
	this.axisBoxes[i].rightHandler = this.sliderChanged.bind(this);	
    },
    showInfo: function(i){
        var ax = this.axisObject(i);
        var ary = ax['ary'];
        alert(ax['name'] + '[' + ary.length + '] = ' + ary[0] + ' .. ' + ary[ary.length - 1] + ' [' + ax['units'] + ']');
    },
    axisChanged: function(e){
        var target = e.target;
        var targetNum = target.id.match(/axis([0-9]+)$/)[1];
        var targetValue = target.value;
        this.setAxes(targetNum, targetValue);
        this.setCut();
        if(this.checkCurrentSize()) (this.callback)();
    },
    setAxes: function(num, val){
        /* make consistency of all axes */
        var target = this.axis(num);
        if(target[target.selectedIndex] != val)
            target.value = val;
        /* this.selectedAxes[num] is the old value of target */
        var other;
	var tmpary = [num];
        for(var i = 0; i < this.allAxes.length; i++){
            other = this.axis(i);
            if(i < this.ndims + this.anim){ /* select object for axes */
                if((i != num) && (val == other.value)){
                    other.value = this.selectedAxes[num];
		    tmpary.push(i);
		    break;
		}
            }
            else /* label tag for other dims */
                if(val == other.innerHTML){
                    other.innerHTML = this.selectedAxes[num];
		    tmpary.push(i);
		    break;
		}
        }
	tmpary.each(function(i){this.addSlider(i);}, this);
        this.updateAxisBox();
    },
    updateAxisBox: function(){ /* update property */
        this.selectedAxes = new Array();
        for(var i = 0; i < this.allAxes.length; i++){
            var ndim_nanim = this.ndims + this.anim;
            if(i < ndim_nanim) /* axes of the diagram */
                this.selectedAxes[i] = this.axisName(i);
            var ary = this.axisObject(i)['ary']; /* grid points */
            var axlen = ary.length;
            /* do not use this.axisCut() because it reads this.axisBox().value  */
            if(this.cut) var cut = this.cut.get(this.axisName(i));
            this.axisBoxes[i].setLeft(((!this.anim || i != this.ndims) && cut && cut[0]) ? cut[0] : ary[0]); /* left box */

            if(i < ndim_nanim || this.ndims == 0){ /* right box */
                this.axisBoxes[i].setRight(((!this.anim || i != this.ndims) && cut && cut[1]) ? cut[1] : ary[axlen - 1]);
                if((i < ndim_nanim && parseFloat(this.axisBoxes[i].getLeft()) == parseFloat(this.axisBoxes[i].getRight())) && (this.ndims != 0)){
                    /* reset when max == min */
                    this.axisBoxes[i].setLeft(ary[0]);
                    this.axisBoxes[i].setRight(ary[axlen - 1]);
                }
            }
            if(this.anim && i == this.ndims) /* anim box if axis for anim */
                this.axisBoxes[i].setFree((cut && cut[0]) ? cut[0] : ary[0]);
        }
    },
    sliderChanged: function(event){
	if(event){
            this.setCut();
            if(this.checkCurrentSize()) (this.callback)();            
	}
    },
    axisBoxChanged: function(e){
        /* find the closest value in the array of the axis */
        var target = e.target;
        var targetNum = target.id.match(/axis([0-9]+)_([0-9]+)$/)[1];
        var targetValue = target.value;
        var newValue = this.axisObject(targetNum).ary[0];
        this.axisObject(targetNum).ary.each(
            function(val){
                var diff1 = targetValue - newValue;
                var diff2 = targetValue - val;
                if((diff1 * diff1) > (diff2 * diff2)) newValue = val;
            });
        target.value = newValue;
        this.setCut();
        if(this.checkCurrentSize()) (this.callback)();            
    },
    setCut: function(){ /* read the values of the axis boxes */
        this.cut = new Hash(); /* defined by prototype.js */
        for(var i = 0; i < this.allAxes.length; i++)
            this.cut.set(this.axisName(i), this.axisCut(i));
    },
    importCut: function(obj){
        this.cut = new Hash(); /* defined by prototype.js */
        for(var ax in obj)
            this.cut.set(ax, obj[ax].split('..'));
        this.updateAxisBox();
        this.checkCurrentSize();
    },
    toHash: function(){
        var newCut = new Hash;
        this.cut.each(function(dat){newCut.set(dat[0], dat[1].join(".."));});
        return newCut;
    },
    show: function(){this.table.style.display = "";},
    hide: function(){this.table.style.display = "none";}
};

