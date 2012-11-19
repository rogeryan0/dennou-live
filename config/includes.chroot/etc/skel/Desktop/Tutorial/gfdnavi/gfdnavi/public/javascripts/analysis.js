var x_axis = null;
var y_axis = null;
var z_axis = null;
var anim_dim = null;
function selectXAxis() {
  var select_x_axis = $('draw_x_axis');
  var select_y_axis = $('draw_y_axis');
  var select_z_axis = $('draw_z_axis');
  var select_anim_dim = $('draw_anim_dim');
  if ( select_x_axis ) {
    var old = x_axis;
    var ary = [0,1,2,3];
    x_axis = select_x_axis.selectedIndex;
    if ( x_axis==y_axis && draw_method.ndims>1 ) {
	ary = ary.without(x_axis);
	if ( draw_method.ndims>2 )
	    ary = ary.without( select_z_axis.selectedIndex );
	if ( select_anim_dim && !select_anim_dim.disabled )
	    ary = ary.without( select_anim_dim.selectedIndex );
      select_y_axis.selectedIndex = ary.min();
      selectYAxis();
    } else if ( x_axis==z_axis && draw_method.ndims>2 ) {
	ary = ary.without(x_axis);
	ary = ary.without( select_y_axis.selectedIndex );
	if ( select_anim_dim && !select_anim_dim.disabled )
	    ary = ary.without( select_anim_dim.selectedIndex );
      select_z_axis.selectedIndex = ary.min();
      selectZAxis();
    } else if ( x_axis==anim_dim && select_anim_dim && !select_anim_dim.disabled ) {
      ary = ary.without(x_axis);
      if ( draw_method.ndims>1 )
        ary = ary.without( select_y_axis.selectedIndex );
      if ( draw_method.ndims>2 )
        ary = ary.without( select_z_axis.selectedIndex );
      select_anim_dim.selectedIndex = ary.min();
      selectAnimDim();
    }
    changeDimensionStatus(old);
    selectProjectionType();
  }
}
function selectYAxis() {
  var select_x_axis = $('draw_x_axis');
  var select_y_axis = $('draw_y_axis');
  var select_z_axis = $('draw_z_axis');
  var select_anim_dim = $('draw_anim_dim');
  if( select_y_axis && draw_method.ndims>1 ) {
    var old = y_axis;
    var ary = [0,1,2,3];
    y_axis = select_y_axis.selectedIndex;
    if ( x_axis==y_axis ) {
      ary = ary.without(y_axis);
      if ( draw_method.ndims > 2 )
        ary = ary.without( select_z_axis.selectedIndex );
      if ( select_anim_dim && !select_anim_dim.disabled )
        ary = ary.without( select_anim_dim.selectedIndex );
      select_x_axis.selectedIndex = ary.min();
      selectXAxis();
    } else if ( z_axis==y_axis ) {
      ary = ary.without(x_axis);
      ary = ary.without(y_axis);
      if ( select_anim_dim && !select_anim_dim.disabled )
        ary = ary.without( select_anim_dim.selectedIndex );
      select_z_axis.selectedIndex = ary.min();
      selectZAxis();
    } else if ( y_axis==anim_dim && select_anim_dim && !select_anim_dim.disabled ) {
      ary = ary.without(x_axis);
      ary = ary.without(y_axis);
      if ( draw_method.ndims > 2 );
        ary = ary.without( select_z_axis.selectedIndex );
      select_anim_dim.selectedIndex = ary.min();
      selectAnimDim();
    }
    changeDimensionStatus(old);
    selectProjectionType();
  }
}
function selectZAxis() {
  var select_x_axis = $('draw_x_axis');
  var select_y_axis = $('draw_y_axis');
  var select_z_axis = $('draw_z_axis');
  var select_anim_dim = $('draw_anim_dim');
  if( select_z_axis && draw_method.ndims>2 ) {
    var old = z_axis;
    var ary = [0,1,2,3];
    z_axis = select_z_axis.selectedIndex;
    if ( x_axis==z_axis ) {
      ary = ary.without(y_axis);
      ary = ary.without(z_axis);
      if ( select_anim_dim && !select_anim_dim.disabled )
        ary = ary.without( select_anim_dim.selectedIndex );
      select_x_axis.selectedIndex = ary.min();
      selectXAxis();
    } else if ( y_axis==z_axis ) {
      ary = ary.without(y_axis);
      ary = ary.without(z_axis);
      if ( select_anim_dim && !select_anim_dim.disabled )
        ary = ary.without( select_anim_dim.selectedIndex );
      select_y_axis.selectedIndex = ary.min();
      selectYAxis();
    } else if ( z_axis==anim_dim && select_anim_dim && !select_anim_dim.disabled ) {
      ary = ary.without(x_axis);
      ary = ary.without(y_axis);
      ary = ary.without(z_axis);
      select_anim_dim.selectedIndex = ary.min();
      selectAnimDim();
    }
    changeDimensionStatus(old);
    selectProjectionType();
  }
}
function selectAnimDim() {
  var select_x_axis = $('draw_x_axis');
  var select_y_axis = $('draw_y_axis');
  var select_z_axis = $('draw_z_axis');
  var select_anim_dim = $('draw_anim_dim');
  if ( select_anim_dim && !select_anim_dim.disabled ) {
    var old = anim_dim;
    var ary = [0,1,2,3];
    anim_dim = select_anim_dim.selectedIndex;
    if ( anim_dim==x_axis ) {
      ary = ary.without(anim_dim);
      if ( draw_method.ndims>1 )
        ary = ary.without( select_y_axis.selectedIndex );
      if ( draw_method.ndims>2 )
        ary = ary.without( select_z_axis.selectedIndex );
      select_x_axis.selectedIndex = ary.min();
      selectXAxis();
    } else if ( anim_dim==y_axis && draw_method.ndims>1 ) {
      ary = ary.without(anim_dim);
      ary = ary.without(x_axis);
      if ( draw_method.ndims>2 )
        ary = ary.without( select_z_axis.selectedIndex );
      select_y_axis.selectedIndex = ary.min();
      selectYAxis();
    } else if ( anim_dim==z_axis && draw_method.ndims>1 ) {
      ary = ary.without(anim_dim);
      ary = ary.without(x_axis);
      ary = ary.without(y_axis);
      select_z_axis.selectedIndex = ary.min();
      selectZAxis();
    }
    changeDimensionStatus(old);
    selectProjectionType();
  }
}

var dimensions = new Array();
var common_dim_index = new Array();
function changeDimensionStatus(old) {
  var id;
  var select_anim_dim = $('draw_anim_dim');
  if ( !(old==null) )
      if( (old!=x_axis) && !(old==y_axis&&draw_method.ndims>1) && !(old==z_axis&&draw_method.ndims>2) && !(old==anim_dim&&!select_anim_dim.disabled) && (draw_method.ndims>0))
      dimensions[common_dim_index[old]].disableRight();
  if ( !(x_axis==null) )
    dimensions[common_dim_index[x_axis]].enableRight();
  if ( draw_method.ndims>1 )
    if ( !(y_axis==null) )
      dimensions[common_dim_index[y_axis]].enableRight();
  if ( draw_method.ndims>2 )
    if ( !(z_axis==null) )
      dimensions[common_dim_index[z_axis]].enableRight();
  if ( !(anim_dim==null) )
    if ( select_anim_dim && !select_anim_dim.disabled )
      dimensions[common_dim_index[anim_dim]].enableRight();
  Element.hide("gmap");
}


function setAnim() {
  var anim_check_box = $('analysis_draw_anim');
  var select_anim_dim = $('draw_anim_dim');
  var draw_button = $('draw_button_fig');
  draw_button.src = "images/drawbutton.gif";
  draw_button.onclick = drawExecute;
  selectXAxis();
  selectYAxis();
  selectZAxis();
  if ( anim_check_box ) {
    if ( !anim_check_box.disabled && anim_check_box.checked ) {
      anim_check_box.disabled = false;
      select_anim_dim.disabled = false;
      selectAnimDim();
      draw_button.src = "images/startanim.gif";
      draw_button.onclick = anim.start.bind(anim);
    } else {
      select_anim_dim.disabled = true;
      select_anim_dim.disabled = true;
      changeDimensionStatus(anim_dim);
    }
    selectProjectionType();
  }
}

var draw_method;
function selectDrawMethod() {
    var anim_check_box = $('analysis_draw_anim');
    var select_anim_dim = $('draw_anim_dim');

    var draw_method_select = $('draw_method');

    if (draw_method_select) {
        var draw_method_options = draw_method_select.options;
        draw_method = draw_method_options[draw_method_select.selectedIndex];
        for(n=0;n<draw_method_options.length;n++)
          Element.hide('draw_'+draw_method_options[n].value+'_settings');
        Element.show('draw_'+draw_method.value+'_settings');
        /* update the order of the variables */
        if(variablesOrder && variablesOrder[draw_method.value])
            setVarOrder(draw_method.value);

        if( draw_method.ndims == 0 ) {
            /* if x, y and z axes are shown, they should be hidden */
            if ( $('draw_x_axis') ) Element.hide('draw_x_axis_row');
            if ( $('draw_y_axis') ) Element.hide('draw_y_axis_row');
            if ( $('draw_z_axis') ) Element.hide('draw_z_axis_row');
            if ( select_anim_dim && select_anim_dim.length>1 ) {
                anim_check_box.disabled = false;
                select_anim_dim.disabled = false;
            } else {
                if ( anim_check_box ) {
                    anim_check_box.disabled = true;
                    select_anim_dim.disabled = true;
                }
            }
            dimensions.each( function(v,i){ v.enableRight(); } );
            setAnim();
        } else if( draw_method.ndims == 1 ) {
            dimensions.each( function(v,i){ v.disableRight(); } );
            /* if x axis is hidden, it should be shown */
            Element.show('draw_x_axis_row');
            changeDimensionStatus(x_axis);
            /* if y axis and z axis are shown, they should be hidden */
            if ( $('draw_y_axis') ) Element.hide('draw_y_axis_row');
            if ( $('draw_z_axis') ) Element.hide('draw_z_axis_row');
            if ( select_anim_dim && select_anim_dim.length>1 ) {
                anim_check_box.disabled = false;
                select_anim_dim.disabled = false;
            } else {
                if ( anim_check_box ) {
                    anim_check_box.disabled = true;
                    select_anim_dim.disabled = true;
                }
            }
	    setAnim();
	} else if( draw_method.ndims == 2 ) {
            dimensions.each( function(v,i){ v.disableRight(); } );
            /* if x axis and y axis are hidden, it should be shown */
            Element.show('draw_x_axis_row');
            changeDimensionStatus(x_axis);
            Element.show('draw_y_axis_row');
            changeDimensionStatus(y_axis);
            /* if z axis is shown, it should be hidden */
            if ( $('draw_z_axis') ) Element.hide('draw_z_axis_row');
            if ( select_anim_dim && select_anim_dim.length>2 ) {
                anim_check_box.disabled = false;
                select_anim_dim.disabled = false;
            } else {
                anim_check_box.disabled = true;
                select_anim_dim.disabled = true;
            }
            setAnim();
	} else {
            /* added by S. Otsuka */
            Element.show('draw_x_axis_row');
            changeDimensionStatus(x_axis);
            /* what is this? */
            Element.show('draw_y_axis_row');
            changeDimensionStatus(y_axis);
            Element.show('draw_z_axis_row');
            changeDimensionStatus(z_axis);
            if ( select_anim_dim && select_anim_dim.length>3 ) {
                anim_check_box.disabled = false;
                select_anim_dim.disabled = false;
            } else {
                anim_check_box.disabled = true;
                select_anim_dim.disabled = true;
            }
	}
        selectProjectionType();
    }
    setAnim();
}


var variablesOrder = new Array();
function selectVarOrder(select, type, i) {
  var new_num = select.selectedIndex;
  variablesOrder[type] || (variablesOrder[type] = new Array());
  var old_num = variablesOrder[type][i];
  variablesOrder[type][i] = new_num;
  for(var n = 0; n < variablesOrder[type].length;n++){
    if(n==i) continue;
    var sel = $(type + '_variables_order_' + n);
    if(sel.selectedIndex == new_num){
      sel.options[old_num].selected = true;
      variablesOrder[type][n] = old_num;
      break;
    }
  }
  copyVarOrder(type);
}

function setVarOrder(type){
    if(variablesOrder && variablesOrder[type]){
        for(var n = 0; n < variablesOrder[type].length; n++){
            var sel = $(type + '_variables_order_' + n);
            if(sel.selectedIndex != variablesOrder[type][n]){
                sel.options[sel.selectedIndex].selected = false;
                sel.options[variablesOrder[type][n]].selected = true;
            }
        }
        copyVarOrder(type);
    }
}

function copyVarOrder(type){
  /* 
     copy data to the hidden form 
     jmax: num of variables for each diagram 
     type: name of the draw method or 'analysis'
  */
  var hidden = document.getElementById('variables_order_hidden');
  var jmax = document.getElementById(type + '_variables_order_0').length;
  /* clean old data */
  while(hidden.childNodes.length != 0)
    hidden.removeChild(hidden.lastChild);
  /* copy new data */
  for(var n = 0; n < variablesOrder[type].length / jmax; n++){
    for(var j = 0; j < jmax; j++){
      var dat = document.createElement('input');
      dat.name = "analysis[draw_variables_order][" + n + "][" + j + "]";
      dat.value = variablesOrder[type][n * jmax + j];
      hidden.appendChild(dat);
    }
  }
}


function getKeyCode(e) {
  if (window.event) {
    return event.keyCode;
  } else if (e.keyCode) {
    return (e.keyCode!=0) ? e.keyCode : e.charCode;
  } else if (e.which) {
    return e.which;
  }
}
function eventStop(e) {
  if ( e ) {
    e.preventDefault(); 
    e.stopPropagation(); 
  } else {
    event.returnValue = false; 
    event.cancelBubble = true; 
  }
}

var executable;
function afterCallBack() {
  if( action_type == 0 ) {
    dimensions.each( function(v,i){ v.disableRight(); } );
    selectDrawMethod();
    if( executable )
      Element.show("draw_button");
    else
      Element.hide("draw_button");
    document.onkeypress = function(e){
      if ( getKeyCode(e)==Event.KEY_RETURN ) {
        //$('draw_button').click();
        drawExecute();
        Event.stop(e);
      }
    };
  }
  if( action_type == 1 ) {
    if( executable )
      if ($("analysis_button"))
        Element.show("analysis_button");
    else
      if ($("analysis_button"))
        Element.hide("analysis_button");
    document.onkeypress = function(e){
      if ( getKeyCode(e)==Event.KEY_RETURN ) {
        $('analysis_button').click();
        Event.stop(e);
      }
    };

  }
}


function openOptionMenu() {
  Element.hide('open_menu_link');
  Element.show('option_menu');
}

function closeOptionMenu() {
  Element.hide('option_menu');
  Element.show('open_menu_link');
}

shrinkHalf = function(element) {
  element = $(element);
  var options = Object.extend({
    moveTransition: Effect.Transitions.sinoidal,
    scaleTransition: Effect.Transitions.sinoidal,
    opacityTransition: Effect.Transitions.none
  }, arguments[1] || {});
  var oldStyle = {
    top: element.style.top,
    left: element.style.left,
    height: element.style.height,
    width: element.style.width,
    opacity: element.getInlineOpacity() };

  var dims = element.getDimensions();
  var moveX = dims.width / 2;
  var moveY = dims.height / 2;
  return new Effect.Parallel(
    [ new Effect.Opacity(element, { sync: true, to: 0.0, from: 1.0, transition:
options.opacityTransition }),
      new Effect.Scale(element, 0.5, { sync: true, transition:
options.scaleTransition})
    ], Object.extend({
         beforeStartInternal: function(effect) {
           effect.effects[0].element.makePositioned();
           effect.effects[0].element.makeClipping(); },
         afterFinishInternal: function(effect) {
           effect.effects[0].element.undoClipping();
           effect.effects[0].element.undoPositioned();
}
       }, options)
  );
}



function showOption(category, name) {
  var first = arguments[2];
  full_name = category+"_"+name+"_option";
  var element = $(full_name);
  Element.show(element);
  Element.hide("add_"+full_name+"_link");
  Element.show("hide_"+full_name+"_link");
  if( !first )
    new Effect.Highlight(element, {
      duration: 3.0
    });
  setCookie("option_" + category + "_" + name, "true", null);
}

function hideOption(category, name) {
  full_name = category+"_"+name+"_option";
  Element.hide("hide_"+full_name+"_link");
  Element.show("add_"+full_name+"_link");
  Effect.Fade(full_name);
  setCookie("option_" + category + "_" + name, "false", null);
}

function showOptions(category) {
  Element.show(category+"_options");
  Element.show(category+"_options_menu");
}

function hideOptions(category) {
  Element.hide(category+"_options");
  Element.hide(category+"_options_menu");
}

var action_type;
function changeActionType(type) {
  action_type = type;
  new Ajax.Request(gfdnaviUrlRoot + "/analysis/action_type_selected",
                   {asynchronous: true,
                    evalScripts: true,
                    parameters: 'action_type=' + type,
                    method: 'post',
                    onLoading: function(){ progressText.start($('progress')); },
                    onComplete: function(res){ progressText.stop(); },
                    onFailure: function(res){
                        $('messages').update('error occured in server<br><div class="information"><p>'+res.responseText+"</p></div>");
                      }
                   });
  return false;
}

function changeVariable() {
  var form = $('variables_body');
  new Ajax.Request(gfdnaviUrlRoot + "/analysis/variables_selected",
                   {asynchronous: true,
                    evalScripts: true,
                    parameters: Form.serialize(form),
                    method: 'post',
                    onLoading: function(){ progressText.start($('progress')); },
                    onComplete: function(res){ progressText.stop(); },
                    onFailure: function(res){
                        $('messages').update('error occured in server<br><div class="information"><p>'+res.responseText+"</p></div>");
                      }
                   });
  return false;
}

var actionButton = null;
var onAction = false;
var onDraw = false;
var onAnim = false;
actionExecuteHook = {
                     onLoading: function(){
                       if ( actionButton ) actionButton.disabled = true;
                       if ( !onAnim ) progressText.start($('progress'));
                       onDraw = true;
                       onAction = true;
                     },
                     onComplete: function(res){
                       if ( onAnim ) {
                         if (anim) anim.moveDial();
                       } else {
                         progressText.stop();
                       }
                       if ( actionButton ) {
                         actionButton.disabled = false;
                         actionButton = null;
                       }
                       onAction = false;
                     },
                     onFailure: function(res){
                        $('messages').update('error occured in server<br><div class="information"><p>'+res.responseText+"</p></div>");
                       }
};

var funcs = null;
var funcVNums = null;
function selectFunction(select) {
  var index = select.selectedIndex;
  var obj = funcs[index];
  var html = "";
  var elm;
  for(var i = 0; i < funcs.length;i++) {
      elm = $('function_arguments_'+funcs[i].name);
      Element.hide(elm);
//      Form.disable(elm);
      Form.getElements(elm).invoke('disable');
  }
  elm = $('function_arguments_'+obj.name);
  Element.show(elm);
//  Form.enable(elm);
  Form.getElements(elm).invoke('enable');

  for(i=0;i<funcVNums.length;i++){
      elm = $('function_variables_order_div_'+funcVNums[i]);
      Element.hide(elm);
//      Form.disable(elm);
      Form.getElements(elm).invoke('disable');
  }
  Element.show(obj.order);
//  Form.enable(obj.order);
  Form.getElements(obj.order).invoke('enable');

  return false;
}

function drawKnowledgeFromAnalysisButton() {
  var node = document.getElementById('create_knowledge_from_analysis');
  if (node.childNodes.length == 0) {
      var form = document.createElement('form');
      form.setAttribute("action", gfdnaviUrlRoot + "/knowledge/new_from_analysis");
      form.setAttribute("method", "post");
      var br1 = document.createElement('br');
      var br2 = document.createElement('br');
      var inputButton = document.createElement('input');
      inputButton.setAttribute("type", "image");
      inputButton.setAttribute("src", gfdnaviUrlRoot + "/images/create_from_analysis_button.png");
      inputButton.setAttribute("name", "create_a_knowledge_document");
      inputButton.setAttribute("alt", "Create a Knowledge Document");
      inputButton.setAttribute("title", "Create a Knowledge Document");
      inputButton.setAttribute("align", "absmiddle");
      var inputHidden = document.createElement('input');
      inputHidden.setAttribute("type", "hidden");
      inputHidden.setAttribute("name", "from_analysis");
      inputHidden.setAttribute("value", "from_analysis");

      form.appendChild(inputButton);
      form.appendChild(inputHidden);
      node.appendChild(br1);
      node.appendChild(br2);
      node.appendChild(form);
  }
}

function deleteKnowledgeFromAnalysisButton() {
  var node = document.getElementById('create_knowledge_from_analysis');
  if (node.childNodes.length != 0) {
      node.removeChild(node.lastChild);
      node.removeChild(node.lastChild);
      node.removeChild(node.lastChild);
  }
}

var benchmark = null;
function drawExecute() { 
  if (actionButton) actionButton.disable = false;
  actionButton = $('draw_button');
  actionExecute('action_type=draw');
  if (benchmark) benchMark.start();
  return false;
}
function analysisExecute() {
  if (actionButton) actionButton.disable = false;
  actionButton = $('analysis_button');
  actionExecute('action_type=analysis');
  if (benchmark) benchMark.start();
  return false;
}
function actionExecute(param) {
  var action_form = $('action_form');
  var variable_form = $('variables_body');
  new Ajax.Request(gfdnaviUrlRoot + "/analysis/execute",
                   Object.extend({
                     asynchronous:true,
                     evalScripts:true,
                     parameters: Form.serialize(variable_form)+'&'+Form.serialize(action_form)+'&'+param,
                     method: 'get'
                   }, actionExecuteHook)
                  );
  return false;
}

var Anim = Class.create();
Anim.prototype = {
  initialize: function() {
    this.interval = 1000;
    this.intervalId = null;
  },
  start: function() {
    this.clear();
    var anim_dim = $('draw_anim_dim').selectedIndex;
    var dim = dimensions[common_dim_index[anim_dim]];
    onAnim = true;
    var action_form = $('action_form');
    var variable_form = $('variables_body');
    var params = Form.serialize(variable_form)+'&'+Form.serialize(action_form);
    var min = dim.left_obj.dial.index;
    var max = dim.right_obj.dial.index;
    if ( this.params!=params || this.dim!=dim || this.min!=min || this.max!=max ) {
      new Ajax.Request(gfdnaviUrlRoot + "/analysis/clear_diagrams",
                       {evalScripts:false,
                       onLoading: function(){ onAction = true; },
                       onComplete: function(){ onAction = false; }
                       }
                      );
      this.ary = dim.ary;
      this.min = min;
      this.max = max;
      this.index = this.min;
      this.diagrams = new Array();
      this.diagramIndex = null;
      this.params = params;
      this.dim = dim;
      this.firstLoop = true;
    }
    var free_obj = dim.free_obj;
    free_obj.dial.index = this.index;
    this.moveDial();
    free_obj.dial_dom.show();
    this.intervalId = setInterval(this.draw.bind(this), this.interval);
    var button = $('draw_button_fig');
    button.src = 'images/stopanim.gif';
    button.onclick = this.stop.bind(this);
    /* this part is a dirty workaround */
    window.document.onmousedown = function(event){ 
      var target;
      if(event) target = event.target;
      else target = window.event.srcElement;
      if(!(target.id && (target.id == "draw_button_fig"))) anim.stop();
    };
  },
  stop: function() {
    this.clear();
    var button = $('draw_button_fig');
    this.dim.free_obj.dial_dom.hide();
    button.src = 'images/startanim.gif';
    button.onclick = this.start.bind(this);
    window.document.onmousedown = null;
  },
  clear: function() {
    if ( this.intervalId ) {
      clearInterval( this.intervalId );
      this.intevalId = null;
    }
    onAnim = false;
  },
  draw: function() {
    if ( onAction || onDraw ) return;
    if ( this.index > this.max ) {
      if ( this.firstLoop )
        this.firstLoop = false;
      this.index = this.min;
    }
    this.dim.free_obj.dial.index = this.index;
    if ( this.firstLoop ) {
      actionExecute( 'anim[val]=' + this.ary[this.index] + '&action_type=draw&disable_benchmark=true' );
    } else {
      this.next();
    }
    this.index += 1;
  },
  next: function() {
    if ( this.diagramIndex == null ) {
      this.diagramIndex = 0;
    } else {
      Element.hide( this.diagrams[this.diagramIndex] );
      this.diagramIndex += 1;
      if ( this.diagramIndex >= this.diagrams.length ) {
        this.diagramIndex = 0;
      }
    }
    Element.show( this.diagrams[this.diagramIndex] );
    this.moveDial();
  },
  moveDial: function() {
    this.dim.moveFreeDial();
  }
};
var anim = new Anim();

function actionDropDiagram(upper, lower) {
  if (actionButton) actionButton.disable = false;
  actionButton = null;
  new Ajax.Request(gfdnaviUrlRoot + '/analysis/pile_up?upper=' + upper + '&lower=' + lower,
                   Object.extend({
                     asynchronous:true,
                     evalScripts:true
                   }, actionExecuteHook)
                  );
}


var num_keywords = 0;
function addRowKeyword() {
  var row;
  num_keywords = num_keywords + 1;
  row = "<tr>\n";
  row += "<td><input type='text' name='keywords[" + num_keywords + "][name]' id='save_keywords_" + num_keywords + "_name'/></td>\n";
  row += "<td><input type='text' name='keywords[" + num_keywords + "][value]' id='save_keywords_" + num_keywords + "_value'/></td>\n";
  row += "</tr>\n";
  new Insertion.Bottom($('save_keywords'),row);
}



var map = null;
var marker0;
var marker1;
var polygon;
var marker_xreverse = false;
var marker_yreverse = false;
function show_gmap(){
    if (typeof(google.maps)!="object") return;
    Element.show('gmap');
    if (parseFloat(dimensions[0].getLeft()) < parseFloat(dimensions[0].getRight())) 
	marker_xreverse = false; 
    else 
	marker_xreverse = true;
    if (parseFloat(dimensions[1].getLeft()) < parseFloat(dimensions[1].getRight())) 
	marker_yreverse = false; 
    else 
	marker_yreverse = true;
    if (map) {
	moveMarker0();
	moveMarker1();
    } else {
	map = new google.maps.Map(document.getElementById("map"),
				  {zoom: 0,
				   center: new google.maps.LatLng(0,180),
				   mapTypeId: google.maps.MapTypeId.HYBRID
				  });
	drawLine();
	var ll = new google.maps.LatLng(dimensions[1].getLeft(),dimensions[0].getLeft());
	var rr = new google.maps.LatLng(dimensions[1].getRight(),dimensions[0].getRight());
	marker0 = new google.maps.Marker({position: ll, draggable: true});
	marker1 = new google.maps.Marker({position: rr, draggable: true});
	google.maps.event.addListener(marker0, "dragend", function() {
		checkMarkers();
		drawLine();
	    });
	google.maps.event.addListener(marker1, "dragend", function() {
		checkMarkers();
		drawLine();
	    });
	marker0.setMap(map);
	marker1.setMap(map);
    }
    dimensions[0].leftHandler = moveMarker0;
    dimensions[1].leftHandler = moveMarker0;
    dimensions[0].rightHandler = moveMarker1;
    dimensions[1].rightHandler = moveMarker1;
}

function checkMarkers(){
    var point0 = marker0.getPosition();
    var point1 = marker1.getPosition();
    var lon0 = point0.lng();
    var lat0 = point0.lat();
    var lon1 = point1.lng();
    var lat1 = point1.lat();
    var tmp = 0;
    var swap01 = false;
    if (lon0 < 0) lon0 = lon0 + 360;
    if (lon1 < 0) lon1 = lon1 + 360;
    if ((lon0 > lon1) ^ marker_xreverse){
	tmp = lon0;
	lon0 = lon1;
	lon1 = tmp;
	swap01 = true;
    }
    if ((lat0 > lat1) ^ marker_yreverse){
	tmp = lat0;
	lat0 = lat1;
	lat1 = tmp;
	swap01 = true;
    }
    dimensions[0].setRight(lon1);
    dimensions[1].setRight(lat1);
    dimensions[0].setLeft(lon0);
    dimensions[1].setLeft(lat0);
    if (swap01){
	marker0.setPosition(new google.maps.LatLng(lat0,lon0));
	marker1.setPosition(new google.maps.LatLng(lat1,lon1));
    }
}

function drawLine() {
    var l0 = parseFloat(dimensions[0].getLeft());
    var l1 = parseFloat(dimensions[1].getLeft());
    var r0 = parseFloat(dimensions[0].getRight());
    var r1 = parseFloat(dimensions[1].getRight());
    var tmp;
    if(marker_xreverse){tmp = l0; l0 = r0; r0 = tmp;}
    if(marker_yreverse){tmp = l1; l1 = r1; r1 = tmp;}
    var sw = new google.maps.LatLng(l1,l0,true);
    var ne = new google.maps.LatLng(r1,r0,true);
    var latlngbounds = new google.maps.LatLngBounds(sw,ne);
    var rectOpts = {bounds: latlngbounds, 
		    fillColor: "#FF0000", 
		    fillOpacity: 0.2, 
		    strokeColor: "#FF0000", 
		    strokeOpacity: 1, 
		    strokeWeight: 2};
    if (polygon)
	polygon.setOptions(rectOpts);
    else{
	polygon = new google.maps.Rectangle(rectOpts);
	polygon.setMap(map);
    }
}

function moveMarker0() {
    marker0.setPosition(new google.maps.LatLng(dimensions[1].getLeft(),dimensions[0].getLeft()));
  drawLine();
  checkLogProjection();
}
function moveMarker1() {
    marker1.setPosition(new google.maps.LatLng(dimensions[1].getRight(),dimensions[0].getRight()));
  drawLine();
  checkLogProjection();
}

function setMapProjOption(){
    var projection = $('draw_projection');
    var projection_1d = $('draw_projection_1d');
    var projection_2d = $('draw_projection_2d');
    var map_fit_visible;
    if(Element.visible('draw_projection_2d')){
	if(projection_2d.value>=20){
	    Element.show('map_radius');
	}else{
	    Element.hide('map_radius');
	}
	if(projection_2d.value==10 || projection_2d.value==11){
	    Element.show('map_fit');
            map_fit_visible = true;
	}else{
	    Element.hide('map_fit');
            map_fit_visible = false;
	}
	if(projection_2d.value>=10 && 
           !(map_fit_visible && ($('analysis_draw_map_fit').checked))){
	    if(projection_2d.value<20)
		Element.show('map_window');
	    else
		Element.hide('map_window');
	    Element.show('map_axis');
	}else{
	    Element.hide('map_axis');
	    Element.hide('map_window');
	}
        /* check validity of map axis */
	updateMapAxis();
	/* SET VALUE TO HIDDEN OBJECT */
	projection.value = projection_2d.value;
    }else{
	Element.hide('map_axis');
	Element.hide('map_radius');
	Element.hide('map_fit');
	Element.hide('map_window');
	/* SET VALUE TO HIDDEN OBJECT */
	projection.value = projection_1d.value;
    }
}

function selectProjectionType(){
    var draw_method;
    var draw_method_select = $('draw_method');
    var select_x_axis = $('draw_x_axis');
    var select_y_axis = $('draw_y_axis');
    var select_z_axis = $('draw_z_axis');
    var select_anim_dim = $('draw_anim_dim');
    if (draw_method_select) {
        var draw_method_options = draw_method_select.options;
        draw_method = draw_method_options[draw_method_select.selectedIndex];
        if( draw_method.ndims >= 2 ) {
            var axes = new Array(select_x_axis,select_y_axis,select_z_axis);
            var flag_lon = false;
            var flag_lat = false;
            for(var i=0; i<axes.length; i++){
                if (axes[i]){
                    var axisname = axes[i][axes[i].selectedIndex].value.toLowerCase().substring(0,3);
                    var axisunits = axes[i][axes[i].selectedIndex].title.toLowerCase();
                    if ((axisname=="lon") || axisunits.match(/^deg.*e/i)) flag_lon = true;
                    if ((axisname=="lat") || axisunits.match(/^deg.*n/i)) flag_lat = true;
                }
            }
            if (flag_lon && flag_lat)
                setProjectionType2D();
            else
                setProjectionType1D();
        } else {
            setProjectionType1D();
        }
    }
}

function setProjectionType1D(){
    Element.show("draw_projection_1d");
    Element.hide("draw_projection_2d");
    setMapProjOption();
    checkLogProjection();
}

function setProjectionType2D(){
    Element.hide("draw_projection_1d");
    Element.show("draw_projection_2d");
    setMapProjOption();
    checkLogProjection();
}

function checkLogProjection(){
    var ndim = draw_method.ndims;
    var proj_select;
    if(Element.visible('draw_projection_2d')) proj_select = $('draw_projection_2d');
    else proj_select = $('draw_projection_1d');

    var indices = new Array;
    /* reset status */
    for(var i = 1; i < 4; i++)
	proj_select.options[i].disabled = false;
    if (ndim != 0){
	if (ndim==1) indices = [x_axis];
	else if(ndim>=2) indices = [x_axis, y_axis];
	var loglogdisabled = false;
	var exchange = $('analysis_'+draw_method.value+'_exchange');
	for (var i = 0; i < indices.length; i++){
	    var dim = dimensions[common_dim_index[indices[i]]];
	    if(dim){
		var minmax = [parseFloat(dim.getLeft()), parseFloat(dim.getRight())];
		if(Math.abs(minmax[0]) > Math.abs(minmax[1]))
		    minmax = [minmax[1],minmax[0]]; /* sort by abs */
                var index;
		if (exchange && (exchange.checked))
		    index = 1 + i; /* if x and y are exchanged */
		else
		    index = 2 - i; /* if not exchanged */
		if (((minmax[0])*(minmax[1]) <= 0) /* if the axis straddles 0 */
		    || (Math.abs(minmax[0])*10 > Math.abs(minmax[1]))) /* if the variable range is too small */
		    {
		    proj_select.options[index].disabled = true;
		    if (proj_select.selectedIndex == index)
			proj_select.selectedIndex = 0;
		    loglogdisabled = true;
		}
	    }
	}
	proj_select.options[3].disabled = loglogdisabled;
	if (proj_select.selectedIndex == 3) /* Log-Log coordinate */
	    proj_select.selectedIndex = 0;
    }
}

function updateMapAxis(){
    var lonval = $('map_axis_lon').value;
    var latval = $('map_axis_lat').value;
    var rotval = $('map_axis_rot').value;
    var lon = (lonval != "") ? parseFloat(lonval) : "";
    var lat = (latval != "") ? parseFloat(latval) : "";
    var rot = (rotval != "") ? parseFloat(rotval) : "";
    var proj = parseInt($('draw_projection_2d').value);
    /* check validity */
    if((lon != "") && (lon < -180)) lon = -180;
    if((lon != "") && (lon >  180)) lon =  180;
    if((lat != "") && (lat <  -90)) lat =  -90;
    if((lat != "") && (lat >   90)) lat =   90;
    if(proj >= 20 && proj <= 23 && lat == 0) lat = 90;
    /* update values */
    $('map_axis_lon').value = lon;
    $('map_axis_lat').value = lat;
    $('analysis_draw_map_axis').value = [lon, lat, rot].map(function(m){return (Object.isNumber(m) ? m : " ");}).join(",");
}

function updateMapWindow(){
    var doms = ['map_win_xmin', 'map_win_xmax', 'map_win_ymin', 'map_win_ymax'].map(function(id){return $(id);});
    var ifnum = doms.map(function(dom){return dom.value != "";});
    var nums = [];
    for(var i = 0; i < 4; i ++) nums[i] = (ifnum[i] ? parseFloat(doms[i].value) : "");
    var tmp;
    /* check validity */
    if(ifnum[0] && (nums[0] < -180)) nums[0] = -180;
    if(ifnum[0] && (nums[0] >  180)) nums[0] =  180;
    if(ifnum[1] && (nums[1] < -180)) nums[1] = -180;
    if(ifnum[1] && (nums[1] >  180)) nums[1] =  180;
    if(ifnum[2] && (nums[2] <  -90)) nums[2] =  -90;
    if(ifnum[2] && (nums[2] >   90)) nums[2] =   90;
    if(ifnum[3] && (nums[3] <  -90)) nums[3] =  -90;
    if(ifnum[3] && (nums[3] >   90)) nums[3] =   90;
    if(ifnum[0] && ifnum[1] && (nums[0] > nums[1])){
        tmp = nums[0];
	nums[0] = nums[1];
	nums[1] = tmp;
    }
    if(ifnum[0] && ifnum[1] && (nums[0] == nums[1])){
	if(nums[0] == -180) nums[1] = 180; else nums[0] = -180;
    }
    if(ifnum[2] && ifnum[3] && (nums[2] > nums[3])){
        tmp = nums[2];
	nums[2] = nums[3];
	nums[3] = tmp;
    }
    if(ifnum[2] && ifnum[3] && (nums[2] == nums[3])){
	if(nums[2] == -90) nums[3] = 90; else nums[3] = -90;
    }
    /* update values */
    for(var i = 0; i < 4; i++) doms[i].value = nums[i];
    $('analysis_draw_map_window').value = nums.map(function(m){return (Object.isNumber(m) ? m : " ");}).join(",");
}

function updateViewport(){
    var vpt = new Array($('draw_viewport_x0'),
                        $('draw_viewport_x1'),
                        $('draw_viewport_y0'),
                        $('draw_viewport_y1'));
    for (i=1; i<4; i++){
        vpt[i].value = parseFloat(vpt[i].value);
        if(vpt[i].value < 0) vpt[i].value = 0.0;
        if(vpt[i].value > 1) vpt[i].value = 1.0;
    }
    if (vpt[0].value > vpt[1].value){
        var tmp = vpt[0].value;
        vpt[0].value = vpt[1].value;
        vpt[1].value = tmp;
    }
    if (vpt[2].value > vpt[3].value){
        var tmp = vpt[2].value;
        vpt[2].value = vpt[3].value;
        vpt[3].value = tmp;
    }
    if (vpt[0].value == vpt[1].value)
        if (vpt[0].value > 0.2) vpt[0].value = 0.2;
        else vpt[1].value = 0.8;
    if (vpt[2].value == vpt[3])
        if (vpt[2].value > 0.2) vpt[2].value = 0.2;
        else vpt[3].value = 0.8;
    $('analysis_draw_viewport').value
	= vpt[0].value + ','
	+ vpt[1].value + ','
	+ vpt[2].value + ','
	+ vpt[3].value;
}

function minimizeDiagram(img_name){
    var img = $(img_name + '_image');
    if (img.naturalWidth == null) img.naturalWidth = img.width;
    img.width = 100;
    var popup = $('popup_menu_diagram_menu_'+(img_name.match(/^diagram_([0-9]+)$/)[1]));
    if(popup) Element.remove(popup); 
}

function maximizeDiagram(img_name){
    var img = $(img_name + '_image');
    if (img.naturalWidth != null) img.width = img.naturalWidth;
    var popup = $('popup_menu_diagram_menu_'+(img_name.match(/^diagram_([0-9]+)$/)[1]));
    if(popup) Element.remove(popup); 
}

function destroyDiagram(img_name){
    var popup = $('popup_menu_diagram_menu_'+(img_name.match(/^diagram_([0-9]+)$/)[1]));
    if(popup) Element.remove(popup); 
    new Effect.Fade(img_name+'_table',{afterFinishInternal: function(effect){Element.remove(effect.element);}});
}

function operationForAllDiagrams(op){
    var diagrams = $('diagrams').children;
    for(var i=0; i<diagrams.length; i++){
        var d = diagrams[i].id.match(/^(diagram_[0-9]+)_table$/);
        if(d) op(d[1]);
    }
}
