var canvas;
var ctx;

Droppables.isContained = function(element, drop) {
  var containmentNode;
  if(drop.tree) {
    containmentNode = element.treeNode; 
  } else {
    containmentNode = element.parentNode;
  }
  return !drop._containers.detect(function(c) { return containmentNode == c });
};

var Plug = Class.create();
Plug.prototype = {
  initialize: function(type, entity) {
    var entity_id = entity.entity_id;
    var divId = type + "_plug_" + entity_id;
    new Insertion.Bottom(entity.div,
                         '<div class="plug ' + type + '" id="' + divId + '" style="display:none;"></div>'
                        );
    var div = $(divId);
    var width = div.getWidth();
    var height = div.getHeight();
    div.style.position = 'absolute';
    div.style.left = entity.width/2 - width/2 + 'px';
    if ( type=="input")
      div.style.top = -height/2 - 1 + 'px';
    else
      div.style.top = entity.height - height/2 - 3 + 'px';
    div.show();
    this.entity = entity;
    this.type = type;
    this.div = div;
    div.plug = this;
    this.width = width;
    this.height = height;
    this.cons = new Array();
    this.drag = new Draggable(div,
      {
        zindex: false,
        constraint: true,
        snap: function(x,y,obj) {
          var relPos = this.entity.relativePosition();
          var offset = obj.offset;
          drawCanvas();
          ctx.beginPath();
          ctx.strokeStyle = "blue";
          ctx.lineWidth = 1;
          var posFrom = this.getCenter();
          ctx.moveTo( posFrom[0], posFrom[1]);
          ctx.lineTo( x+relPos[0]+offset[0], y+relPos[1]+offset[1] );
          ctx.stroke();
          return [x,y]
        }.bind(this),
        endeffect: function(element) {
          drawCanvas();
        }
      }
    );
    Droppables.add(div,
      {
        accept: type=="input" ? "output" : "input",
        hoverclass: "plug_hover",
        containment: entity.div,
        onDrop: function(from, dest, event) {
          from = from.plug;
          dest = dest.plug;
          if (from.type == "input") {
            var tmp = from;
            from = dest;
            dest = tmp;
          }
          new Connector(from, dest);
        }
      }
    );
  },
  getCenter: function() {
    var pos = Position.cumulativeOffset(this.div);
    var offset = this.entity.offset();
    var x = pos[0] + this.width/2 - offset[0] + 1;
    var y = pos[1] + this.height/2 - offset[1] + 1;
    return [x, y]
  },
  pushConnector: function(con) {
    this.cons.push(con);
  },
  removeConnector: function(con) {
    this.cons = this.cons.without(con);
  },
  destroy: function() {
    for (var i=0; i<this.cons.length; i++) {
      this.cons[i].destroy();
    }
    this.drag.destroy();
    Droppables.remove(this.div);
    Element.remove( this.div );
  }
}

var ApiEntity = Class.create();
ApiEntity._list = new Array();
ApiEntity._selected = null;
ApiEntity.prototype = {
  initialize: function(name, form, output) {
    var entity_id = ApiEntity._list.last() ? ApiEntity._list.last().entity_id+1 : 0;
    var div_id = "entity_" + entity_id;
    var img_id = "entity_delete_" + entity_id;
    var html;
    html  = '<div class="entity" id="' + div_id + '" style="display:none">';
    html +=   '<nobr>';
    html +=     '<span>' + name + '</span>';
    html +=     '<img src="' + gfdnaviUrlRoot + "/images/x.gif" + '" class="delete" alt="delete" id="' + img_id + '" style="margin-left:3px;"/>';
    html +=   '</nobr>';
    html += '</div>';
    new Insertion.Bottom('api_entities', html);
    var div = $(div_id);
    div.style.position = 'absolute';
    var height = canvas.getHeight()-25
    var y = (100*entity_id + 10);
    var x = (100*parseInt(y/height) + 10)%(canvas.getWidth()-50);
    y = y%height;
    var img = $(img_id);
    img.onclick = this.destroy.bind(this);
    var form_id = "entity_form_" + entity_id;
    html  = '<div id="' + form_id + '" style="display:none;">'
    html +=   form.gsub("api_id",entity_id);
    html += '</div>'
    new Insertion.Bottom('entity_form', html);
    div.onmousedown = this.select.bind(this);
    this.canvas = canvas;
    var offset = this.offset();
    div.style.left = offset[0] + x + 'px';
    div.style.top = offset[1] + y + 'px';
    div.show();
    this.entity_id = entity_id;
    this.div = div;
    this.width = div.getWidth();
    this.height = div.getHeight();
    this.inputPlug = new Plug("input", this);
    this.outputPlug = new Plug("output", this);
    this.form = $(form_id);
    this.name = name;
    this.output = output;
    ApiEntity._list.push( this );
    this.drag = new Draggable(div,
      {
        starteffect: function(element) {
          this._clone = element.cloneNode(true);
          element.parentNode.insertBefore(this._clone, element);
          element._opacity = Element.getOpacity(element);
          Draggable._dragging[element] = true;
          new Effect.Opacity(element, {duration:0.2, from:element._opacity, to:0.5});
        }.bind(this),
        snap: function(x,y) {
          var width = this.canvas.getWidth();
          var height = this.canvas.getHeight();
          var offset = this.offset();
          var xmin = offset[0];
          var xmax = offset[0] + width - this.width;
          var padding = 2;
          if ( x < xmin+padding )
            x = xmin+padding;
          else if ( x > xmax-padding )
            x = xmax-padding;
          var ymin = offset[1] + this.inputPlug.height/2;
          var ymax = offset[1] + height - this.height - this.outputPlug.height/2;
          if ( y < ymin+padding )
            y = ymin+padding;
          else if ( y > ymax-padding )
            y = ymax-padding;
          return [x,y];
        }.bind(this),
        revert: function(elm) {
          Element.remove(this._clone);
          this._clone = null;
          drawCanvas();
          return false;
        }.bind(this)
      }
    );
    this.select();
  },
  offset: function() {
    return Position.cumulativeOffset(this.canvas);
  },
  relativePosition: function() {
    var ary0 = Position.cumulativeOffset(this.div);
    var ary1 = this.offset();
    return [ ary0[0]-ary1[0], ary0[1]-ary1[1] ]
  },
  select: function() {
    if ( ApiEntity._selected ) ApiEntity._selected.unselect();
    if ( Connector._selected ) Connector._selected.unselect();
    this.form.show();
    Element.addClassName(this.div, "selected");
    ApiEntity._selected = this;
  },
  unselect: function() {
    this.form.hide();
    Element.removeClassName(this.div, "selected");
    ApiEntity._selected = null;
  },
  destroy: function(e) {
    this.inputPlug.destroy();
    this.outputPlug.destroy();
    this.drag.destroy();
    Element.remove( this.form );
    Element.remove( this.div );
    ApiEntity._list = ApiEntity._list.without(this);
    if (e) Event.stop(e);
  }
}

var Connector = Class.create();
Connector._list = new Array();
Connector._selected = null;
Connector.prototype = {
  initialize: function(from, dest) {
    var connector_id = Connector._list.last() ? Connector._list.last().connector_id+1 : 0;
    var html;
    var div_id = "connector_" + connector_id;
    var box_id = "connector_box_" + connector_id;
    var img_id = "connector_delete_" + connector_id;
    html  = '<div id="' + div_id + '" style="display:none">';
    html +=   '<nobr>';
    html +=     '<div id="' + box_id + '" class="connector"></div>';
    html +=     '<img src="' + gfdnaviUrlRoot + "/images/x.gif" + '" class="delete" alt="delete" id="' + img_id  + '" style="margin-left:1px;display:none;"/>';
    html +=   '</nobr>';
    html += '</div>';
    new Insertion.Bottom('api_entities', html);
    var div = $(div_id);
    var box = $(div_id);
    var img = $(img_id);
    div.style.position = 'absolute';
    div.onmouseover = function(){ this.img.show(); }.bind(this);
    div.onmouseout = function(){ this.img.hide(); }.bind(this);
    box.onclick = this.select.bind(this);
    img.onclick = this.destroy.bind(this);
    from.pushConnector( this );
    dest.pushConnector( this );
    var form_id = "connector_form_" + connector_id;
    var connector_select_input_id = "connector_select_input_" + connector_id;
    html  = '<div id="' + form_id + '" style="display:none;">';
    html += '<input type="hidden" name="connector[' + connector_id + '][from_api]" value="' + from.entity.entity_id + '"/>';
    html += '<input type="hidden" name="connector[' + connector_id + '][dest_api]" value="' + dest.entity.entity_id + '"/>';
    html += '<span>';
    html += from.entity.name + ":";
    html += '<select name="connector[' + connector_id + '][from_param]">';
    html += '<option selected></option>';
    from.entity.output.each( function(op) {
      var value = op.gsub(/\(.*\)/,"");
      value = value.sub(/^[^\[]*\[/, "[");
      html += '<option value="' + value + '">';
      html += op;
      html += '</option>';
    });
    html += '</select>';
    html += '</span>';
    html += ' => ';
    html += '<span>';
    html += dest.entity.name + ":";
    html += '<select name="connector[' + connector_id + '][dest_param]" id="' + connector_select_input_id + '">';
    html += '<option selected></option>';
    $A($(dest.entity.form).getElementsByTagName('*')).inject([],
      function(elements, child) {
        if (Form.Element.Serializers[child.tagName.toLowerCase()])
          elements.push(Element.extend(child));
        return elements;
      }
    ).each( function(element) {
      var name = element.name;
      if ( !( name.match("uri_wsdl") || name.match("api_name") ) ) {
        text = name.gsub(/^apis\[\d*\]\[/,"");
        text = text.gsub(/[^\d]\]\[([^\d]*)\]/, function(ary){ return "=>"+ary[1];});
        text = text.gsub(/([^\d])\]/, function(ary){return ary[1];});
        html += '<option value="' + name + '">';
        html +=   text;
        html += '</option>';
      }
    } );
    html += '</select>';
    html += '</span>';
    html += "<hr/>";
    html += '</div>';
    new Insertion.Bottom('entity_form', html);
    $(connector_select_input_id).onchange = function(e) {
      var elm = Event.element(e);
      if (this.selected_input) {
        this.selected_input.readOnly = false;
        this.selected_input.style.color = "";
        this.selected_input.style.backgroundColor = "";
      }
      var ind = elm.selectedIndex;
      this.selected_input = $(elm[ind].value);
      this.selected_input.readOnly = true;
      this.selected_input.style.color = "#888";
      this.selected_input.style.backgroundColor = "#f5f5f5";
    }.bind(this);
    this.form = $(form_id);
    this.from = from;
    this.dest = dest;
    this.connector_id = connector_id;
    this.div = div;
    this.box = box;
    this.img = img;
    this.divWidth = box.getWidth();
    this.divHeight = box.getHeight();
    Connector._list.push( this );
    this.draw();
    div.show();
    this.select();
  },
  offset: function() {
    return Position.cumulativeOffset(canvas);
  },
  draw: function() {
    var posFrom = this.from.getCenter();
    var posDest = this.dest.getCenter();
    if ( this == Connector._selected )
      ctx.strokeStyle = "blue";
    else
      ctx.strokeStyle = "black";
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo( posFrom[0], posFrom[1]);
    ctx.lineTo( posDest[0], posDest[1] );
    ctx.stroke();
    var div = this.div;
    var offset = this.offset();
    div.style.left = (posFrom[0]+posDest[0])/2 - this.divWidth/2 + offset[0] + 1 + 'px';
    div.style.top = (posFrom[1]+posDest[1])/2 - this.divHeight/2 + offset[1] + 1 + 'px';
  },
  select: function() {
    if ( ApiEntity._selected ) ApiEntity._selected.unselect();
    if ( Connector._selected ) Connector._selected.unselect();
    this.form.show();
    Element.addClassName(this.box, "selected");
    Connector._selected = this;
    drawCanvas();
  },
  unselect: function() {
    this.form.hide();
    Element.addClassName(this.box, "selected");
    Connector._selected = null;
    drawCanvas();
  },
  destroy: function(e) {
    if (this.selected_input) {
      this.selected_input.readOnly = false;
      this.selected_input.style.color = "";
      this.selected_input.style.backgroundColor = "";
    }
    this.from.removeConnector( this );
    this.dest.removeConnector( this );
    Element.remove(this.div);
    Element.remove(this.form);
    Connector._list = Connector._list.without( this );
    drawCanvas();
    if (e) Event.stop(e);
  }
}

function drawCanvas() {
  var width = canvas.getWidth();
  var height = canvas.getHeight();
  ctx.clearRect(0,0,width,height);
  var i;
  for (i=0; i<Connector._list.length; i++) {
    Connector._list[i].draw();
  }
}

var Wsdl = Class.create();
Wsdl._list = new Array();
Wsdl._selected = null;
Wsdl.prototype = {
  initialize: function(name, apis, forms, outputs) {
    Wsdl._list.push( name );
    var id = Wsdl._list.length - 1;
    this.name = name;
    this.apis = new Array();
    for (var i=0; i<apis.length; i++) {
      this.apis.push( new WsApi(apis[i], forms[i], outputs[i], id) );
    }
    var html;
    var span_id = 'span_' + name;
    html = '<span id="' + span_id + '" class="link">' + name + '</span><br/>';
    new Insertion.Bottom('wsdl_list', html);
    var span = $(span_id);
    span.onclick = this.select.bind(this);
    this.span = span;
  },
  select: function() {
    var apis = this.apis;
    var api_list = $('api_list');
    if (Wsdl._selected)
      Element.removeClassName(Wsdl._selected.span, "selected");
    api_list.update("");
    for (var i=0; i<apis.length; i++) {
      apis[i].show();
    }
    Wsdl._selected = this;
    Element.addClassName(Wsdl._selected.span, "selected");
  }
}

var WsApi = Class.create();
WsApi.prototype = {
  initialize: function(name, form, output, id) {
    this.name = name;
    this.form = form;
    this.output = output;
    this.id = id;
  },
  show: function() {
    var html;
    var span_id = this.name + '_' + this.id;
    html = '<span id="' + span_id + '" class="link">';
    html +=  this.name;
    html += '</span><br/>';
    new Insertion.Bottom('api_list', html);
    $(span_id).onclick = function() {
                           new ApiEntity(this.name, this.form, this.output);
                         }.bind(this);
  }
}


function execute() {
  new Ajax.Request(gfdnaviUrlRoot + "/work_flow/execute",
                    {
                      parameters: Form.serialize('entity_form'),
                      onLoading: function() { document.body.style.cursor = "wait"; },
                      onComplete: function() { document.body.style.cursor = "auto"; },
                      onFailure: function(req) {
                        alert("operation failed");
//                        alert(req.responseText);
                      }
                    }
                  );
  return false;
}

function get_ruby_code(elm) {
  form = $('entity_form');
  form.action = gfdnaviUrlRoot + "/work_flow/get_ruby_code";
  form.method = "post";
  form.submit();
  return false;
}
