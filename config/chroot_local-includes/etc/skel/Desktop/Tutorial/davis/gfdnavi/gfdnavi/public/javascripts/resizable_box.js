var HBar = Class.create();
Object.extend(HBar.prototype, Draggable.prototype);
Object.extend(HBar.prototype, {
  initDrag: function(event) {
    this.lastPointerX = Event.pointerX(event);
    Draggable.prototype.initDrag.apply(this, arguments);
    var box = this.boxObj.box;
    var parent = box.parentNode;
    var min_width = this.boxObj.min_width;
    this.left_bound = Position.cumulativeOffset(box)[0] + min_width;
    this.right_bound = Position.cumulativeOffset(parent)[0] + Element.getDimensions(parent).width - min_width - this.div_width;
  },
  updateDrag: function(event, pointer) {
    var offset = this.offset[0];
    var pos = pointer[0] - offset;
    if (pos < this.left_bound) pos = this.left_bound;
    if (pos > this.right_bound) pos = this.right_bound;
    pointer[0] = pos + offset;
    var dist = pointer[0] - this.lastPointerX;
    this.lastPointerX = pointer[0];
    var boxObj = this.boxObj;
    if (!boxObj.box_width) /* recovery from resetWidth */
      boxObj.box_width = parseInt(Element.getDimensions(boxObj.box).width);
    boxObj.setWidth( boxObj.box_width + dist, true );
    Draggable.prototype.updateDrag.apply(this, arguments);
  },
  endDrag: function(event) {
    Draggable.prototype.endDrag.apply(this, arguments);
    var boxObj = this.boxObj;
    setCookie(boxObj.box.id+"Width", boxObj.box_width);
  }
})

var VBar = Class.create();
Object.extend(VBar.prototype, Draggable.prototype);
Object.extend(VBar.prototype, {
  initDrag: function(event) {
    this.lastPointerY = Event.pointerY(event);
    Draggable.prototype.initDrag.apply(this, arguments);
    var box = this.boxObj.box;
    var parent = box.parentNode;
    var min_height = this.boxObj.min_height
    this.top_bound = Position.cumulativeOffset(box)[1] + min_height;
    var parent_height =  parent.style.height;
    if ( parent_height )
	this.bottom_bound = Position.cumulativeOffset(parent)[1] + parent_height - min_height - this.div_height;
  },
  updateDrag: function(event, pointer) {
    var offset = this.offset[1];
    var pos = pointer[1] - offset;
    if (pos < this.top_bound) pos = this.top_bound;
    if (this.bottom_bound && pos > this.bottom_bound) pos = this.bottom_bound;
    pointer[1] = pos + offset;
    var dist = pointer[1] - this.lastPointerY;
    this.lastPointerY = pointer[1];
    var boxObj = this.boxObj;
    if (!boxObj.box_height) /* recovery from resetHeight */
      boxObj.box_height = parseInt(Element.getDimensions(boxObj.box).height);
    boxObj.setHeight( boxObj.box_height + dist, true );
    Draggable.prototype.updateDrag.apply(this, arguments);
  },
  endDrag: function(event) {
    Draggable.prototype.endDrag.apply(this, arguments);
    var boxObj = this.boxObj;
    setCookie(boxObj.box.id+"Height", boxObj.box_height);
  }
})



var ResizableBox = Class.create();
ResizableBox.prototype = {
  initialize: function(box, box_width, box_height, hbarFlag, vbarFlag, others) {
    var w = parseInt(getCookie(box.id+"Width"));
    if ( w ) box_width = w;
    var h = parseInt(getCookie(box.id+"Height"));
    if ( h ) box_height = h;
    if ( box_width ) box.style.width = box_width + 'px';
    if ( box_height ) box.style.height = box_height + 'px';
    var dim = Element.getDimensions(box);
    box_width = dim.width;
    box_height = dim.height;

    var parent = box.parentNode;

    if ( hbarFlag ) {
      new Insertion.After(box, '<div class="hbar" style="float:left"></div>');
      var cn = $A(parent.childNodes);
      for ( var i=0; i<cn.length; i++) {
        if ( cn[i] == box ) {
          var hbar_div = cn[i+1];
          break;
        }
      }
      hbar_div.style.height = box_height - 2 + 'px';
      var hbar = new HBar(hbar_div, {constraint: 'horizontal',
                        starteffect: null,
                        endeffect: null})
      hbar.boxObj = this;
      this.hbar = hbar;
      this.hbar_div = hbar_div;
      if ( others )
        others.each(function(value,index){
          value.style.width = box_width + 'px';
        });
      hbar.div_width = Element.getDimensions(hbar_div).width;
    }

    if ( vbarFlag ) {
      new Insertion.After(box, '<div class="vbar"></div>');
      var cn = $A(parent.childNodes);
      for ( var i=0; i<cn.length; i++) {
        if ( cn[i] == box ) {
          var vbar_div = cn[i+1];
          break;
        }
      }
      vbar_div.style.width = box_width - 2 + 'px';
      //this.vbar_offset = -2;
      //vbar_div.style.top = this.vbar_offset + 'px';
      var vbar = new VBar(vbar_div, {constraint: 'vertical',
                        starteffect: null,
                        endeffect: null})
      vbar_div.style.position = 'static';
      vbar.boxObj = this;
      this.vbar = vbar;
      this.vbar_div = vbar_div;
      if ( others )
        others.each(function(value,index){
          value.style.height = box_height - 2 + 'px';
        });
      vbar.div_height = Element.getDimensions(vbar_div).height;
    }

    this.box = box;
    if(box.style.width || others) this.box_width = box_width;
    if(box.style.height || others) this.box_height = box_height;
    this.others = others;
    this.min_width = 10;
    this.min_height = 30;
  },
  setWidth: function(width, flag) {
    if ( !flag ) {
      var min_width = this.min_width;
      if ( this.hbar ) min_width = min_width + this.hbar.div_width;
      if ( width < min_width ) width = min_width;
    }
    this.box.style.width =  width + 'px';
    if ( this.others )
      this.others.each(function(value,index){
        value.style.width = width + 'px';
      });
    this.box_width = width;
  },
  setHeight: function(height, flag) {
    var box = this.box;
    if ( !flag ) {
      var min_height = this.min_height;
      //      if ( this.vbar ) min_height = min_height + this.vbar.div_height;
      if ( height < min_height ) height = min_height;
    }
    this.box.style.height = height + 'px';
    if ( this.others )
      this.others.each(function(value,index){
        value.style.height = height + 'px';
      });
    this.box_height = height;
  },
  resetSize: function() {
    resetWidth();
    resetHeight();
  },
  resetWidth: function() {
    setCookie(this.box.id+"Width", null);
    this.box.style.width = null;
    this.box_width = null; /* new value will be set when dragged */
  },
  resetHeight: function() {
    setCookie(this.box.id+"Height", null);
    this.box.style.height = null;
    this.box_height = null; /* new value will be set when dragged */
    }
}

