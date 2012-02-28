dial_image = gfdnaviUrlRoot + "/images/slider_f.gif";
dial_red_image = gfdnaviUrlRoot + "/images/slider_f_red.gif";

var Dial = Class.create();
Object.extend(Dial.prototype, Draggable.prototype);
Object.extend(Dial.prototype, {
  updateDrag: function(event, pointer) {
    var offset = this.offset[0];
    offset += Position.cumulativeOffset(this.element)[0];
    offset -= this.currentDelta()[0];
    offset -= this.relative_offset;
    var pos = pointer[0] - offset;
    var index = this.slider.posToIndex(pos, this.min, this.max);
    var ary = this.slider.indexToValAndPos(index);
    if (this.text)
      this.text.value = ary[0];
    this.index = index;
    pos = ary[1];
    pointer[0] = pos + offset;
    if(this.set_bound)
      this.set_bound(index);
    if(this.come_up)
      this.come_up();
    Draggable.prototype.updateDrag.apply(this, arguments);
  },
  endDrag: function(event, pointer) {
    if (this.handler) this.handler(event);
    Draggable.prototype.endDrag.apply(this, arguments);
  }
});

var Slider = Class.create();
Slider.prototype = {
  initialize: function(name, width, text_size, ary, make_right, left_default, right_default) {

    var element = $(name);

    var left_obj = new Object();
    this.left_obj = left_obj;

    left_obj.text_id = name+"_left_text";
    left_obj.dial_id = name+"_left_dial";
    var right_obj;
    if(make_right) {
      right_obj = new Object();
      this.right_obj = right_obj;
      right_obj.text_id = name+"_right_text";
      right_obj.dial_id = name+"_right_dial";
    }
    var free_obj = new Object();
    this.free_obj = free_obj;
    free_obj.dial_id = name+"_free_dial";

    var back_id = name+"_back";
    var td_id = name+"_td";
    this.back_id = back_id;

    var text_var;

    var html = '<table><tr>';
    if(left_default != null)
      text_var = left_default;
    else
      text_var = ary.first();
    html += '<td><input type="text" name="' + name + '[min]" id="' + left_obj.text_id + '" size="' + text_size + '" value="' + text_var +'"/></td>';
    html += '<td id="' + td_id + '">';
    html += '<div id="' + back_id + '" style="width:' + width + 'px; height:' + 0 + 'px;"><hr style="position:relative;top:1ex"/></div>';
    html += '<span id="' + left_obj.dial_id + '" style="position:relative;"></span>';
    if(make_right)
      html += '<span id="' + right_obj.dial_id + '" style="position:relative;"></span>';
    html += '<span id="' + free_obj.dial_id + '" style="position:relative;display:none;"></span>';
    html += '</td>';
    if(make_right) {
      if(right_default != null)
        text_var = right_default;
      else
        text_var = ary.last();
      html += '<td><input type="text" name="' + name + '[max]" id="' + right_obj.text_id + '" size="' + text_size + '" value="' + text_var + '" /><td>';
    }
    html += '</tr></table>';
    element.update(html);

    var imgl = new Image();
    imgl.src = dial_image;
    imgl.alt = name+"_min";
    left_obj.dial_dom = $(left_obj.dial_id);
    left_obj.dial_dom.appendChild(imgl);
    left_obj.dial_img = imgl;
    if(make_right) {
      var imgr = new Image();
      imgr.src = dial_image;
      imgr.alt = name+"_max";
      right_obj.dial_dom = $(right_obj.dial_id);
      right_obj.dial_dom.appendChild(imgr);
      right_obj.dial_img = imgr;
    }
    var imgf = new Image();
    imgf.src = dial_red_image;
    imgf.alt = name+"_free";
    free_obj.dial_dom = $(free_obj.dial_id);
    free_obj.dial_dom.appendChild(imgf);
    free_obj.dial_img = imgf;
    free_obj.dial_dom.style.zIndex = 3;


    left_obj.text_dom = $(left_obj.text_id);
    Event.observe(left_obj.text_dom, 'change', this.moveLeftDial.bind(this));
    if(make_right) {
      right_obj.text_dom = $(right_obj.text_id);
      Event.observe(right_obj.text_dom, 'change', this.moveRightDial.bind(this));
    }

    var left_bound =  0;
    this.left_bound = left_bound;
    //var img_width =  imgl.width;
    var img_width =  11;
    this.right_bound = left_bound + width - img_width;

    this.ary = ary;

    var options = {
      constraint: 'horizontal',
      starteffect: null,
      endeffect: null
    };
    var left_dial = new Dial(left_obj.dial_dom, options);
    left_dial.min = 0;
    left_dial.max = ary.length-1;
    left_dial.text = left_obj.text_dom;
    left_dial.slider = this;
    left_dial.relative_offset = 0;
    left_dial.index = 0;
    left_dial.handler = function(event) { if (this.slider.leftHandler) this.slider.leftHandler(event); };
    left_obj.dial = left_dial;

    if(make_right) {
      var right_dial = new Dial(right_obj.dial_dom, options);
      right_dial.min = 0;
      right_dial.max = ary.length-1;
      right_dial.text = right_obj.text_dom;
      right_dial.slider = this;
      right_dial.relative_offset = img_width;
      right_dial.index = right_dial.max;
      right_dial.handler = function(event) { if (this.slider.rightHandler) this.slider.rightHandler(event); };
      right_obj.dial = right_dial;

      left_obj.dial.set_bound = function(index) {
        this.slider.right_obj.dial.min = index;
      };
      left_obj.dial.come_up = function() {
        this.slider.left_obj.dial_dom.style.zIndex = 2;
        this.slider.right_obj.dial_dom.style.zIndex = 1;
      };

      right_obj.dial.set_bound = function(index) {
        this.slider.left_obj.dial.max = index;
      };
      right_obj.dial.come_up = function() {
        this.slider.right_obj.dial_dom.style.zIndex = 2;
        this.slider.left_obj.dial_dom.style.zIndex = 1;
      };
    }
    var free_dial = new Dial(free_obj.dial_dom, options);
    free_dial.min = 0;
    free_dial.max = ary.length-1;
    free_dial.slider = this;
    free_dial.relative_offset = img_width*2;
    free_dial.index = 0;
    free_obj.dial = free_dial;


    this.moveLeftDial();
    if(this.right_obj)
      this.moveRightDial();
    this.moveFreeDial();
  },

  enableRight: function() {
    this.right_obj.text_dom.disabled = false;
    Element.setOpacity(this.right_obj.dial_img, 1.0);
  },
  disableRight: function() {
    this.right_obj.text_dom.disabled = true;
    Element.setOpacity(this.right_obj.dial_img, 0.3);
  },
  enableFree: function(){
    this.free_obj.dial_dom.show();
  },
  disableFree: function(){
    this.free_obj.dial_dom.hide();
  },

  setLeft: function(val) {
    this.left_obj.text_dom.value = val;
    this.moveLeftDial();
  },
  setRight: function(val) {
    this.right_obj.text_dom.value = val;
    this.moveRightDial();
  },
  setFree: function(val) {
    this.free_obj.dial.index = this.valToIndex(val);
    this.moveFreeDial();
  },
  getLeft: function() {
    return this.left_obj.text_dom.value;
  },
  getRight: function() {
    return this.right_obj.text_dom.value;
  },
  getFree: function() {
    return this.indexToValAndPos(this.free_obj.dial.index)[0];
  },

  moveLeftDial: function(event) {
    this.moveDial(this.left_obj);
    if (this.leftHandler) this.leftHandler(event);
  },
  moveRightDial: function(event) {
    this.moveDial(this.right_obj);
    if (this.rightHandler) this.rightHandler(event);
  },
  moveFreeDial: function() {
    this.moveDial(this.free_obj);
  },

  moveDial: function(obj) {
    var text_dom = obj.text_dom;
    var index;
    if ( text_dom ) {
      index = this.valToIndex(text_dom.value);
    } else {
      index = obj.dial.index;
    }
    var ary = this.indexToValAndPos(index);
    if(obj.dial.set_bound)
      obj.dial.set_bound(index);
    if(obj.dial.come_up)
      obj.dial.come_up();
    if (text_dom)
      text_dom.value = ary[0];
    if(ary)
      obj.dial_dom.style.left = (ary[1]-obj.dial.relative_offset)+'px';
    obj.dial.index = index;
  },

  posToIndex: function(pos, min, max) {
    var flac = (pos-this.left_bound)/(this.right_bound-this.left_bound);
    var index = Math.round(flac*(this.ary.length-1));
    if (index < min) index = min;
    if (index > max) index = max;
    return index;
  },

  valToIndex: function(val) {
    var min;
    var index;
    var diff;
    this.ary.each(function(v,i) {
      diff = Math.abs( v - val );
      if( i==0 || diff < min ) {
        min = diff;
        index = i;
      }
    });
    return index;
  },

  indexToValAndPos: function(index) {
    var val = this.ary[index];
    var pos = this.left_bound+(this.right_bound-this.left_bound)*index/(this.ary.length-1);
    return [val, pos];
  }

};
