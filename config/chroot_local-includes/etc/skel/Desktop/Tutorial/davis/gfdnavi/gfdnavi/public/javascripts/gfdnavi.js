var gfdnaviUrlRoot;

function setCookie(name,value,expire){
    if (value) {
	document.cookie = name + '=' + escape(value) + ((expire==null)?'':('; expires='+expire.toGMTString()));
    } else {
	expire = new Date();
        var year = expire.getYear();
	year = year < 1900 ? year + 1900 : year;
	expire.setYear(year - 1);
	document.cookie = name + '=null; expires='+expire.toGMTString();
    }
}

function getCookie(name){
  var search = name + '=';
  if (document.cookie.length>0) {
    var offset = document.cookie.indexOf(search);
    if (offset != -1){
      offset += search.length;
      var end = document.cookie.indexOf(';',offset);
      if(end == -1)
        end = document.cookie.length;
      return unescape(document.cookie.substring(offset,end));
    }
  }
  return null;
}

var PopupMenu = Class.create();
PopupMenu.prototype = {
  initialize: function(element, name, html, title) {
    this.html = html;
    this.title = title;
    this.name = name;
    if (element)
      Event.observe(element, "click", this.openMenu.bindAsEventListener(this));
  },
  openMenu: function(e) {
    //var date = new Date();
    //date = date.getTime();
    var id = 'popup_menu_'+this.name;//date;
    var hid = 'popup_header_'+this.name;//date;
    var menu = $(id);
    if (menu != null) {
      menu.style.left = Event.pointerX(e)-5 + 'px';
      menu.style.top = Event.pointerY(e)-5 + 'px';
      return;
    }
    var html = '<div id="'+id+'" class="popup_menu" style="display:none">'
          + '<table cellspacing="0" cellpadding="0" border="1" bordercolor="black">'
          + '<tr id="'+hid+'" border="0" style="background-color:#ddeeff;cursor:move;">'
          + '<td>'
          + '<div align="left" style="float:left">';
    if (this.title)
      html += this.title;
    html += '</div>'
          + '<div align="right"><img src="' + gfdnaviUrlRoot + '/images/x.gif" alt="X" class="delete" onClick="Element.remove($(\''+id+'\'));"/></div>'
          + '</td></tr>'
          + '<tr><td>'
          + '<div style="padding:5px">' + this.html  + '</div>'
          + '</td></tr>'
          + '</table>'
          + '</div>';

    new Insertion.Bottom(Element.extend(document.body), html);
    menu = $(id);
    new Draggable(menu,{handle: hid});
    if (e) {
      menu.style.left = Event.pointerX(e)-5 + 'px';
      menu.style.top = Event.pointerY(e)-5 + 'px';
    } else {
      menu.style.left = '10px';
      menu.style.top = '10px';
    }
    menu.style.zIndex = '10';
    Element.show(menu);
    if (e)
      Event.stop(e);
    this.id = id;
  },
  closeMenu: function(e) {
    Element.remove(this.id);
  }
};

var BenchMark = Class.create();
BenchMark.prototype = {
  initialize: function (){},
  start: function() {
    this.start_date = new Date();
  },
  set: function(ary, body) {
    this.ids = ary;
    this.body = body;
  },
  load: function(id) {
    if (this.ids) {
      this.ids = this.ids.without(id);
      if (this.ids.length == 0)
        this.complete();
    }
  },
  complete: function() {
    if ( this.start_date ) {
      var html = this.body + '<br/><hr/>';
      var now = new Date();
      var msec = now.getTime() - this.start_date.getTime();
      html += parseInt(msec/60000)+':'+parseInt((msec%60000)/1000)+'.'+parseInt(msec%1000);
      var pop = new PopupMenu(null, html, 'Result of Benchmark');
      pop.openMenu();
      this.start_date = null;
      this.ids = null;
      this.body = null;
    }
  }
}
var benchMark = new BenchMark();


var normalBgColor = "transparent";
var highlightBgColor = "gray";
function bgUnhighlight(obj) { obj.style.backgroundColor=normalBgColor; }
function bgHighlight(obj) { obj.style.backgroundColor=highlightBgColor; }

var html_hidden = " <font color='red'><b>(hidden currently)</b></font> ";
var temp_save_str = "";

function helpLinkHilit(id, self) {
  var obj = $(id);
  if(obj && testElementVisible(obj)){
    obj.style.backgroundColor=highlightBgColor;
  } else {
    temp_save_str = self.innerHTML;
    self.innerHTML = temp_save_str + html_hidden;
  }
}
function helpLinkUnhilit(id, self) {
  var obj = $(id);
  if(obj && testElementVisible(obj)){
    obj.style.backgroundColor=normalBgColor;
  } else {
    self.innerHTML = temp_save_str;
  }
}

function testElementVisible(obj) {
  var elm = obj;
  while (elm) {
    if (elm == document)
      return true;
    if (!Element.visible(elm))
      return false;
    elm = elm.parentNode;
  }
}

function bgFlash(id){
   new Effect.Highlight(id,
          {startcolor:"#ff0000",endcolor:"#ffffff",restorecolor:"#ffffff"});
}

//copy of knowledge.js
function showCommentListShort () {
    var comment_list_short = document.getElementById("comment_list_short");
    var comment_list_short_button = document.getElementById("comment_list_short_button");
    var comment_list_hide = document.getElementById("comment_list_hide");
    var comment_list_hide_button = document.getElementById("comment_list_hide_button");
    var comment_list_all = document.getElementById("comment_list_all");
    var comment_list_all_button = document.getElementById("comment_list_all_button");
    
    comment_list_short.style.display = "block";
    comment_list_short_button.style.borderStyle = "solid";
    comment_list_hide.style.display = "none";
    comment_list_hide_button.style.borderStyle = "none";
    comment_list_all.style.display = "none";
    comment_list_all_button.style.borderStyle = "none";
}

function showCommentListHide () {
    var comment_list_short = document.getElementById("comment_list_short");
    var comment_list_short_button = document.getElementById("comment_list_short_button");
    var comment_list_hide = document.getElementById("comment_list_hide");
    var comment_list_hide_button = document.getElementById("comment_list_hide_button");
    var comment_list_all = document.getElementById("comment_list_all");
    var comment_list_all_button = document.getElementById("comment_list_all_button");
    
    comment_list_short.style.display = "none";
    comment_list_short_button.style.borderStyle = "none";
    comment_list_hide.style.display = "block";
    comment_list_hide_button.style.borderStyle = "solid";
    comment_list_all.style.display = "none";
    comment_list_all_button.style.borderStyle = "none";
}

function showCommentListAll () {
    var comment_list_short = document.getElementById("comment_list_short");
    var comment_list_short_button = document.getElementById("comment_list_short_button");
    var comment_list_hide = document.getElementById("comment_list_hide");
    var comment_list_hide_button = document.getElementById("comment_list_hide_button");
    var comment_list_all = document.getElementById("comment_list_all");
    var comment_list_all_button = document.getElementById("comment_list_all_button");
      
    comment_list_short.style.display = "none";
    comment_list_short_button.style.borderStyle = "none";
    comment_list_hide.style.display = "none";
    comment_list_hide_button.style.borderStyle = "none";
    comment_list_all.style.display = "block";
    comment_list_all_button.style.borderStyle = "solid";
}

function enableButton (button_id) {
    var button = document.getElementById(button_id);
    button.style.display = "block";
}
