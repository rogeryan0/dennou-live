var Tab = Class.create();
Tab.prototype = {
  initialize: function() {
    this.tab_list = new Array();
    this.body_list = new Array();
    this.action_list = new Array();
  },

  select: function(num) {
    this.tab_list.each( function(val,i){val.className = "tab_close";} );
    this.tab_list[num].className = "tab_open";
    this.body_list.each( function(val,i){Element.hide(val);} );
    Element.show(this.body_list[num]);
    if( this.action_list[num] )
      eval(this.action_list[num]);
  }
}
