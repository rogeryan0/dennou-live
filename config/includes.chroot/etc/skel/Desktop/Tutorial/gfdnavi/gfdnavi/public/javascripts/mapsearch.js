//<![CDATA[
startflg=false

// have a semi-transparent background color.
function Rectangle(bounds, opt_weight, opt_color,opt_id) {
      this.bounds_ = bounds;
      this.weight_ = opt_weight || 2;
      this.color_ = opt_color || "#ff2233";
      this.id_ = opt_id || "rectangle";
}
//Rectangle.prototype = new GOverlay();

// Creates the DIV representing this rectangle.
Rectangle.prototype.initialize = function(map) {
     // Create the DIV representing our rectangle
     var div = document.createElement("div");
     div.id=this.id_;
     div.style.border = this.weight_ + "px solid " + this.color_;
     div.style.position = "absolute";

     map.getPane(G_MAP_MAP_PANE).appendChild(div);

     this.map_ = map;
     this.div_ = div;
}

// Remove the main DIV from the map pane
Rectangle.prototype.remove = function() {
      this.div_.parentNode.removeChild(this.div_);
}

// Copy our data to a new Rectangle
Rectangle.prototype.copy = function() {
      return new Rectangle(this.bounds_, this.weight_, this.color_,
                           this.backgroundColor_, this.opacity_);
}

// Redraw the rectangle based on the current projection and zoom level
Rectangle.prototype.redraw = function(force) {
   // We only need to redraw if the coordinate system has changed
   if (!force) return;

   // Calculate the DIV coordinates of two opposite corners of our bounds to
   // get the size and position of our rectangle
   var c1 = this.map_.fromLatLngToDivPixel(this.bounds_.getSouthWest());
   var c2 = this.map_.fromLatLngToDivPixel(this.bounds_.getNorthEast());

   // Now position our DIV based on the DIV coordinates of our bounds
   this.div_.style.width = Math.abs(c2.x - c1.x) + "px";
   this.div_.style.height = Math.abs(c2.y - c1.y) + "px";
   this.div_.style.left = (Math.min(c2.x, c1.x) - this.weight_) + "px";
   this.div_.style.top = (Math.min(c2.y, c1.y) - this.weight_) + "px";
}

var map;

var start_point;
var end_point;
var current_point;
var dateline;

function eDrag() {
        GEvent.clearListeners(map,"click");
        map.enableDragging();
}

function dDrag() {
        GEvent.clearListeners(map,"mousemove");
        map.disableDragging();

        GEvent.addListener(map, "click", function(overlay, point){
          if(!startflg){
             map.clearOverlays();
             start_point=point;
                 $("query_spatial_region").checked=true;
                 checkSpace();
                 $("query_start_lon").value= point.x;
                 $("query_start_lat").value= point.y;
                 startflg=true;
              }else{
                 end_point=point;
                 $("query_end_lon").value= point.x;
                 $("query_end_lat").value= point.y;
                 startflg=false;
             var rectBounds = new GLatLngBounds(start_point,end_point);
             map.addOverlay(new Rectangle(rectBounds));
          }
        });

        GEvent.addListener(map, "mousemove", function(latlng){
          if(startflg){
            map.clearOverlays();            
            var rectBounds = new GLatLngBounds(start_point,latlng);            
            map.addOverlay(new Rectangle(rectBounds));
            current_point=latlng;
          }
          
        });
        
}

function checkSpace() {
  if(  $("query_spatial_region").checked ) {
    $("query_start_lon").disabled = false;
    $("query_start_lat").disabled = false;
    $("query_end_lon").disabled = false;
    $("query_end_lat").disabled = false;
  } else {
    $("query_start_lon").disabled = true;
    $("query_start_lat").disabled = true;
    $("query_end_lon").disabled = true;
    $("query_end_lat").disabled = true;
  }
}


function drawGroup(gid,radius,pos,dia,str,mode){
       
       if(mode=="partial"){
          var divpartial = document.createElement("div");
//          divpartial.id="group";
          divpartial.id=gid;
          divpartial.style.position = "absolute";
          divpartial.style.width = dia+"px";
          divpartial.style.height = dia+"px";
          divpartial.style.left=map.fromLatLngToDivPixel(pos).x-dia/2+"px";
          divpartial.style.top=map.fromLatLngToDivPixel(pos).y-dia+"px";
          map.getPane(G_MAP_MAP_PANE).appendChild(divpartial);
          var imgpoints = document.createElement("img");
          imgpoints.src= gfdnaviUrlRoot + "/images/kikyu_g.gif";
          
          imgpoints.style.width = dia+"px";
          divpartial.appendChild(imgpoints);

          var divpartial3 = document.createElement("div");
//          divpartial3.id="group";
          divpartial3.id=gid;
          divpartial3.style.position = "absolute";
          divpartial3.style.width = dia+"px";
          divpartial3.style.height = dia+"px";
          divpartial3.style.left=map.fromLatLngToDivPixel(pos).x-dia/4+"px";
          divpartial3.style.top=map.fromLatLngToDivPixel(pos).y-dia+"px";
          map.getPane(G_MAP_MAP_PANE).appendChild(divpartial3);
          divpartial3.style.color="green";
          divpartial3.style.fontSize=17;
          divpartial3.style.fontWeight="bold";
          var txtpoints = document.createTextNode(str);
          
          var lnk = document.createElement("a");
          lnk.href="#";
          lnk.onclick=function(){zoomGroup(pos,radius);};
          lnk.appendChild(txtpoints);
          divpartial3.appendChild(lnk);

       }else{
          var divpoints = document.createElement("div");
          divpoints.id=gid;
          divpoints.style.position = "absolute";
          divpoints.style.width = dia+"px";
          divpoints.style.height = dia+"px";
          divpoints.style.left=map.fromLatLngToDivPixel(pos).x-dia/2+"px";
          divpoints.style.top=map.fromLatLngToDivPixel(pos).y-dia+"px";
          map.getPane(G_MAP_MAP_PANE).appendChild(divpoints);
          var imgpoints = document.createElement("img");
          imgpoints.src = gfdnaviUrlRoot + "/images/kikyu_r.gif";

          imgpoints.style.width = dia+"px";
          divpoints.appendChild(imgpoints);

          var divpoints3 = document.createElement("div");
          divpoints3.id=gid;
          divpoints3.style.position = "absolute";
          divpoints3.style.width = dia+"px";
          divpoints3.style.height = dia+"px";
          divpoints3.style.left=map.fromLatLngToDivPixel(pos).x-dia/4+"px";
          divpoints3.style.top=map.fromLatLngToDivPixel(pos).y-dia+"px";
          map.getPane(G_MAP_MAP_PANE).appendChild(divpoints3);
          divpoints3.style.color="blue";
          divpoints3.style.fontSize=17;
          divpoints3.style.fontWeight="bold";
          var txtpoints = document.createTextNode(str);
          var lnk = document.createElement("a");
          lnk.href="#";
          lnk.onclick=function(){zoomGroup(pos,radius);};
          lnk.appendChild(txtpoints);
          divpoints3.appendChild(lnk);

       }
       eDrag();
}

function zoomGroup_pos(posx,posy,radius){
  var pos=new GLatLng(posx,posy);
  zoomGroup(pos,radius);
}

function execute(){
  new Ajax.Request(gfdnaviUrlRoot + "/search/remapsearch",
                   {method:"get",
                    parameters:Form.serialize("mapsearch")
                   });
}

function zoomGroup(pos,radius){
   var objbnds=new GLatLngBounds(new GLatLng(pos.lat()-radius,pos.lng()-radius),new GLatLng(pos.lat()+radius,pos.lng()+radius));
   var oldzm = map.getZoom();
   var zm = map.getBoundsZoomLevel(objbnds);
   if(zm==0){zm=oldzm+1;}
      map.setCenter(pos,zm);
         var cObj = map.getCenter();
         document.mapsearch.query_center_x.value=cObj.x;
         document.mapsearch.query_center_y.value=cObj.y;
//         document.mapsearch.query_zoomlevel.value=17-zm;
         document.mapsearch.query_zoomlevel.value=zm;
         document.mapsearch.query_span.value=map.getBounds().toSpan().lat();
         document.mapsearch.query_window_start_lat.value=map.getBounds().getSouthWest().lat();
         document.mapsearch.query_window_start_lon.value=map.getBounds().getSouthWest().lng();
         document.mapsearch.query_window_end_lat.value=map.getBounds().getNorthEast().lat();
         document.mapsearch.query_window_end_lon.value=map.getBounds().getNorthEast().lng();
	execute();
}

function focusGroup_pos(posx,posy,gid){
  var pos=new GLatLng(posx,posy);
  focusGroup(pos,gid);
}

function focusGroup(pos,gid){
   //map.setCenter(pos);
   map.openInfoWindow(pos,document.createTextNode("Here!"))                 
}

function NarrowDownByTime(){
   var i;
   var j;
   var start ="2004:12:02:10:10";
   var end ="2005:06:23:12:10";
   document.mapsearch.query_time_region.checked=true;
   var children = $("query_starttime").childNodes;
    for(i=0; i<children.length; i++)
      if( children[i].tagName == "SELECT" )
        children[i].disabled = false;
   children = $("query_endtime").childNodes;
    for(i=0; i<children.length; i++)
      if( children[i].tagName == "SELECT" )
        children[i].disabled = false;
        
   xx = start.match(/\d+/g);
   for(j=0;j<xx.length;j++){
     var eno=j+1;
     select = document.getElementsByName("query[starttime("+eno+"i)]")[0];
     for(i=0;i<select.length;i++){
       if(select.options[i].text==xx[j]){
         select.selectedIndex=i;     
       }
     }
   }

   xx = end.match(/\d+/g);
   var i;
   var j;
   for(j=0;j<xx.length;j++){
     var eno=j+1;
     select = document.getElementsByName("query[endtime("+eno+"i)]")[0];
     for(i=0;i<select.length;i++){
       if(select.options[i].text==xx[j]){
         select.selectedIndex=i;     
       }
     }
   }
   document.mapsearch.query_window_start_lat.value=map.getBounds().getSouthWest().lat();
   document.mapsearch.query_window_start_lon.value=map.getBounds().getSouthWest().lng();
   document.mapsearch.query_window_end_lat.value=map.getBounds().getNorthEast().lat();
   document.mapsearch.query_window_end_lon.value=map.getBounds().getNorthEast().lng();
}

function Zoom(){
       GEvent.addListener(map, "zoom", function(oldZoomLevel, newZoomLevel){
         var cObj = map.getCenter();
         document.mapsearch.query_center_x.value=cObj.x;
         document.mapsearch.query_center_y.value=cObj.y;
         document.mapsearch.query_zoomlevel.value=17-newZoomLevel;
         document.mapsearch.query_window_start_lat.value=map.getBounds().getSouthWest().lat();
         document.mapsearch.query_window_start_lon.value=map.getBounds().getSouthWest().lng();
         document.mapsearch.query_window_end_lat.value=map.getBounds().getNorthEast().lat();
         document.mapsearch.query_window_end_lon.value=map.getBounds().getNorthEast().lng();
        });
}

function checkTime() {
  var children;
  var i;
  if( $("query_time_region").checked ) {
    children = $("query_starttime").childNodes;
    for(i=0; i<children.length; i++)
      if( children[i].tagName == "SELECT" )
        children[i].disabled = false;
    children = $("query_endtime").childNodes;
    for(i=0; i<children.length; i++)
      if( children[i].tagName == "SELECT" )
        children[i].disabled = false;
  } else {
    children = $("query_starttime").childNodes;
    for(i=0; i<children.length; i++)
      if( children[i].tagName == "SELECT" )
        children[i].disabled = true;
    children = $("query_endtime").childNodes;
    for(i=0; i<children.length; i++)
      if( children[i].tagName == "SELECT" )
        children[i].disabled = true;
  }
}
//]]>
