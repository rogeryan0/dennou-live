var map;
var square;
var start;
var end;
var clicksquareevent;
var searchbutton;
var select_region;
var partial_covered = [];
var points = [];
var spconditions = [];
var mode = 0; /* 0:None, 1:Clicked start point, 2:Clicked end point*/

var marker;

window.onload = initialize;

function initialize() {
    var mapid = document.getElementById('map');
    var myOptions = {
        zoom: 1,
        center: new google.maps.LatLng(43,141),
        mapTypeId: google.maps.MapTypeId.ROADMAP,
        scaleControl: true
    };
    
    map = new google.maps.Map(mapid, myOptions);
    
    loadResults();
    
    google.maps.event.addListener(map, 'mousemove', 
                                  function(event){
                                      moveEndPoint(event.latLng);
                                  });
    google.maps.event.addListener(map, 'click', 
                                  function(event){
                                      clickMap(event.latLng);
                                  });  
}

function loadResults(){
    var res = document.getElementById("points").childNodes;
    var j = 0;
    for(var i = 0; i < res.length; i++){
      if(res[i].id != undefined){		
          var lat = parseFloat(res[i].childNodes[1].innerHTML);
          var lon = parseFloat(res[i].childNodes[3].innerHTML);
	  var cnt = res[i].childNodes[5].innerHTML;
          var imageurl = "https://chart.googleapis.com/chart?chst=d_bubble_text_small&chld=bbT|" + cnt + "|C6EF8C|000000";
	  var pt = new google.maps.LatLng(lat, lon);
	  var marker = new google.maps.Marker(
              {position: pt,   
               map:      map, 
               title:    'data',
	       icon:     imageurl});
      }    
    }
    
    res = document.getElementById("partial_covered").childNodes;
    var j = 0;
    for(i = 0; i < res.length; i++){
        if(res[i].id != undefined){		
            var stlat = parseFloat(res[i].childNodes[1].innerHTML);
            var stlon = parseFloat(res[i].childNodes[3].innerHTML);
            var edlat = parseFloat(res[i].childNodes[5].innerHTML);
            var edlon = parseFloat(res[i].childNodes[7].innerHTML);
            
            var pcover= [
  	        new google.maps.LatLng(stlat, stlon),
  	        new google.maps.LatLng(edlat, stlon),
  	        new google.maps.LatLng(edlat, (stlon + edlon)/2),  
  	        new google.maps.LatLng(edlat, edlon),  
  	        new google.maps.LatLng(stlat, edlon),
  	        new google.maps.LatLng(stlat, (stlon + edlon)/2),  
  	        new google.maps.LatLng(stlat, stlon)
            ];
            var sq =  new google.maps.Polyline(
                {path:          pcover,
		 strokeColor:   "#000000",
    		 strokeOpacity: 0.6,
    		 strokeWeight:  1});    
            sq.setMap(map);
            partial_covered.push(sq);
        }    
    }
    
    for(i = 0; i < spconditions.length; i++){
	spconditions[i].setMap(null);
    }
    
    res = document.getElementById("spconditions").childNodes;
    var j = 0;
    for(i = 0; i < res.length; i++){
        if(res[i].id != undefined){	
            var stlat = parseFloat(res[i].childNodes[1].innerHTML);
            var stlon = parseFloat(res[i].childNodes[3].innerHTML);
            var edlat = parseFloat(res[i].childNodes[5].innerHTML);
            var edlon = parseFloat(res[i].childNodes[7].innerHTML);
            var pcover= [
  	        new google.maps.LatLng(stlat, stlon),
  	        new google.maps.LatLng(stlat, edlon),
  	        new google.maps.LatLng(edlat, edlon),  
  	        new google.maps.LatLng(edlat, stlon),
  	        new google.maps.LatLng(stlat, stlon)
            ];
            var sq =  new google.maps.Polygon(
                {paths:         pcover,
		 strokeColor:   "#000000",
    		 strokeOpacity: 1,
    		 strokeWeight:  0.5,
		 fillOpacity:   0});    
            sq.setMap(map);
            partial_covered.push(sq);
            spconditions.push(sq);
        }    
    }
    
}

function clickMap(location){
    var clickedLocation = new google.maps.LatLng(location);
    if(mode == 0){
        if(square != null){
            searchbutton.setMap(null);
            square.setMap(null);
            google.maps.event.removeListener(clicksquareevent);      
        }
        start = new google.maps.LatLng(location.lat(), location.lng());
        end   = new google.maps.LatLng(location.lat(), location.lng());
        mode = 1;
        var squareCoords= [
  	    new google.maps.LatLng(start.lat(), start.lng()),
  	    new google.maps.LatLng(start.lat(), end.lng()),
  	    new google.maps.LatLng(end.lat(),   end.lng()),  
  	    new google.maps.LatLng(end.lat(),   start.lng()),
  	    new google.maps.LatLng(start.lat(), start.lng())
        ];
        
        square =  new google.maps.Polygon(
            {paths:         squareCoords,
	     strokeColor:   "#FF0000",
    	     strokeOpacity: 0.8,
    	     strokeWeight:  2,
    	     clickable:     true});    
        square.setMap(map);
        clicksquareevent = google.maps.event.addListener(square, 'click',
                                                         function(event){
                                                             clickEndPoint(event.latLng);
                                                         });        
    }
    else{
        mode = 0;
    }
}

function clickEndPoint(location){
    mode = 0;
    google.maps.event.removeListener(clicksquareevent);
    var image = gfdnaviUrlRoot + '/images/search_button.png';
    searchbutton = new google.maps.Marker(
        {position: location,
         map:      map,
         icon:     image});
    
    var stlat = start.lat();
    var stlng = start.lng();
    var edlat = end.lat();
    var edlng = end.lng();
    if(stlat > edlat){
        edlat = stlat;
        stlat = end.lat();
    }
    if(stlng > edlng){
        edlng = stlng;
        stlng = end.lng();
    }
    
    var searchurl = gfdnaviUrlRoot + "/cross/cross_space?start_lon=" + stlng + "&start_lat=" + stlat + "&end_lon=" + edlng + "&end_lat=" + edlat;
    clickbuttonevent = google.maps.event.addListener(searchbutton, 'click', 
                                                     function(event){
                                                         square.setMap(null);
                                                         searchbutton.setMap(null);
                                                         mode = 0;
                                                         new Ajax.Request(searchurl,
                                                                          { acynchronous: true,
                                                                            evalScripts: true,
                                                                            parameters: 'authenticity_token=' + encodeURIComponent('...')
                                                                          });
                                                         return false;});
}

function moveEndPoint(location){
    if(mode == 1){
        end = new google.maps.LatLng(location.lat(), location.lng());
        var squareCoords = [
  	    new google.maps.LatLng(start.lat(), start.lng()),
  	    new google.maps.LatLng(start.lat(), end.lng()),
  	    new google.maps.LatLng(end.lat(),   end.lng()),  
  	    new google.maps.LatLng(end.lat(),   start.lng()),
  	    new google.maps.LatLng(start.lat(), start.lng())
        ]; 
        square.setPath(squareCoords);	
    }
}


