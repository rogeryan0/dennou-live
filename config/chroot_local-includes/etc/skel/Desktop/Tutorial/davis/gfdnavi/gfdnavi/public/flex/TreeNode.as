package
{

  import flash.events.Event;
  import flash.net.URLRequest;
  import flash.net.navigateToURL;

  import mx.collections.ArrayCollection;
  import mx.controls.Alert;
  import mx.controls.Tree;
  import mx.rpc.AsyncResponder;
  import mx.rpc.AsyncToken;
  import mx.rpc.http.mxml.HTTPService;


  public class TreeNode {

      public function TreeNode():void {
	    super();
      }

      [Embed(source='../images/tree/folder.png')]
      [Bindable]
      public static var closedFolderIcon:Class;

      [Embed(source='../images/tree/binary.png')]
      [Bindable]
      public static var binaryIcon:Class;

      [Embed(source='../images/tree/image2.png')]
      [Bindable]
      public static var imageIcon:Class;

      [Embed(source='../images/tree/knowledge_icon.png')]
      [Bindable]
      public static var knowledgeIcon:Class;

      [Embed(source='../images/tree/anal_viz.png')]
      [Bindable]
      public static var analysisIcon:Class;

      [Embed(source='../images/tree/show.png')]
      [Bindable]
      public static var showIcon:Class;


      public static var hs:HTTPService;
      public static var tree:Tree;
      public static var changeHandler:Function;
      public static var mainURI:String;


      public var host:String;
      public var has_variable:Boolean;
      public var has_image:Boolean;

      public var getting:Boolean;
      private var childrenUri:String;

      private var _chld:ArrayCollection;
      private var _dirs:ArrayCollection;
      private var _vars:ArrayCollection;
      private var _imgs:ArrayCollection;

      [Bindable]
      private var _kas:ArrayCollection;

      public var name:String;
      public var path:String;
      public var size:int;
      public var node_type:String;
      public var mtime:String;
      public var title:String;
      public var description:String;
      public var parent:Object;
      public var numDirs:int;
      public var imgSrc:String;

      public var dlUri:String;
      public var dimensions:ArrayCollection;

      [Bindable]
      public var nodeTypeIcon:Class;
      [Bindable]
      private var _nodeActionIcon:Class;

      public function toString():String { return "[Object TreeNode]"};

      public function get nodeActionIcon():Class {
      	if (childrenUri) getChildren();
      	return _nodeActionIcon;
      }
      public function set nodeActionIcon(icon:Class):void {
      	_nodeActionIcon = icon;
      }

      public function get directories():ArrayCollection {
	    getChildren();
	    return _dirs;
      }
      public function set directories(dirs:ArrayCollection):void {
	    _dirs = dirs;
      }
      public function get variables():ArrayCollection {
	    getChildren();
	    return _vars;
      }
      public function set variables(vars:ArrayCollection):void {
	    _vars = vars;
      }
      public function get images():ArrayCollection {
	    getChildren();
	    return _imgs;
      }
      public function set images(imgs:ArrayCollection):void {
	    _imgs = imgs;
      }
      public function get children():ArrayCollection {
	    getChildren();
	    return _chld;
      }
      public function set children(chld:ArrayCollection):void {
	    _chld = chld;
      }

      public function get keywordAttributes():ArrayCollection {
          if (!_kas) {
	        _kas = new ArrayCollection();
	        hs.url = host + "/data" + path + "/keyword_attributes.xml";
	        hs.method = "GET";
	        var call:AsyncToken = hs.send();
    	    call.addResponder(new AsyncResponder(addKAs, faildGetKAs, call));
	      }
	      return _kas;
	  }
	  public function set keywordAttributes(kas:ArrayCollection):void {
	  	_kas = kas;
	  }

      private function addKAs(result:Object, token:Object = null):void {
//                   trace(result.toString(), "succeded to get Node Keyword Attributes");
          var obj:Object;
	      var kas:Object;
	      if ( result.result.hasOwnProperty("keyword-attributes") ) {
             kas = result.result["keyword-attributes"];
		     if (kas) {
			   kas = kas["keyword-attribute"];
			   if (!kas.length) kas = new Array(kas);
			   for each (var ka:Object in kas) {
			     obj = new Object();
			     obj.name = ka.name;
			     obj.value = ka.value;
			     obj.lang = ka.lang;
			     _kas.addItem(obj);
			   }
		     }
		   }
      }
      private function faildGetKAs(error:Object, token:Object = null):void {
//                    trace(error.toString());
           Alert.show("failed to get Node Keyword Attributes","Error");
           _kas = null;
	  }

      private function getChildren():void {
	    if (!getting && !_chld) {
	      getting = true;
	      getNodes(childrenUri, this);
	    }
      }

      public static function getNodes(uri:String, parent:Object):ArrayCollection {
            var host:String = parent.host;
	    var dirs:ArrayCollection = new ArrayCollection();
	    var vars:ArrayCollection;
	    var imgs:ArrayCollection;
	    var chld:ArrayCollection;
	    if (parent is TreeNode) {
	    	vars = new ArrayCollection();
	    	imgs = new ArrayCollection();
	    	chld = new ArrayCollection();
   	        parent.directories = dirs;
	        parent.variables = vars;
	        parent.images = imgs;
	        parent.children = chld;
	    }
	    hs.url = uri + "?num_dirs=true";
	    hs.method = "GET";    
	    var call:AsyncToken = hs.send();
	    call.addResponder(new AsyncResponder(
	    	function(result:Object, token:Object = null):void {
//	    		trace(result.toString(), "succeded to get Nodes");
         		var obj:TreeNode;
		 	var nodes:Object = result.result.nodes.node;
        		if (!nodes.length) nodes = new Array(nodes);
			for each (var node:Object in nodes) {
		 	   obj = new TreeNode();
		 	   obj.host = host;
		           obj.name = node.name;
		 	   obj.numDirs = node.num_dirs;
		 	   obj.path = node.path;
		   	   obj.size = node.size;
		   	   obj.node_type = node.node_type;
		  	   obj.mtime = node.mtime;
		   	   if ((node.title is String) && (node.title!="NULL")) obj.title = node.title;
		   	   if ((node.description is String ) && (node.description!="NULL")) obj.description = node.description;
          		   obj.imgSrc = node.img_src;
         		   obj.dlUri = node.dl_uri;
		  	   if (parent is TreeNode) obj.parent = parent;
		   	   if (node.node_type == "directory") {
		   	     obj.childrenUri = node.children.uri;
		   	     obj.nodeTypeIcon = closedFolderIcon;
		   	     dirs.addItem(obj);
      	  		   } else if (node.node_type == "variable") {
      	  		     obj.nodeTypeIcon = binaryIcon;
      	  		     obj.nodeActionIcon = analysisIcon;
           		     if (vars) vars.addItem(obj);
		     	   } else if (node.node_type == "image") {
		     	     obj.nodeTypeIcon = imageIcon;
		     	     // obj.nodeActionIcon = showIcon;
        		      if (imgs) imgs.addItem(obj);
                           } else if (node.node_type == "knowledge") {
                   	      obj.nodeTypeIcon = knowledgeIcon;
                           } else {
                   	      obj.nodeTypeIcon = closedFolderIcon;
          		   }
	  		   if (chld) chld.addItem(obj);
  	 		   obj.addEventListener(Event.SELECT,addNodesToAnalysis);
 		           changeHandler(parent);
		           if (parent is TreeNode) {
                             if (vars.length > 0) parent.nodeActionIcon = analysisIcon;
		             parent.getting = false;
		          }
                     }
	        },
	        function(error:Object, token:Object = null):void {
                  Alert.show("faild to list Nodes: "+uri,"error");
                  trace(error.toString());
 		  if (parent is TreeNode) {
		     parent.getting = false;
		     parent.children = null;
		  }
	        },
	        call
	    ));
	    return dirs;
      }


      public static function getNode(uri:String,func:Function,option:String=null):TreeNode {
            uri = uri+".xml";
            if (option) uri = uri + "?" + option;
	    hs.url = uri;
	    hs.method = "GET";
 	    var node:TreeNode = new TreeNode();
	    var call:AsyncToken = hs.send();
	    call.addResponder(new AsyncResponder(
	    	function(result:Object, token:Object = null):void {
//	    		trace(result.toString(), "succeded to get Nodes");
                        var obj:Object;
                        if (result.result.directory) {
                          obj = result.result.directory;
                        } else if (result.result.variable) {
                          obj = result.result.variable;
                        } else if (result.result.image) {
                          obj = result.result.image;
                        } else if (result.result.knowledge) {
                          obj = result.result.knowledge;
                        } else if (result.result["draw-method"]) {
                          obj = result.result["draw-method"];
                        } else if (result.result["function"]) {
                          obj = result.result["function"];
                        }
		 	node.host = obj["gfdnavi_root"];
                        node.name = obj.name;
		 	node.path = obj.path;
		   	node.size = obj.size;
		   	node.node_type = obj.node_type;
		  	node.mtime = obj.mtime;
		   	if ((obj.title is String) && (obj.title!="NULL")) node.title = obj.title;
		   	if ((obj.description is String ) && (obj.description!="NULL")) node.description = obj.description;
          		node.imgSrc = obj.img_src;
         		node.dlUri = obj.dl_uri;
                        if (node.node_type=="directory") node.childrenUri = obj.children.uri;
                        if (node.node_type=="variable" && obj.dimensions.dimension) {
                           var dims:Object = obj.dimensions.dimension;
                           if (!dims.length) dims = new Array(dims);
                           var ary:Array = new Array();
                           for each (var dim:Object in dims)
                             ary.push({name: dim.name, max:dim.max, min:dim.min, units:dim.units})
			   node.dimensions = new ArrayCollection(ary);
			}
                        func(node);
	        },
	        function(error:Object, token:Object = null):void {
                  Alert.show("faild to list Nodes: "+uri,"error");
                  trace(error.toString());
	        },
	        call
	    ));
	    return node;
      }

      private static function addNodesToAnalysis(event:Event):void {
      	var node:TreeNode = event.currentTarget as TreeNode;
        if (node.node_type == "directory") {
          for each (var chld:TreeNode in node.variables)
            chld.dispatchEvent(new Event(Event.SELECT));
        } else {
            navigateToURL(new URLRequest("http://"+mainURI+"/flex/Analysis.swf?path="+node.host+"/data"+node.path), "_self");
        }
      }
   }
}
