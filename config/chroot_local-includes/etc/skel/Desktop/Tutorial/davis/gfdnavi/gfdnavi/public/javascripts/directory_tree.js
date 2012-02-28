var tree;
var Tree = Class.create();
Tree.imagefile = new Array(gfdnaviUrlRoot+"/images/tree/plus.gif",
			 gfdnaviUrlRoot+"/images/tree/minus.gif");
Tree.prototype = {
    initialize: function(divId) {
	this.divId = divId;
	this.showDirDetailsAjax = null;
	var imagefile = Tree.imagefile;
	this.image = new Array(imagefile.length);
	for (var count = 0; count < (imagefile.length - 1); count++){
		this.image[count] = new Image();
		this.image[count].src = imagefile[count];
	}
    },

    create: function(path) {
	var param = {asynchronous: false,
		     method: 'get',
		     evalScripts: true};
	if (path)
	    param.parameters = 'path='+path;
	new Ajax.Updater(this.divId,
			 gfdnaviUrlRoot + '/finder/create_tree',
			 param
			 );
    },

    open: function(path) {
	new Ajax.Request(gfdnaviUrlRoot + '/finder/open_tree',
                         {asynchronous: true,
			  evalScripts: true,
			  parameters: 'path=' + path,
			  method: 'post'});
    },

    selectedFunction: null,
    previousSelectedPath: null,

    dirSelected: function(path, notopen) {
	if (!notopen)
	    this.dirShow(path,true);
	this.selectedFunction(path);
	if (this.previousSelectedPath)
	    Element.removeClassName('dir_name' + this.previousSelectedPath,'selected');
	Element.addClassName('dir_name' + path, 'selected');
	this.previousSelectedPath = path;
    },

    dirChange: function(path) {
	var dir_id = 'dir'+path;
	var img_id = 'dir_img'+path;
	if( Element.visible(dir_id) ) {
	    this.dirHide(path);
	} else {
	    this.dirShow(path);
	}
    },

    dirShow: function(path, selected) {
	var dir_id = 'dir'+path;
	var img_id = 'dir_img'+path;
	$(img_id).src = Tree.imagefile[1];
	Element.show(dir_id);
	this.sendStatus('opened', path, selected);
    },

    dirHide: function(path) {
	var dir_id = 'dir'+path;
	var img_id = 'dir_img'+path;
	$(img_id).src = Tree.imagefile[0];
	Element.hide(dir_id);
	this.sendStatus('closed', path);
    },

    sendStatus: function(status, path, selected) {
	param = status + '=' + path;
	if (selected)
	    param = param + '&selected=true';
	new Ajax.Request(gfdnaviUrlRoot + '/finder/dir_status',
                         {asynchronous: true,
			  evalScripts: true,
			  parameters: param,
			  method: 'post'});
    },


    dirDetailsDivId: null,

    showDirDetails: function(path) {
	if (this.showDirDetailsAjax)
	    this.showDirDetailsAjax.transport.abort();
	param = { method: 'get',
		  evalScripts: true};
	if (path)
	    param.parameters = 'path='+path;
	this.showDirDetailsAjax = new Ajax.Updater(this.dirDetailsDivId,
				      gfdnaviUrlRoot + '/finder/show_details',
				      param
		                      );
    }

};
