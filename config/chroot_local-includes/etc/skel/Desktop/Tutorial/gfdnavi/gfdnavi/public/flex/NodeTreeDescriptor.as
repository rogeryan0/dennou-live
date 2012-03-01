package
{
  import mx.controls.treeClasses.DefaultDataDescriptor;
  import mx.collections.ICollectionView;
  import mx.collections.ArrayCollection;
  import mx.controls.Alert;
  import mx.rpc.AsyncResponder;
  import mx.rpc.AsyncToken;
  import mx.rpc.http.mxml.HTTPService;


  public class NodeTreeDescriptor extends DefaultDataDescriptor
  {
    public function NodeTreeDescriptor():void
    {
      super();
    }

    public function toString():String { return "[Object NodeTreeDescriptor]"};

    public override function getChildren(node:Object, model:Object=null):ICollectionView {
      if (node.directories) {
         return node.directories;
      } else {
         return node.children;
      }
    }

    public override function isBranch(node:Object, model:Object=null):Boolean {
       if (node.numDirs > 0) {
          return true;
       } else {
          return false;
       }

    }

    public override function hasChildren(node:Object, model:Object=null):Boolean {
       var children:ArrayCollection;

       if (node.directories) {
          children = node.directories;
       } else {
          children = node.children;
       }
       if (children.length > 0) {
          return true;
       } else {
          return false
       }
    }

  }
}
