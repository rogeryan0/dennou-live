function pdMenu(mName){
    Menu = document.getElementById(mName).style;
    Menu.left = Event.pointerX + "px";
    Menu.top  = Event.pointerY + "px";
    Menu.visibility = "visible";
}
function Hidden(mName){
    Menu.visibility = "hidden";
}
function pdMenu2(mName) {
    Menu2 = document.getElementById(mName).style;
    Menu2.left = Event.clientX + window.pageXOffset + "px";
    Menu2.top  = Event.clientY + window.pageYOffset + "px";
    Menu2.visibility = "visible";
}
function Hidden2(){
    Menu2.visibility = "hidden";
}
function showLoadingIconAll(){
    //var key=document.getElementByID('keyword');
    //key.innerText="";
    //var res=document.getElementByID('results');
    //res.innerText="";
    var areas=document.getElementsByName('loading');
    for(i=0;i<areas.length;i++){
        areas[i].style.display="block";
    }
}
function hideLoadingIconAll(){
    var areas=document.getElementsByName('loading');
    for(i=0;i<areas.length;i++){
        areas[i].style.display="none";
    }
}
