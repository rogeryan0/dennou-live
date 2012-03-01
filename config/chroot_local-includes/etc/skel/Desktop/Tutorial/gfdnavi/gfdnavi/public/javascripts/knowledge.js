var num_of_figure;
function setNumOfFigure(x) {
    num_of_figure = x;
}

function swapFigure(figure_number) {
    var temp;

  //knowledge_figure
    // figure_caption
    temp = document.getElementById("knowledge_figure_"+ figure_number +"_figure_caption").value;
    document.getElementById("knowledge_figure_"+ figure_number +"_figure_caption").value = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_figure_caption").value;
    document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_figure_caption").value = temp;

    //file_name and figure_path
    //new_from_analysis   => 見えているもの(displayed_textbox): name        隠れているもの(hidden_textbox): figure_path
    //それ以外+MoreFigure => 見えているもの(displayed_textbox): figure_path 隠れているもの(hidden_textbox): name
    //
    //displayed_textbox, hidden_textbox 共に
    //id で get して、 name と value だけを入れ替える

    //display name
    split1 = document.getElementById("knowledge_figure_"+ figure_number +"_displayed_textbox").name.split("]");
    split2 = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_displayed_textbox").name.split("]");
    document.getElementById("knowledge_figure_"+ figure_number +"_displayed_textbox").name = split1[0] + "]" + split2[1] + "]";
    document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_displayed_textbox").name = split2[0] + "]" + split1[1] + "]";
    //display value
    temp = document.getElementById("knowledge_figure_"+ figure_number +"_displayed_textbox").value;
    document.getElementById("knowledge_figure_"+ figure_number +"_displayed_textbox").value = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_displayed_textbox").value;
    document.getElementById("knowledge_figure_"+ (figure_number + 1 ) +"_displayed_textbox").value = temp;
    //hidden name    
    split1 = document.getElementById("knowledge_figure_"+ figure_number +"_hidden_textbox").name.split("]");
    split2 = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_hidden_textbox").name.split("]");
    document.getElementById("knowledge_figure_"+ figure_number +"_hidden_textbox").name = split1[0] + "]" + split2[1] + "]";
    document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_hidden_textbox").name = split2[0] + "]" + split1[1] + "]";
    //hidden value
    temp = document.getElementById("knowledge_figure_"+ figure_number +"_hidden_textbox").value;
    document.getElementById("knowledge_figure_"+ figure_number +"_hidden_textbox").value = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_hidden_textbox").value;
    document.getElementById("knowledge_figure_"+ (figure_number + 1 ) +"_hidden_textbox").value = temp;
    
    // label
    temp = document.getElementById("label_"+ figure_number).firstChild.nodeValue;
    document.getElementById("label_"+ figure_number).firstChild.nodeValue = document.getElementById("label_"+ (figure_number + 1)).firstChild.nodeValue;
    document.getElementById("label_"+ (figure_number+1)).firstChild.nodeValue = temp;

    // png
    temp = document.getElementById("png_"+ figure_number).firstChild.nodeValue;
    document.getElementById("png_"+ figure_number).firstChild.nodeValue = document.getElementById("png_"+ (figure_number + 1)).firstChild.nodeValue;
    document.getElementById("png_"+ (figure_number+1)).firstChild.nodeValue = temp;

  //diagram_id (もう使ってないはず)
    temp = document.getElementById("knowledge_figure_"+ figure_number +"_diagram_id").value;
    document.getElementById("knowledge_figure_"+ figure_number +"_diagram_id").value = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_diagram_id").value;
    document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_diagram_id").value = temp;

  //diagram_number (diagram_cache用) : もう使ってないのでコメントアウトした。
    //    temp = document.getElementById("knowledge_figure_"+ figure_number +"_diagram_number").value;
    //    document.getElementById("knowledge_figure_"+ figure_number +"_diagram_number").value = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_diagram_number").value;
    //    document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_diagram_number").value = temp;

  //image
    temp = document.getElementById("img_"+ figure_number).src;
    document.getElementById("img_"+ figure_number).src = document.getElementById("img_"+ (figure_number + 1)).src;
    document.getElementById("img_"+ (figure_number + 1)).src = temp;

  //link to image
    temp = document.getElementById("link_to_img_"+ figure_number).href;
    document.getElementById("link_to_img_"+ figure_number).href = document.getElementById("link_to_img_"+ (figure_number + 1)).href;
    document.getElementById("link_to_img_"+ (figure_number + 1)).href = temp;

    temp = document.getElementById("link_to_img_"+ figure_number).firstChild.nodeValue;
    document.getElementById("link_to_img_"+ figure_number).firstChild.nodeValue = document.getElementById("link_to_img_"+ (figure_number + 1)).firstChild.nodeValue;
    document.getElementById("link_to_img_"+ (figure_number + 1)).firstChild.nodeValue = temp;

  //"view this image in the original size"
    temp = document.getElementById("view_image_"+ figure_number).firstChild.nodeValue;
    document.getElementById("view_image_"+ figure_number).firstChild.nodeValue = document.getElementById("view_image_"+ (figure_number + 1)).firstChild.nodeValue;
    document.getElementById("view_image_"+ (figure_number+1)).firstChild.nodeValue = temp;

  //link of "view this image in the original size"
    temp = document.getElementById("link_to_view_img_"+ figure_number).href;
    document.getElementById("link_to_view_img_"+ figure_number).href = document.getElementById("link_to_view_img_"+ (figure_number + 1)).href;
    document.getElementById("link_to_view_img_"+ (figure_number + 1)).href = temp;

    temp = document.getElementById("link_to_view_img_"+ figure_number).firstChild.nodeValue;
    document.getElementById("link_to_view_img_"+ figure_number).firstChild.nodeValue = document.getElementById("link_to_view_img_"+ (figure_number + 1)).firstChild.nodeValue;
    document.getElementById("link_to_view_img_"+ (figure_number + 1)).firstChild.nodeValue = temp;


}

function deleteFigure(figure_number) {
    // figure_number は消したい画像の番号
    // それより後ろの画像は一つずつ前に移動させる。

    if (window.confirm("Are you sure?")){
	// 全てのfigureに対してキャプション、パス、画像、リンクを一つ前に動かす。
	for (var i = figure_number; i < num_of_figure; i++) {
	    // caption
	    document.getElementById("knowledge_figure_"+ i +"_figure_caption").value = document.getElementById("knowledge_figure_"+ (i+1) +"_figure_caption").value;
	    // figure_path, name
	    //   display value
	    document.getElementById("knowledge_figure_"+ i +"_displayed_textbox").value = document.getElementById("knowledge_figure_"+ (i+1) +"_displayed_textbox").value;
	    //   display name
	    split1 = document.getElementById("knowledge_figure_"+ figure_number +"_displayed_textbox").name.split("]");
	    split2 = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_displayed_textbox").name.split("]");
	    document.getElementById("knowledge_figure_"+ figure_number +"_displayed_textbox").name = split1[0] + "]" + split2[1] + "]";

	    //   hidden value
	    document.getElementById("knowledge_figure_"+ i +"_hidden_textbox").value = document.getElementById("knowledge_figure_"+ (i+1) +"_hidden_textbox").value;
	    //   hidden name
	    split1 = document.getElementById("knowledge_figure_"+ figure_number +"_hidden_textbox").name.split("]");
	    split2 = document.getElementById("knowledge_figure_"+ (figure_number + 1) +"_hidden_textbox").name.split("]");
	    document.getElementById("knowledge_figure_"+ figure_number +"_hidden_textbox").name = split1[0] + "]" + split2[1] + "]";

	    // label
	    document.getElementById("label_"+ figure_number).firstChild.nodeValue = document.getElementById("label_"+ (figure_number + 1)).firstChild.nodeValue;

	    // png
	    document.getElementById("png_"+ figure_number).firstChild.nodeValue = document.getElementById("png_"+ (figure_number + 1)).firstChild.nodeValue;

	    // img , link_to_img
	    document.getElementById("img_"+ i).src = document.getElementById("img_"+ (i+1)).src;
	    document.getElementById("link_to_img_"+ i).src = document.getElementById("link_to_img_"+ (i+1)).src;
	}

	// フィールドを消す。
	var delete_field;
	if (delete_field = document.getElementById("figure_field_"+ num_of_figure)){
	    delete_field.parentNode.removeChild(delete_field);
	}

	// 画像の数を一つ減らす。
	num_of_figure--;

	// ↑↓ボタンについても処理
	var last_down_button;
	if (last_down_button = document.getElementById("down_"+ num_of_figure)){
	    // delete down.png
	    var last_down_button_parent = last_down_button.parentNode;
	    last_down_button_parent.removeChild(last_down_button);
	    // add white.png
	    var down_img = document.createElement("img");
	    down_img.setAttribute("id", "down_alternative_"+ num_of_figure);
	    down_img.setAttribute("alt", "Down");
	    down_img.setAttribute("border", "0");
	    down_img.setAttribute("src", "/images/white.bmp");
	    down_img.setAttribute("height", "50");
	    down_img.setAttribute("width", "50");
	    last_down_button_parent.appendChild(down_img);
	}
    }

}

function moreFigure() {
  if (num_of_figure === void 0) {
    num_of_figure = 1;
  }

    if (num_of_figure > 0) {
	var down_swap_alternative = document.getElementById("down_alternative_"+ num_of_figure);
	var down_swap_parent = down_swap_alternative.parentNode;
	var down_swap = document.createElement("a");
	down_swap.setAttribute("href", "#");
	down_swap.setAttribute("id", "down_"+ num_of_figure);
	if (navigator.appVersion.indexOf("MSIE") != -1) {
	    down_swap.setAttribute("onclick", new Function("swapFigure("+ num_of_figure + "); location.href='#knowledge_figure_form'; return false;"));//for Internet Explorer
	} else {
	    down_swap.setAttribute("onclick", "swapFigure("+ num_of_figure + "); location.href='#knowledge_figure_form'; return false;");// for Firefox
	}
	var down_img = document.createElement("img");
	down_img.setAttribute("alt", "Down");
	down_img.setAttribute("border", "0");
	down_img.setAttribute("src", "/images/down.png");
	down_img.setAttribute("height", "50");
	down_img.setAttribute("width", "50");

	down_swap_parent.removeChild(down_swap_alternative);
	down_swap_parent.appendChild(down_swap);
	down_swap.appendChild(down_img);
    }
    num_of_figure++;

    var outer_tr = document.createElement("tr");
    var outer_td = document.createElement("td");
    var fieldset = document.createElement("fieldset");
    fieldset.setAttribute("id", "figure_field_"+ num_of_figure);
    if (navigator.appVersion.indexOf("MSIE") != -1) {
	fieldset.setAttribute("className", "form_fieldset");//for Internet Explorer
    } else {
	fieldset.setAttribute("class", "form_fieldset");//for Firefox
    }
    var figure_number = document.createElement("input");
    figure_number.setAttribute("id", "knowledge_figure_"+ num_of_figure +"_figure_number");
    figure_number.setAttribute("name", "knowledge_figure[" + num_of_figure + "][figure_number]");
    figure_number.setAttribute("type", "hidden");
    figure_number.setAttribute("value", num_of_figure-1);
    var legend = document.createElement("legend");
    var legend_text = document.createTextNode("Figure "+ num_of_figure);
    legend.appendChild(legend_text);
    var table = document.createElement("table");
    var tbody = document.createElement("tbody");
    var tr = document.createElement("tr");
    tr.setAttribute("valign", "top");

    var td1 = document.createElement("td");
    var up_swap = document.createElement("a");
    up_swap.setAttribute("href", "#");
    up_swap.setAttribute("id", "up_"+ num_of_figure);
    if (navigator.appVersion.indexOf("MSIE") != -1) {
	up_swap.setAttribute("onclick", new Function("swapFigure("+ (num_of_figure - 1) + "); location.href='#knowledge_figure_form'; return false;"));//for Internet Explorer
    } else {
	up_swap.setAttribute("onclick", "swapFigure("+ (num_of_figure - 1) + "); location.href='#knowledge_figure_form'; return false;");// for Firefox
    }
    var up_img = document.createElement("img");
    up_img.setAttribute("id", "up_img_"+ num_of_figure);
    up_img.setAttribute("alt", "Up");
    up_img.setAttribute("border", "0");
    up_img.setAttribute("src", "/images/up.png");
    up_img.setAttribute("height", "50");
    up_img.setAttribute("width", "50");
    /*
    var down_swap = document.createElement("a");
    down_swap.setAttribute("href", "#");
    down_swap.setAttribute("id", "down_"+ num_of_figure);
    if (navigator.appVersion.indexOf("MSIE") != -1) {
	down_swap.setAttribute("onclick", new Function("swapFigure("+ num_of_figure + "); location.href='#knowledge_figure_form'; return false;"));//for Internet Explorer
    } else {
	down_swap.setAttribute("onclick", "swapFigure("+ num_of_figure + "); location.href='#knowledge_figure_form'; return false;");// for Firefox
    }
    */
    var down_img = document.createElement("img");
    down_img.setAttribute("id", "down_alternative_"+ num_of_figure);
    down_img.setAttribute("alt", "Down");
    down_img.setAttribute("border", "0");
    down_img.setAttribute("src", "/images/white.bmp");
    down_img.setAttribute("height", "50");
    down_img.setAttribute("width", "50");

    var td2 = document.createElement("td");
    var caption_label = document.createTextNode("Caption:");
    var caption = document.createElement("textarea");
    caption.setAttribute("class", "caption_text")
    caption.setAttribute("id", "knowledge_figure_"+ num_of_figure +"_figure_caption");
    caption.setAttribute("name", "knowledge_figure["+ num_of_figure +"][figure_caption]");
    caption.setAttribute("cols", "50");
    caption.setAttribute("rows", "4");
    var figure_name = document.createElement("input");
    figure_name.setAttribute("id", "knowledge_figure_"+ num_of_figure +"_hidden_textbox");
    figure_name.setAttribute("name", "knowledge_figure[" + num_of_figure + "][name]");
    figure_name.setAttribute("type", "hidden");
    if (num_of_figure <= 9) {
	default_file_name = "image00".concat(String(num_of_figure));
    } else if (num_of_figure <= 99) {
	default_file_name = "image0".concat(String(num_of_figure));
    } else {
	default_file_name = "image".concat(String(num_of_figure));
    }
    figure_name.setAttribute("value", default_file_name);
    var label = document.createElement("div");
    label.setAttribute("id", "label_"+ num_of_figure);
    var label_text = document.createTextNode("Figure Path:");
    var figure_path = document.createElement("input");
    figure_path.setAttribute("type", "text");
    if (navigator.appVersion.indexOf("MSIE") != -1) {
	figure_path.setAttribute("className", "textarea_size60");//for Internet Explorer
    } else {
	figure_path.setAttribute("class", "textarea_size60");//for Firefox etc...
    }
    figure_path.setAttribute("id", "knowledge_figure_"+ num_of_figure +"_displayed_textbox");
    figure_path.setAttribute("name", "knowledge_figure["+ num_of_figure +"][figure_path]");
    figure_path.setAttribute("value", "");
    var png_span = document.createElement("span");
    png_span.setAttribute("id", "png_"+ num_of_figure);
    var png = document.createTextNode(".png");
    var diagram_id = document.createElement("input");
    diagram_id.setAttribute("id", "knowledge_figure_"+ num_of_figure +"_diagram_id");
    diagram_id.setAttribute("name", "knowledge_figure[" + num_of_figure + "][diagram_id]");
    diagram_id.setAttribute("type", "hidden");
    diagram_id.setAttribute("value", "");
    //    var diagram_path = document.createElement("input");
    //    diagram_path.setAttribute("id", "knowledge_figure_"+ num_of_figure +"_figure_path");
    //    diagram_path.setAttribute("name", "knowledge_figure[" + num_of_figure + "][diagram_path]");
    //    diagram_path.setAttribute("type", "hidden");
    //    diagram_path.setAttribute("value", "");

    var td3 = document.createElement("td");
    var figure_img = document.createElement("img");
    figure_img.setAttribute("src", "/images/white.bmp");
    figure_img.setAttribute("id", "img_"+ num_of_figure);
    figure_img.setAttribute("name", "img_"+ num_of_figure);
    figure_img.setAttribute("width", "150");
    figure_img.setAttribute("height", "150");
    var link_to_img = document.createElement("a");
    link_to_img.setAttribute("href", "#knowledge_figure_form");
    link_to_img.setAttribute("id", "link_to_img_"+ num_of_figure);
    link_to_img.setAttribute("name", "link_to_img_"+ num_of_figure);
    var view_image = document.createElement("span");
    view_image.setAttribute("id", "view_image_"+ num_of_figure);
    var link_to_view_img = document.createElement("a");
    link_to_view_img.setAttribute("href", "#knowledge_figure_form");
    link_to_view_img.setAttribute("id", "link_to_view_img_"+ num_of_figure);
    link_to_view_img.setAttribute("name", "link_to_view_img_"+ num_of_figure);
    var space = document.createTextNode("");
    var td4 = document.createElement("td");
    var delete_button = document.createElement("a");
    delete_button.setAttribute("href", "#");
    delete_button.setAttribute("id", "delete_figure_"+ num_of_figure);
    if (navigator.appVersion.indexOf("MSIE") != -1) {
	delete_button.setAttribute("onclick", new Function("deleteFigure("+ num_of_figure +");location.href='#knowledge_figure_form';return false;"));//for Internet Explorer
    } else {
	delete_button.setAttribute("onclick", "deleteFigure("+ num_of_figure +");location.href='#knowledge_figure_form';return false;");//for Firefox etc...
    }
    var delete_img = document.createElement("img");
    delete_img.setAttribute("alt", "X");
    delete_img.setAttribute("border", "0");
    delete_img.setAttribute("src", "/images/delete.png");

    var knowledge_figure_tbody = document.getElementById("knowledge_figure_tbody");
    knowledge_figure_tbody.appendChild(outer_tr);
    outer_tr.appendChild(outer_td);
    outer_td.appendChild(fieldset);
    fieldset.appendChild(figure_number);
    fieldset.appendChild(legend);
    fieldset.appendChild(table);
    table.appendChild(tbody);
    tbody.appendChild(tr);
    tr.appendChild(td1);
    td1.appendChild(document.createElement("br"));
    td1.appendChild(up_swap);
    up_swap.appendChild(up_img);
    td1.appendChild(document.createElement("br"));
    td1.appendChild(document.createElement("br"));
    td1.appendChild(down_img);
    tr.appendChild(td2);
    td2.appendChild(caption_label);
    td2.appendChild(document.createElement("br"));
    td2.appendChild(caption);
    td2.appendChild(figure_name);
    td2.appendChild(document.createElement("br"));
    td2.appendChild(label)
    label.appendChild(label_text);
    td2.appendChild(figure_path);
    td2.appendChild(png_span);
    png_span.appendChild(png);
    td2.appendChild(diagram_id);
    //    td2.appendChild(diagram_path);
    td2.appendChild(document.createElement("br"));
    tr.appendChild(td3);
    td3.appendChild(figure_img);
    td3.appendChild(document.createElement("br"));
    td3.appendChild(link_to_img);
    link_to_img.appendChild(view_image);
    view_image.appendChild(link_to_view_img);
    link_to_view_img.appendChild(space);
    tr.appendChild(td4);
    td4.appendChild(delete_button);
    delete_button.appendChild(delete_img);

    var first_up_button;
    if (first_up_button = document.getElementById("up_1")){
	first_up_button.parentNode.removeChild(first_up_button);
    }
}

function putNumSelectMenu(n){
  select_menu = document.getElementById("knowledge_horizontal_figures");
  if (n == 0) {
    select_menu.disabled = false;
  } else {
    select_menu.disabled = true;
  }
}

function putCategory(id){
  select = document.getElementById(id);
  category = document.getElementById("knowledge_category");
  category.value = select.title;
}

// gfdnavi.js にコピー
function showCommentListShort () {
    comment_list_short = document.getElementById("comment_list_short")
    comment_list_short_button = document.getElementById("comment_list_short_button")
    comment_list_hide = document.getElementById("comment_list_hide")
    comment_list_hide_button = document.getElementById("comment_list_hide_button")
    comment_list_all = document.getElementById("comment_list_all")
    comment_list_all_button = document.getElementById("comment_list_all_button")
    
    comment_list_short.style.display = "block";
    comment_list_short_button.style.borderStyle = "solid";
    comment_list_hide.style.display = "none";
    comment_list_hide_button.style.borderStyle = "none";
    comment_list_all.style.display = "none";
    comment_list_all_button.style.borderStyle = "none";
}

function showCommentListHide () {
    comment_list_short = document.getElementById("comment_list_short")
    comment_list_short_button = document.getElementById("comment_list_short_button")
    comment_list_hide = document.getElementById("comment_list_hide")
    comment_list_hide_button = document.getElementById("comment_list_hide_button")
    comment_list_all = document.getElementById("comment_list_all")
    comment_list_all_button = document.getElementById("comment_list_all_button")
    
    comment_list_short.style.display = "none";
    comment_list_short_button.style.borderStyle = "none";
    comment_list_hide.style.display = "block";
    comment_list_hide_button.style.borderStyle = "solid";
    comment_list_all.style.display = "none";
    comment_list_all_button.style.borderStyle = "none";
}

function showCommentListAll () {
    comment_list_short = document.getElementById("comment_list_short")
    comment_list_short_button = document.getElementById("comment_list_short_button")
    comment_list_hide = document.getElementById("comment_list_hide")
    comment_list_hide_button = document.getElementById("comment_list_hide_button")
    comment_list_all = document.getElementById("comment_list_all")
    comment_list_all_button = document.getElementById("comment_list_all_button")
      
    comment_list_short.style.display = "none";
    comment_list_short_button.style.borderStyle = "none";
    comment_list_hide.style.display = "none";
    comment_list_hide_button.style.borderStyle = "none";
    comment_list_all.style.display = "block";
    comment_list_all_button.style.borderStyle = "solid";
}

function deleteDisplayCommentInputFormButton () {
    display_comment_input_form_button = document.getElementById("display_comment_input_form_button");
    display_comment_input_form_button.style.display = "none";
}

// gfdnavi.js にコピー
function enableButton (button_id) {
    button = document.getElementById(button_id);
    button.style.display = "block";
}

function restoreDocument (title, textbody, description, category, creator, default_layout, horizontal_figures, figures_size_height_or_width, figures_size_units, figures_size_number) {
    document.getElementById("knowledge_title").value = title;
    document.getElementById("knowledge_textbody").value = textbody;
    document.getElementById("knowledge_description").value = description;
    document.getElementById("knowledge_category").value = category;
    document.getElementById("knowledge_creator").value = creator;

    document.getElementById("knowledge_default_layout").selectedIndex = Number(default_layout);
    document.getElementById("knowledge_horizontal_figures").value = Number(horizontal_figures);

    document.getElementById("knowledge_height_or_width").selectedIndex = Number(figures_size_height_or_width);
    document.getElementById("knowledge_percent_or_pixel").selectedIndex = Number(figures_size_units);
    document.getElementById("knowledge_figures_size_number").value = Number(figures_size_number);

    //グループ関連と絵がまだできてない
}

function restoreDocumentIncludePath (title, textbody, description, category, creator, default_layout, horizontal_figures, figures_size_height_or_width, figures_size_units, figures_size_number, path) {
    document.getElementById("node_path").value = path;
    restoreDocument(title, textbody, description, category, creator, default_layout, horizontal_figures, figures_size_height_or_width, figures_size_units, figures_size_number);
}



