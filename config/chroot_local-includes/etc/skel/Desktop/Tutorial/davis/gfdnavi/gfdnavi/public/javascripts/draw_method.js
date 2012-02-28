var numInputs = 1;
var numAttrs = 0;

function changeVars() {
  numInputs = $('draw_method_nvars').selectedIndex + 1;
  var ary = new Array(numAttrs+numInputs);
  for (i=0; i<numInputs; i++) {
      if (i==0)
	  ary[i] = 'gphys';
      else
	  ary[i] = 'gphys' + i;
  }
  ary[numInputs] = "opt";
  Element.update('vizshot_args', ary.join(", "));
  ary[numInputs] = "newfrm";
  ary[numInputs+1] = "opt";
  Element.update('ggraph_args', ary.join(", "));
}

var numOptMax=0;
function changeOptions() {
  numOpts = $('option_num').selectedIndex;
  changeVars();

  for (i=0; i<[numOpts,numOptMax].min(); i++)
      Element.show('option_'+i);
  for (i=numOpts;i<numOptMax; i++)
      Element.hide('option_'+i);

  html = "";
  for (i=numOptMax; i<numOpts; i++) {
    html += "<div id='option_" + i + "'>\n";
    html += "<h4>&lt;Option " + i +"&gt;</h4>\n";
    html += "<table>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='option_" + i + "_name'>name</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <input type='text' name='option[" + i + "][name]' id='option_" + i + "_name'></input>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='option_" + i + "_default'>default</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <input type='text' size='10' name='option[" + i + "][default]' id='option_" + i + "_default'/>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='option_" + i + "_optional'>optional</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <input type='checkbox' name='option[" + i + "][optional]' id='option_" + i + "_optional' value='true' />\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='option_" + i + "_value_type'>value type</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <select id='option_" + i + "_value_type' name='option[" + i + "][value_type]'>\n";
    html += "        <option selected>int</option>\n";
    html += "        <option>float</option>\n";
    html += "        <option>string</option>\n";
    html += "        <option>boolean</option>\n";
    html += "        <option>array_int</option>\n";
    html += "        <option>array_float</option>\n";
    html += "      </select>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='option_" + i + "_parser'>parser</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <select id='option_" + i + "_parser' name='option[" + i + "][parser]'>\n";
    html += "        <option selected>vizshot</option>\n";
    html += "        <option>ggraph</option>\n";
    html += "      </select>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "</table>\n";
    html += "</div>\n";
    numOptMax = i+1;
  }
  new Insertion.Bottom('option_table', html);
}

