var numInputs = 1;
var numArgs = 0;

function changeInput() {
  numInputs = $('function_nvars').selectedIndex + 1;
  var ary = new Array(numArgs+numInputs);
  for (i=0; i<numArgs; i++)
    ary[i] = 'arg' + i;
  for (i=0; i<numInputs; i++)
    ary[i+numArgs] = 'gphys' + i;
  Element.update('input_args', ary.join(", "));
}

var numArgMax=0;
function changeArgument() {
  numArgs = $('argument_num').selectedIndex;
  changeInput();

  for (i=0; i<[numArgs,numArgMax].min(); i++)
      Element.show('argument_'+i);
  for (i=numArgs;i<numArgMax; i++)
      Element.hide('argument_'+i);

  html = "";
  for (i=numArgMax; i<numArgs; i++) {
    html += "<div id='argument_" + i + "'>\n";
    html += "<h4>argument" + i +"</h4>\n";
    html += "<table>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='argument_" + i + "_description'>description</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <textarea rows='5' cols='40' name='argument[" + i + "][description]' id='argument_" + i + "_description'></textarea>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='argument_" + i + "_value_type'>value type</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <select id='argument_" + i + "_value_type' name='argument[" + i + "][value_type]'>\n";
    var arg_types = ['int','float','string','boolean','array_int','array_float','array_string','array_boolean'];
    for(var j=0; j<arg_types.length; j++)
      html += "        <option selected>" + arg_types[j] + "</option>\n";
    html += "      </select>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='argument_" + i + "_default'>default</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <input type='text' size='10' name='argument[" + i + "][default]' id='argument_" + i + "_default'/>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "</table>\n";
    html += "</div>\n";
    numArgMax = i+1;
  }
  new Insertion.Bottom('arguments_table', html);
}

var numOutMax=0;
function changeOutput() {
  var num = $('output_num').selectedIndex + 1;

  for (i=0; i<[num,numOutMax].min(); i++)
      Element.show('output_'+i);
  for (i=num; i<numOutMax; i++)
      Element.hide('output_'+i);

  var html = "";
  for (i=numOutMax; i<num; i++) {
    html += "<div id='output_" + i + "'\n";
    html += "<h4>output" + i +"</h4>\n";
    html += "<table>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='output_" + i + "_name'>name</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <input type='text' size='10' name='output[" + i + "][name]' id='output_" + i + "_name'/>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='output_" + i + "_subscript'>subscript</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <input type='text' size='10' name='output[" + i + "][subscript]' id='output_" + i + "_subscript'/>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "  <tr>\n";
    html += "    <td>\n";
    html += "      <label for='output_" + i + "_description'>description</label>\n";
    html += "    </td>\n";
    html += "    <td>\n";
    html += "      <textarea rows='5' cols='40' name='output[" + i + "][description]' id='output_" + i + "_description'></textarea>\n";
    html += "    </td>\n";
    html += "  </tr>\n";
    html += "</table>\n";
    html += "</div>\n";
    numOutMax = i+1;
  }
  new Insertion.Bottom('outputs_table', html);
}
