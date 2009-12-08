/* @licstart  The following is the entire license notice for the Javascript code in this page.
 *
 * Copyright 2009 Harri Pitkänen (hatapitk@iki.fi)
 *
 * The Javascript code in this page is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice for the Javascript code in this page.
 */

function joukahainen(wid) {
  var options = {
    title: "Joukahainen"
  }
  var frame = "<iframe src='http://joukahainen.puimula.org/word/edit?wid="
              + wid + "'></iframe>";
  $(frame).dialog(options).show();
}

function wordInfoReceived(html) {
  var options = {
    width: 450
  };
  $(html).dialog(options).show();
}

function wordClicked(evt) {
  var word = $(this).text();
  $.get("/wordinfo", {q: word}, wordInfoReceived, "html");
}

function gErrorClicked(evt) {
  var options = {
    width: 450,
    title: "Mahdollinen kielioppivirhe"
  };
  var outerElement = $(this).parent()
  var original = $("<span />").text(outerElement.find(".gErrorInner").text());
  var explanation = outerElement.attr("errortext");
  var html = "<div>... " + original.html() + " ...<br />"
             + explanation + "</div>";
  $(html).dialog(options).show();
}

function buildGrammarError(outerElement) {
  var content = outerElement.html();
  outerElement.html("*<span class='gErrorInner'>" + html + "</span>");
}

function updateReceived(html) {
  $("#result").html(html);
  $("#result .gErrorOuter").wrapInner("<span class='gErrorInner'></span>");
  $("#result .gErrorOuter").prepend("<span class='gErrorHandle'>*</span>");
  $("#result .word").click(wordClicked);
  $("#result .gErrorHandle").click(gErrorClicked);
}

function inputChanged() {
  var text = $("#input").val();
  $.get("/spell", {q: text}, updateReceived, "html");
}

function keyUpInInput(evt) {
  if (evt.keyCode >= 16 && evt.keyCode <= 40) {
    // Modifier keys such as Ctrl
    // Movement keys such as arrow left etc.
    return;
  }
  inputChanged();
}

google.load("jquery", "1.3.2");
google.load("jqueryui", "1.7.2");
google.setOnLoadCallback(function() { jQuery(function($) {
  $("#input").keyup(keyUpInInput);
});});
