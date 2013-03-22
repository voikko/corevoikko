/* @licstart  The following is the entire license notice for the Javascript code in this page.
 *
 * Copyright 2009 - 2011 Harri Pitkänen (hatapitk@iki.fi)
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

var AJAX_HANDLER_URL="";
var LOCAL_STORAGE_KEY = "WebVoikkoSpell";

function initLocalStorage() {
  if (!window.localStorage) {
    return;
  }
  $("#save").css("display", "inline");
  var stored = window.localStorage.getItem(LOCAL_STORAGE_KEY);
  if (stored) {
    $("#input").val(stored);
  }
  $("#save").click(function() {
    if (true /*!stored*/) {
      if (!confirm("Tällä tallennustoiminnolla tekstisi tallentuu selaimeesi, ja se latautuu tekstikenttään, kun " +
	      "seuraavan kerran palaat sivulle. Tekstiä EI TALLENNETA PALVELIMELLE, joten se ei säily, jos " +
	      "tyhjennät selaimeesi tallennetut tiedot. Jos tietokonettasi käyttää joku toinen, hän voi nähdä tekstisi. "+
	      "Toisaalta muilta koneilta tai selaimilta ei tekstiisi pääse kukaan käsiksi, et edes sinä itse.\n\n" +
	      "Suomeksi sanottuna TÄMÄ TALLENNUSTOIMINTO ON EPÄLUOTETTAVA mutta toisinaan kätevä. Haluatko varmasti tallentaa?")) {
        return false;
      }
    }
    window.localStorage.setItem(LOCAL_STORAGE_KEY, $("#input").val());
    return false;
  });
}

function loadPortlet(divId) {
  $("#" + divId).load(AJAX_HANDLER_URL + "portlet", function() {
    $("#progress").hide();
    $("#tabs").tabs();
    $("#input").keyup(keyUpInInput);
    $("#input").bind("input", keyUpInInput); // for touch screen devices
    $("#input").click(clickInInput);
    $("#input").bind("cut", inputChanged);
    $("#input").bind("paste", inputChanged);
    $("#voikkoDict").bind("change", dictionaryChanged);
    $("#checkPageClicked").click(checkPageClicked);
    $("#pageUrl").keyup(function(event) {
      if (event.keyCode == 13) {
        // Enter
        checkPageClicked();
      }
    });
    $('#tabs').bind('tabsshow', function(event, ui) {
      $("#result").text("");
    });
    initLocalStorage();
    triggerReferrerCheck();
  });
}

function triggerReferrerCheck() {
  if (window.location.search.search("check=referrer") < 0) {
    return;
  }
  ref = document.referrer;
  if (!ref) {
    return;
  }
  $("#tabs").tabs("select", "#pageUrl");
  $("#pageUrl").val(ref)
  checkPageClicked()
}

function joukahainen(wid) {
  var options = {
    title: "Joukahainen",
    width: 600,
    height: 400
  }
  var frame = $("<div></div>").load(AJAX_HANDLER_URL + "joukahainen?wid=" + wid + " .main");
  frame.dialog(options).show();
}

function dictionaryChanged() {
  if ($("#tabDirect").is(":visible")) {
    inputChanged();
  }
}

function switchDict(button, dictName) {
  $(button).parents(".ui-dialog-content").first().dialog("destroy");
  $("#voikkoDict").val(dictName).change();
}

function wordInfoReceived(html, originalWord, textarea, startPos) {
  var options = {
    width: 450
  };
  var wordInfo = $(html);
  var wordDialog = wordInfo.dialog(options);
  wordInfo.find(".suggestion").click(function() {
    var currentText = textarea.val();
    if (currentText.substr(startPos, originalWord.length) == originalWord) {
      var newText = currentText.substr(0, startPos) + $(this).text() + currentText.substr(startPos + originalWord.length);
      textarea.val(newText);
      wordDialog.dialog("destroy");
      inputChanged();
    }
  });
  wordDialog.show();
}

function wordClicked(evt) {
  var wordSpan = $(this);
  var word = wordSpan.text();
  var dict = $("#voikkoDict").val();
  $.ajax({
    url: AJAX_HANDLER_URL + "wordinfo",
    data: {q: word, d: dict},
    success: function(html) {
      wordInfoReceived(html, word, $("#input"), parseInt(wordSpan.attr("data-start-pos"), 10));
    },
    dataType: "html"
  });
}

function gErrorClicked(evt) {
  var options = {
    width: 450,
    title: "Mahdollinen kielioppivirhe"
  };
  var outerElement = $(this).parent()
  var original = $("<span />").text(outerElement.find(".gErrorInner").text());
  var errorText = outerElement.attr("errortext");
  var divElement = $("<div />");
  divElement.append($("<span>... </span>"));
  divElement.append(original);
  divElement.append($("<span> ...</span>"));
  divElement.append($("<br />"));
  divElement.append($("<span />").text(errorText));
  divElement.dialog(options).show();
}

function updateReceived(html) {
  $("#result").html(html);
  $("#result .gErrorOuter").wrapInner("<span class='gErrorInner'></span>");
  $("#result .gErrorOuter").prepend("<span class='gErrorHandle'>*</span>");
  $("#result .word").click(wordClicked);
  $("#result .gErrorHandle").click(gErrorClicked);
  clearProgressMessage();
}

function updateError(jqXHR, textStatus, errorThrown) {
  $("#result").text("Tekstin analysointi ei onnistunut. Yritä myöhemmin uudelleen.");
  clearProgressMessage();
}

var lastUpdateTimerId = null;

function setProgressMessage() {
  $("#pageUrl").attr("disabled", true);
  $("#checkPageClicked").attr("disabled", true);
  $("#progress").show();
}

function clearProgressMessage() {
  $("#progress").hide();
  $("#checkPageClicked").attr("disabled", false);
  $("#pageUrl").attr("disabled", false);
}

function requestUpdate() {
  lastUpdateTimerId = null;
  var textContent = $("#input").val();
  var dict = $("#voikkoDict").val();
  $.ajax({
    type: 'POST',
    url: AJAX_HANDLER_URL + "spell",
    data: {q: textContent, d: dict},
    success: updateReceived,
    error: updateError,
    dataType: "html"
  });
}

function resetTimeout() {
  if (lastUpdateTimerId != null) {
    window.clearTimeout(lastUpdateTimerId);
    lastUpdateTimerId = null;
  }
}

function checkPageClicked() {
  resetTimeout();
  setProgressMessage();
  var url = $("#pageUrl").val();
  var dict = $("#voikkoDict").val();
  $.post(AJAX_HANDLER_URL + "checkPage", {url: url, d: dict}, updateReceived, "html");
}

function inputChanged() {
  resetTimeout();
  setProgressMessage();
  lastUpdateTimerId = window.setTimeout(requestUpdate, 1200);
}

function keyUpInInput(evt) {
  if (evt.keyCode == 32) {
    // space
    inputChanged();
    return;
  }
  if (evt.keyCode >= 16 && evt.keyCode <= 40) {
    // Modifier keys such as Ctrl
    // Movement keys such as arrow left etc.
    return;
  }
  if ((evt.keyCode == 65 || evt.keyCode == 67) && evt.ctrlKey) {
    // Ctrl+A, Ctrl+C
    return;
  }
  // other keys
  inputChanged();
}

function clickInInput(evt) {
  if (evt.button == 1) {
    // middle mouse button (paste selection in X)
    inputChanged();
  }
}

function clearClicked() {
 $("#input").val("");
}

google.load("jquery", "1.4.1");
google.load("jqueryui", "1.7.2");
