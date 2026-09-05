// Scroll the survey back to its top when the section changes.
Shiny.addCustomMessageHandler("sextant-scroll-top", function(_) {
  var el = document.getElementById("sextant-survey-top");
  if (el) el.scrollIntoView({behavior: "auto", block: "start"});
  window.scrollTo(0, 0);
});

// Draft autosave: the server sends the draft, the browser keeps it.
Shiny.addCustomMessageHandler("sextant-draft-store", function(msg) {
  try { localStorage.setItem("sextant-draft", msg.draft); } catch (e) {}
});
Shiny.addCustomMessageHandler("sextant-draft-clear", function(_) {
  try { localStorage.removeItem("sextant-draft"); } catch (e) {}
});
$(document).on("shiny:connected", function() {
  try {
    var d = localStorage.getItem("sextant-draft");
    if (d) Shiny.setInputValue("sextant_stored_draft", d, {priority: "event"});
  } catch (e) {}
});
