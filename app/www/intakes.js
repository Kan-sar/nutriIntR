// Flush debounced fields before an action so rapid typing uses the latest values.
window.syncIntakeFields = function (ids) {
  ids.forEach(function (id) {
    var element = document.getElementById(id);
    if (!element) return;
    var value = element.type === "number"
      ? (element.value === "" ? null : Number(element.value))
      : element.value;
    Shiny.setInputValue(id, value, {priority: "event"});
  });
};

$(function () {
  Shiny.addCustomMessageHandler("ai-busy", function (busy) {
    var button = document.getElementById("explain");
    if (!button) return;
    button.disabled = busy;
    button.textContent = busy ? "Generating explanation..." : "Generate explanation";
    button.setAttribute("aria-busy", busy ? "true" : "false");
  });
  Shiny.addCustomMessageHandler("selected-finding", function (message) {
    document.querySelectorAll(".finding-choice").forEach(function (button) {
      button.setAttribute("aria-pressed", button.dataset.finding === message.id ? "true" : "false");
    });
  });
  $(document).on("click", ".intake-action", function () {
    Shiny.setInputValue(this.dataset.action + "_intake", this.dataset.token, {priority: "event"});
  });
  $(document).on("click", ".finding-choice", function () {
    Shiny.setInputValue("choose_finding", this.dataset.finding, {priority: "event"});
  });
  // Charts first rendered inside collapsed details need their visible dimensions.
  document.addEventListener("toggle", function (event) {
    if (event.target.tagName === "DETAILS" && event.target.open) {
      window.dispatchEvent(new Event("resize"));
    }
  }, true);
  // Shiny custom handlers require exactly one parameter, even for empty messages.
  Shiny.addCustomMessageHandler("clear-intake-times", function (message) {
    ["consumed_time", "medication_time"].forEach(function (id) {
      var element = document.getElementById(id);
      if (element) element.value = "";
      Shiny.setInputValue(id, "", {priority: "event"});
    });
  });
  // Native time controls have no Shiny binding. Initialize each newly opened modal.
  $(document).on("shown.bs.modal", function () {
    var element = document.getElementById("edit_time");
    if (element) Shiny.setInputValue("edit_time", element.value, {priority: "event"});
  });
});
