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
