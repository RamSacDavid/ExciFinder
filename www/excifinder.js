(function () {
  "use strict";

  Shiny.addCustomMessageHandler(
    "excifinder-focus-master-product",
    function (message) {
      if (!message || typeof message.product_id !== "string") return;

      function focusSelectedButton() {
        var buttons = document.querySelectorAll(
          "button.excifinder-master-item[data-product-id]"
        );
        for (var index = 0; index < buttons.length; index += 1) {
          if (
            buttons[index].dataset.productId === message.product_id &&
            buttons[index].getAttribute("aria-pressed") === "true"
          ) {
            buttons[index].focus({ preventScroll: true });
            return true;
          }
        }
        return false;
      }

      if (focusSelectedButton()) return;

      var results = document.querySelector(".excifinder-results");
      if (!results) return;
      var observer = new MutationObserver(function () {
        if (focusSelectedButton()) observer.disconnect();
      });
      observer.observe(results, { childList: true, subtree: true });
    }
  );
})();
