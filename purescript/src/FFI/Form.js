// module FFI.Form

"use strict";

exports["getForm_"] = function(just) {
  return function(nothing) {
    return function(id) {
      return function() {
        var form = document.getElementById(id);
        if (!form) return nothing;
        var fields = {};
        for (var i = 0; i < form.length; i++) {
          var el = form[i];
          fields[el.name] = (el.type === "checkbox") ? (el.checked ? "True" : "False") : encodeURIComponent(el.value);
        }
        return just(fields);
      }
    }
  }
}
