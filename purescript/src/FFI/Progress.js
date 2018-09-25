// module FFI.Progress

"use strict";

var prog

exports["progress"] = function (duration) {
  return function(from) {
    return function(to) {
      return function() {
        if (!prog) 
          prog = new ProgressBar.Line("#ready",
            { color: '#0099ff'
            , fill: '#111'
          })
        prog.set(from)
        if (duration > 0)
          prog.animate(to, { duration: duration })
      }
    }
  }
}
