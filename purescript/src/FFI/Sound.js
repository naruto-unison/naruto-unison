// module FFI.Sound

"use strict";

var vol;

exports["sfxRegister"] = function (show) {
  return function(sounds) {
    return function() {
      vol = document.getElementById("volume")
      vol.onclick = function () {
        var muted = this.className === "click muted"
        this.className = muted ? "click unmuted" : "click muted"
        if (muted)
          createjs.Sound.play("SFXClick")
        if (user) {
          var xmlhttp = new XMLHttpRequest();
          xmlhttp.open("GET", "/api/mute/" + (muted ? "False" : "True"), true)
          xmlhttp.send()
        }
      }
      createjs.Sound.registerSounds(sounds.map(show).map(function(file) {
        return {src: file + ".ogg", id: file}
      }), "audio/")
    }
  }
}

exports["sound_"] = function (show) {
  return function(sound) {
    return function() {
      if (vol && vol.className !== "click muted")
        createjs.Sound.play(show(sound))
    }
  }
}
