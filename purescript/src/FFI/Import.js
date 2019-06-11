// module FFI.Import

"use strict";
exports["bg"]           = bg;
exports["fUserTeam"]    = userTeam;
exports["fUser"]        = user;
exports["hostname"]     = document.URL.replace("http:", "ws:").replace("https:", "wss:");
exports["reload"]       = function() { location.reload(true); }
exports["getPageSize"]  = function() {
    var charScroll = document.getElementById("charScroll");
    if (!charScroll.children[0]) return 0;
    return ( (charScroll.clientHeight / (charScroll.children[0].clientHeight + 4) | 0)
           * (charScroll.clientWidth  / (charScroll.children[0].clientWidth  + 8) | 0)
           );
}
exports["avatars"]        = avatars;
exports["fChars"]         = chars;
exports["csrf"]           = Cookies.get("XSRF-TOKEN");
