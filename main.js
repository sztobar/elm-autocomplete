var require = function() { return function() { return console.log.bind(console); } };
var module = {};

(function() {

  var cancelRequest;

  var app = Elm.Main.fullscreen();

  app.ports.request.subscribe(query => {
    cancelRequest && cancelRequest();
    cancelRequest = jsonp(query, { timeout: 0 }, (err, data) => {
      if (!err) {
        app.ports.response.send(data[1]);
      }
    });
  });

})();
