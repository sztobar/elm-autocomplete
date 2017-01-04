(function() {

let cancelRequest;

app.ports.request.subscribe(query => {
  cancelRequest && cancelRequest();
  cancelRequest = jsonp(query, { timeout: 0 }, (err, data) => {
    if (!err) {
      app.ports.response.send(data);
    }
  });
});

})();