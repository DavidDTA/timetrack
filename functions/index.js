const functions = require("firebase-functions");
const src = require("./src");

var elm = src.Elm.Server.init({ flags: functions.config() });
elm.ports.responses.subscribe(function(params) {
  params[0].status(params[1]).send(params[2]);
});
elm.ports.errors.subscribe(function(message) {
  console.error(message);
});

exports.api = functions.https.onRequest((request, response) => {
  elm.ports.requests.send([request, response]);
});
