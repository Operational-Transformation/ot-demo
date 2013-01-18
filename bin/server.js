#!/usr/bin/env node

var ot = require('ot');
var express = require('express');
var socketIO = require('socket.io');
var path = require('path');
var http = require('http');

var app = express();
var appServer = http.createServer(app);

app.configure(function () {
  app.use(express.logger());
  app.use(express.static(path.join(__dirname, '../public')));
  app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
});

var io = socketIO.listen(appServer);

// source: http://devcenter.heroku.com/articles/using-socket-io-with-node-js-on-heroku
io.configure('production', function () {
  io.set('transports', ['xhr-polling']);
  io.set('polling duration', 10);
});

var str = "# This is a Markdown heading\n\n"
        + "1. un\n"
        + "2. deux\n"
        + "3. trois\n\n"
        + "Lorem *ipsum* dolor **sit** amet.\n\n"
        + "    $ touch test.txt";
var cmServer = new ot.CodeMirrorServer(str, io.sockets, [], function (socket, cb) {
  cb(!!socket.mayEdit);
});
io.sockets.on('connection', function (socket) {
  socket.on('login', function (obj) {
    if (typeof obj.name !== 'string') {
      console.error('obj.name is not a string');
      return;
    }
    socket.mayEdit = true;
    cmServer.setName(socket, obj.name);
    socket.emit('logged_in', {});
  });
});

var port = process.env.PORT || 3000;
appServer.listen(port, function () {
  console.log("Listening on port " + port);
});

process.on('uncaughtException', function (exc) {
  console.error(exc);
});
