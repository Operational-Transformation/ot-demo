#!/usr/bin/env node

var ot = require('ot');
var express = require('express');
var morgan = require('morgan');
var serveStatic = require('serve-static');
var errorhandler = require('errorhandler');
var socketIO = require('socket.io');
var path = require('path');
var http = require('http');

var app = express();
var appServer = http.createServer(app);

app.use(morgan('combined'));
app.use('/', serveStatic(path.join(__dirname, '../../public')));
app.use('/static', serveStatic(path.join(__dirname, '../../public')));
if (process.env.NODE_ENV === 'development') {
  app.use(errorhandler());
}

var io = socketIO.listen(appServer);

var str = "# This is a Markdown heading\n\n"
        + "1. un\n"
        + "2. deux\n"
        + "3. trois\n\n"
        + "Lorem *ipsum* dolor **sit** amet.\n\n"
        + "    $ touch test.txt";
var socketIOServer = new ot.EditorSocketIOServer(str, [], 'demo', function (socket, cb) {
  cb(!!socket.mayEdit);
});
io.sockets.on('connection', function (socket) {
  socketIOServer.addClient(socket);
  socket.on('login', function (obj) {
    if (typeof obj.name !== 'string') {
      console.error('obj.name is not a string');
      return;
    }
    socket.mayEdit = true;
    socketIOServer.setName(socket, obj.name);
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
