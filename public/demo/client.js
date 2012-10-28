(function () {
  var EditorClient = ot.EditorClient;
  var SocketIOAdapter = ot.SocketIOAdapter;
  var CodeMirrorAdapter = ot.CodeMirrorAdapter;

  var socket = io.connect('/');

  // uncomment to simulate more latency
  /*(function () {
    var emit = socket.emit;
    var queue = [];
    socket.emit = function () {
      queue.push(arguments);
      return socket;
    };
    setInterval(function () {
      if (queue.length) {
        emit.apply(socket, queue.shift());
      }
    }, 800);
  })();*/

  var disabledRegex = /(^|\s+)disabled($|\s+)/;

  function enable (el) {
    el.className = el.className.replace(disabledRegex, ' ');
  }

  function disable (el) {
    if (!disabledRegex.test(el.className)) {
      el.className += ' disabled';
    }
  }

  function preventDefault (e) {
    if (e.preventDefault) { e.preventDefault(); }
  }

  function stopPropagation (e) {
    if (e.stopPropagation) { e.stopPropagation(); }
  }

  function stopEvent (e) {
    preventDefault(e);
    stopPropagation(e);
  }

  function removeElement (el) {
    el.parentNode.removeChild(el);
  }

  function beginsWith (a, b) { return a.slice(0, b.length) === b; }
  function endsWith (a, b) { return a.slice(a.length - b.length, a.length) === b; }

  function wrap (chars) {
    cm.operation(function () {
      if (cm.somethingSelected()) {
        var selection = cm.getSelection();
        if (beginsWith(selection, chars) && endsWith(selection, chars)) {
          cm.replaceSelection(selection.slice(chars.length, selection.length - chars.length));
        } else {
          cm.replaceSelection(chars + selection + chars);
        }
      } else {
        var index = cm.indexFromPos(cm.getCursor());
        cm.replaceSelection(chars + chars);
        cm.setCursor(cm.posFromIndex(index + 2));
      }
    });
    cm.focus();
  }

  function bold ()   { wrap('**'); }
  function italic () { wrap('*'); }
  function code ()   { wrap('`'); }

  var editorWrapper = document.getElementById('editor-wrapper');
  var cm = window.cm = CodeMirror(editorWrapper, {
    lineNumbers: true,
    lineWrapping: true,
    mode: 'markdown',
    readOnly: 'nocursor'
  });
  cm.on('change', function () {
    if (!cmClient) { return; }
    (cmClient.undoManager.canUndo() ? enable : disable)(undoBtn);
    (cmClient.undoManager.canRedo() ? enable : disable)(redoBtn);
  });

  var undoBtn = document.getElementById('undo-btn');
  undoBtn.onclick = function (e) { cm.undo(); cm.focus(); stopEvent(e); };
  var redoBtn = document.getElementById('redo-btn');
  redoBtn.onclick = function (e) { cm.redo(); cm.focus(); stopEvent(e); };

  var boldBtn = document.getElementById('bold-btn');
  boldBtn.onclick = function (e) { bold(); stopEvent(e); };
  disable(boldBtn);
  var italicBtn = document.getElementById('italic-btn');
  italicBtn.onclick = function (e) { italic(); stopEvent(e); };
  disable(italicBtn);
  var codeBtn = document.getElementById('code-btn');
  disable(codeBtn);
  codeBtn.onclick = function (e) { code(); stopEvent(e); };

  var loginForm = document.getElementById('login-form');
  loginForm.onsubmit = function (e) {
    preventDefault(e);
    var username = document.getElementById('username').value;
    socket
      .emit('login', { name: username })
      .on('logged_in', function () {
        var li = document.createElement('li');
        li.appendChild(document.createTextNode(username + " (that's you!)"));
        cmClient.clientListEl.appendChild(li);

        enable(boldBtn);
        enable(italicBtn);
        enable(codeBtn);

        cm.setOption('readOnly', false);
        removeElement(overlay);
        removeElement(loginForm);
      });
  };

  var overlay = document.createElement('div');
  overlay.id = 'overlay';
  overlay.onclick = stopPropagation;
  overlay.onmousedown = stopPropagation;
  overlay.onmouseup = stopPropagation;
  var cmWrapper = cm.getWrapperElement();
  cmWrapper.appendChild(overlay);

  var cmClient;
  socket.on('doc', function (obj) {
    cm.setValue(obj.str);
    cmClient = window.cmClient = new EditorClient(
      obj.revision,
      obj.clients,
      new SocketIOAdapter(socket),
      new CodeMirrorAdapter(cm)
    );
    var userListWrapper = document.getElementById('userlist-wrapper');
    userListWrapper.appendChild(cmClient.clientListEl);
  });
})();