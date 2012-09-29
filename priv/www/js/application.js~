$(document).ready(function(){

    var port = location.port ? ":"+location.port : ":80";
    var temp = location.hostname.split('.');
    temp[temp.length - 1] =  temp[temp.length - 1]+"1";
    var hostname = location.hostname;
    var hostname1 = temp.join('.');

    var currentWsUri = 0;
    var wsUriArray = ["ws://"+hostname+port+"/game","ws://"+hostname1+port+"/game"];
    var wsUri = wsUriArray[currentWsUri];

    var socket;
    var guid;
    var uuid;

    var $menu = $('.b-menu');
    var $gameBoard = $('.b-game-board');
    var $unitsWrapper;
    var $gameList = $('.j-game-list');
    var $statistic = $('.j-statistic');


    var gameStatus = "";
    var reconnectionTimer = false;

    $('.j-btn-play').button('reset');
    $('.j-btn-load').click(function (e) { getGameList(e) });
    $('.j-btn-dropdown-load').click(function (e) { getGameList(e) });
    $('.j-btn-exit').click(function (){ location.reload(true); });

    $(document).on( 'click' , '.j-new-game', function(){ newGame(wsUri) } );
    $(document).on( 'click' , '.j-load-game', function(){ newGame(wsUri+"?GUID="+encodeURIComponent($(this).data('guid'))) });

    function getGameList(e){
      if($('.j-text-guid').val() != ""){
        newGame(wsUri+"?GUID="+encodeURIComponent($('.j-text-guid').val()));
        e.stopPropagation();
        e.preventDefault();
        return true;
      }
      $.get("/list", {}, function(data){
        var games = data;
        var html = "";
        for(var i in games){
          var n = 1*i+1;
          html += '<li><a class="j-load-game" data-guid="'+games[i]+'">'+games[i]+'</a></li>';
        }
        if(html == ""){ html = '<li><a class="j-new-game">New game</a></li>'; }
        $gameList.html(html);
      }, "json")
    }
    getGameList();

    function newGame(gameUri){

      socket = new WebSocket(gameUri);
      socket.onopen = function (evt) { openGame(evt) };
      socket.onmessage  = function (evt) { updateState(evt) };
      socket.onclose  = function (evt) {
        if(gameStatus == "Game over") return;
        gameReconnection();
      };
      socket.onerror  = function (evt) { gameReconnection(); };
    }

    function gameReconnection(){
      if(guid == undefined || uuid == undefined) return;
      if(reconnectionTimer) return;

      gameOver("Connection closed. Reconnection... ");
      newGame(wsUri+"?GUID="+encodeURIComponent(guid)+"&UUID="+encodeURIComponent(uuid));
      reconnectionTimer = setInterval( function() {
        gameOver("Connection closed. Reconnection... ");
        newGame(wsUri+"?GUID="+encodeURIComponent(guid)+"&UUID="+encodeURIComponent(uuid));
        currentWsUri += 1;
        currentWsUri = (currentWsUri > 1)? 0 : currentWsUri;
        wsUri = wsUriArray[currentWsUri];
      }, 5000);

    }

    function openGame(){
      drawGrid();
      clearInterval(reconnectionTimer);
      reconnectionTimer  = 0;
      $('.j-btn-play').hide();
      $('.j-btn-load').hide();
      $('.j-btn-dropdown-load').hide();
      $('.j-text-guid').hide();
      $('.j-btn-exit').show();
      $('.j-statistic').show();
      $gameBoard.show();
    }

    function gameOver(text){
      $('.j-btn-play').show();
      $('.j-btn-load').show();
      $('.j-text-guid').show();
      $('.j-btn-dropdown-load').show();
      $('.j-btn-exit').hide();
      $('.j-statistic').hide();

      if(text != undefined){
        gameStatus = text;
        $gameBoard.html("<p class='game-over'>"+text+"</p>");

      } else {
        gameStatus = "Game over";
        $gameBoard.hide();
        $gameBoard.html("");
      }

      socket.close();

    }

    function updateState(evt) {

      var data = $.parseJSON(evt.data);

      if(data.guid && data.uuid){
        guid = data.guid;
        uuid = data.uuid;
        $statistic.html("game id: " + guid);
      }
      if(data.self && data.enemy){
        drawUnits(data.self, data.enemy);
      }
    }
    function drawUnits(self, enemies){

      var step = 20;
      var $ship = $('.chip');
      if(self != 'dead'){
        var html = '<div class="chip chip-style" style="left: '+step*self[0]+'px; top: '+step*self[1]+'px;"></div>';
      } else {
        gameOver("Game over");
      }
      for(var i in enemies){
        html+= '<div class="chip chip-style chip-style-red" style="left: '+step*enemies[i][0]+'px; top: '+step*enemies[i][1]+'px;"></div>';
      }
      $unitsWrapper.html('').html(html);
    }


    $(document).keyup(function (e) {

        var keyCode = e.keyCode || e.which,
            arrow = {left: 37, up: 38, right: 39, down: 40 };
        var offset;
        var speed = 250;

        switch (keyCode) {
            case arrow.left: socket.send("left"); e.preventDefault(); break;
            case arrow.up: socket.send("down"); e.preventDefault(); break;
            case arrow.right: socket.send("right"); e.preventDefault(); break;
            case arrow.down: socket.send("up"); e.preventDefault(); break;
        }
    });
    $('.j-move-left').click(function(e){ socket.send("left"); e.preventDefault(); });
    $('.j-move-up').click(function(e){ socket.send("down"); e.preventDefault(); });
    $('.j-move-right').click(function(e){ socket.send("right"); e.preventDefault(); });
    $('.j-move-down').click(function(e){ socket.send("up"); e.preventDefault(); });

    function drawGrid(){

      var boardTr = "";
      var boardGrid = "";
      var tableSize = 30;

      for(var i=0; i < tableSize; i++){
        boardTr += '<div></div>';
      }
      for(var i=0; i < tableSize; i++){
        boardGrid += '<div>'+boardTr+'</div>';
      }

      $gameBoard.html('<div class="b-game-grid">'+boardGrid+'</div><div class="j-units-wrapper b-units-wrapper"></div>');
      $unitsWrapper = $('.j-units-wrapper');
    }

});