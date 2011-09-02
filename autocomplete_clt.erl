-module(autocomplete_clt).
-export([initClient/3, start/3]).

%% ServerName: autocomplete server host
%% Port: autocomplete server listing port
%% Pid: calling process id
%% return: same as spawn_link
initClient(ServerName, Port, Pid) ->
  spawn_link(fun() -> start(ServerName, Port, Pid) end).

%% ServerName: autocomplete server host
%% Port: autocomplete server listing port
%% Pid: calling process id
%% usage: spawn(start) or spawn_link(start)
start(ServerName, Port, Pid) ->
  %% connect to server
  case gen_tcp:connect(ServerName, Port, [binary, {packet, 0}]) of 
     {ok, Socket} -> 
        loop(ServerName, Port, Socket, Pid, []);
     Other ->
        io:format("failed to connect to host=~p at port:~p\n", [ServerName, Port]),     
        Pid ! {connection_failed, Other}  
   end.


loop(ServerName, Port, Socket, Pid, SoFar) ->
  receive
     {add, Key, Value, RequestId} ->
       io:format("get add request, key=~p, value=~p\n", [Key, Value]),
       add(Key, Value, RequestId, Socket),
       loop(ServerName, Port, Socket, Pid, SoFar);
     {remove, Key, RequestId} ->
       io:format("get remove request key=~p\n", [Key]),
       remove(Key, RequestId, Socket),
       loop(ServerName, Port, Socket, Pid, SoFar);
     {search, Key, RequestId} ->
       io:format("get search request key=~p\n", [Key]),
       search(Key, RequestId, Socket),
       loop(ServerName, Port, Socket, Pid, SoFar);
     {tcp, Socket, Bin} ->
        case processResponse(Pid, [SoFar|binary_to_list(Bin)]) of
            more_read ->
              loop(ServerName, Port, Socket, Pid, [SoFar| binary_to_list(Bin)]);
            Other -> 
              {error, Other}
        end;      
     {tcp_closed, _} ->
       io:format("connection to server closed, reopen it \n"),
       start(ServerName, Port, Pid);
     Other ->
       {error, Other}
  end.     

  add(Key, Value, RequestId, Socket) ->
   %  io:format("in add of key:~p\n", [Key]),
    P = "{\"method\":\"add\", \"data\":[{\"key\":\"" ++ Key ++ "\",\"value\":\"",
    Payload = string:concat(P, Value) ++  "\"}]}",
    Len = string:len(Payload),
    H = "Content-Length:" ++ integer_to_list(Len) ++ "\r\n" ++ "Request-Id:" ++ RequestId ++ "\r\n\r\n" ++ Payload,
    gen_tcp:send(Socket, H).

  remove(Key, RequestId, Socket) ->
    io:format("in remove of key:~p\n", [Key]),
    Payload = "{\"method\":\"remove\", \"data\":[{\"key\":\"" ++ Key ++ "\"}]}",
    Len = string:len(Payload),
    H = "Content-Length:" ++ integer_to_list(Len) ++ "\r\n" ++ "Request-Id:" ++ RequestId ++ "\r\n\r\n" ++ Payload,
    gen_tcp:send(Socket, H).

  search(Key, RequestId, Socket) ->
    io:format("in search of key:~p\n", [Key]),
    Payload = "{\"method\":\"search\", \"data\":[{\"key\":\"" ++ Key ++ "\"}]}",
    Len = string:len(Payload),
    H = "Content-Length:" ++ integer_to_list(Len) ++ "\r\n" ++ "Request-Id:" ++ RequestId ++ "\r\n\r\n" ++ Payload,
    gen_tcp:send(Socket, H).

  processResponse(Pid, Resp) ->  
     case message:parse(Resp) of
       {ok, RequestId, Payload, Remaining} -> 
         io:format("send response back to pid:~p, resp:~p\n", [Pid, Payload]),
         Pid ! {search, RequestId, Payload},
         processResponse(Pid, Remaining);
       more_read ->
         more_read;        
       Other -> 
         Other 
     end.     
