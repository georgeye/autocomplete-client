-module(test).
-export([testAc/0]).


testAc() ->
   P = autocomplete_clt:initClient(localhost, 5050, self()),
   io:format("Pid=~p\n", [P]),
   P ! {search, "apple", "req1"},
   P ! {search, "ap", "req2"},
   P ! {search, "gye", "req3"},
   P ! {search, "app", "req4"},
   loop().

loop() ->  
   receive
     {search, Req, Resp} ->
       io:format("get response for req:~p, resp:~p\n", [Req, Resp]),
       loop();
     Other ->
       Other
   end.    

sleep(T) ->
  receive
    after
      T ->
        true
  end.

