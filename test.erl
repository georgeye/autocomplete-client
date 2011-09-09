-module(test).
-export([testAc/0]).

%% test 4 cases:
 % 1. add key without value
 % 2. add key with value
 % 3. remove and search
 % 4. search something not exist
testAc() ->
   P = autocomplete_clt:initClient(localhost, 5050, self()),
   io:format("Pid=~p\n", [P]),
   
   %% firest case - add key without value
   P ! {add, "testId1", "", "req-add1"},
   P ! {search, "testId1", "req-search1"},
   case getResponse("testId1", "testId1") of 
      true ->
        true;
      false ->
        io:format("failed to find testId1\n"),
        exit
   end,

   % second case -  add key with value
   P ! {add, "testId2", "testValue2", "req-add2"},
   P ! {search, "testId2", "req-search2"},
   case getResponse("testId2", "testValue2") of 
      true ->
        true;
      false ->
        io:format("failed to find testId2\n"),
        exit
   end,
   
   % third case - remove 
   P ! {remove, "testId2", "req-remove1"},
   P ! {search, "testId2", "req-search3"},
   case getResponse("testId2", "testValue2") of 
      true ->
        exit;
      false ->
        io:format("Successfully removed key=testId2\n")
   end,

   %  fourth casee -  search something not exist
   P ! {search, "testId2testid1testid3", "req-search3"},
   case getResponse("testId2testid1testid3", "testId2testid1testid3") of 
      true ->
        exit;
      false ->
        io:format("All tests pass ...\n")
   end.

getResponse(Key, Value) ->
   receive
     {search, Req, Resp} ->
       io:format("get response for req:~p, resp:~p\n", [Req, Resp]),
       matchRequest(Key, Value, Resp);
     _ ->
        false
   end.

matchRequest(_, _, []) ->
      false;
matchRequest(Key, Value, [H|T]) ->
   {Bkey, Bvalue } = H,
   case check(Key, Value, Bkey, Bvalue) of
      true ->
        true;
      false ->
        matchRequest(Key, Value, T)
    end.

check(Key, Value, Bkey, Bvalue) ->
   K1 = binary_to_list(Bkey),
   V1 = binary_to_list(Bvalue),
   if
      K1 =:= Key ->
        V1 =:= Value;
      K1 /= Key ->
        false
   end.     

