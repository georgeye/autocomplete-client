-module(test).
-export([constructAdd/2]).

 constructAdd(Key, Value) ->
    E = json_eep:json_to_term("[1,3.14,{\"key\":\"value\"}]").
