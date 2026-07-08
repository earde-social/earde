-module(realtime_gateway_ffi).
-export([unix_now/0, getenv/1]).

unix_now() ->
    erlang:system_time(second).

getenv(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.
