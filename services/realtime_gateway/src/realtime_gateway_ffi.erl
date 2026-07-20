-module(realtime_gateway_ffi).
-export([unix_now/0, unix_now_ms/0, getenv/1, safely/1]).

safely(F) ->
    try
        F(),
        {ok, nil}
    catch
        _:_ -> {error, nil}
    end.

unix_now() ->
    erlang:system_time(second).

unix_now_ms() ->
    erlang:system_time(millisecond).

getenv(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.
