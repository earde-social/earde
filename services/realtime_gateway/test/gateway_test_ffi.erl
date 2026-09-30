%% Test-only helpers for the recovery tests: process introspection and a
%% minimal WebSocket client over gen_tcp (text frames, no extensions), so the
%% tests drive the real server without extra dependencies.
-module(gateway_test_ffi).
-export([links/1, listener_owner/1, connection_owner/2, wait_down/2,
         http_status/2, http_post/4, ws_connect/3, ws_send/2, ws_recv/2, ws_close/1,
         putenv/2, now_ms/0]).

links(Pid) ->
    case erlang:process_info(Pid, links) of
        {links, Links} -> [P || P <- Links, is_pid(P)];
        undefined -> []
    end.

%% The process that owns the listening socket on 127.0.0.1:Port.
listener_owner(Port) ->
    owner_of(fun(P) ->
        inet:sockname(P) =:= {ok, {{127, 0, 0, 1}, Port}}
            andalso element(1, inet:peername(P)) =:= error
    end).

%% The server-side process that owns the connection behind Client.
connection_owner(Port, Client) ->
    {ok, Local} = inet:sockname(Client),
    owner_of(fun(P) ->
        inet:sockname(P) =:= {ok, {{127, 0, 0, 1}, Port}}
            andalso inet:peername(P) =:= {ok, Local}
    end).

owner_of(Match) ->
    Owners = [Owner || P <- erlang:ports(),
                       erlang:port_info(P, name) =:= {name, "tcp_inet"},
                       Match(P),
                       {connected, Owner} <- [erlang:port_info(P, connected)]],
    case Owners of
        [Owner | _] -> {ok, Owner};
        [] -> {error, nil}
    end.

wait_down(Pid, TimeoutMs) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> true
    after TimeoutMs ->
        erlang:demonitor(Ref, [flush]),
        false
    end.

http_status(Port, Path) ->
    case gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 1000) of
        {ok, S} ->
            Sent = gen_tcp:send(S, [<<"GET ">>, Path,
                                    <<" HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n">>]),
            Result = case Sent =:= ok andalso gen_tcp:recv(S, 0, 2000) of
                {ok, <<"HTTP/1.1 ", Code:3/binary, _/binary>>} ->
                    {ok, binary_to_integer(Code)};
                _ -> {error, nil}
            end,
            gen_tcp:close(S),
            Result;
        {error, _} ->
            {error, nil}
    end.

http_post(Port, Path, Secret, Body) ->
    case gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 1000) of
        {ok, S} ->
            Sent = gen_tcp:send(S, [<<"POST ">>, Path, <<" HTTP/1.1\r\nHost: localhost\r\n">>,
                                  <<"Content-Type: application/json\r\nx-earde-internal: ">>, Secret,
                                  <<"\r\nContent-Length: ">>, integer_to_binary(byte_size(Body)),
                                  <<"\r\nConnection: close\r\n\r\n">>, Body]),
            Result = case Sent =:= ok andalso gen_tcp:recv(S, 0, 2000) of
                {ok, <<"HTTP/1.1 ", Code:3/binary, _/binary>>} ->
                    {ok, binary_to_integer(Code)};
                _ -> {error, nil}
            end,
            gen_tcp:close(S),
            Result;
        {error, _} ->
            {error, nil}
    end.

ws_connect(Port, Target, Origin) ->
    case gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 1000) of
        {ok, S} ->
            Key = base64:encode(crypto:strong_rand_bytes(16)),
            ok = gen_tcp:send(S, [<<"GET ">>, Target, <<" HTTP/1.1\r\n">>,
                                  <<"Host: localhost\r\nUpgrade: websocket\r\n">>,
                                  <<"Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n">>,
                                  <<"Origin: ">>, Origin, <<"\r\n">>,
                                  <<"Sec-WebSocket-Key: ">>, Key, <<"\r\n\r\n">>]),
            case read_head(S, <<>>) of
                {ok, <<"HTTP/1.1 101", _/binary>>, Rest} ->
                    put({ws_buffer, S}, Rest),
                    {ok, S};
                _ ->
                    gen_tcp:close(S),
                    {error, nil}
            end;
        {error, _} ->
            {error, nil}
    end.

read_head(S, Acc) ->
    case binary:split(Acc, <<"\r\n\r\n">>) of
        [Head, Rest] -> {ok, Head, Rest};
        [_] ->
            case gen_tcp:recv(S, 0, 2000) of
                {ok, More} -> read_head(S, <<Acc/binary, More/binary>>);
                Error -> Error
            end
    end.

ws_send(S, Text) ->
    Mask = crypto:strong_rand_bytes(4),
    Len = byte_size(Text),
    LenBits = if
        Len < 126 -> <<1:1, Len:7>>;
        Len < 65536 -> <<1:1, 126:7, Len:16>>;
        true -> <<1:1, 127:7, Len:64>>
    end,
    gen_tcp:send(S, [<<1:1, 0:3, 1:4>>, LenBits, Mask, mask(Text, Mask)]),
    nil.

mask(Data, <<M:32>>) -> mask(Data, M, <<>>).
mask(<<B:32, Rest/binary>>, M, Acc) -> mask(Rest, M, <<Acc/binary, (B bxor M):32>>);
mask(Tail, M, Acc) ->
    Bits = bit_size(Tail),
    <<B:Bits>> = Tail,
    <<MTail:Bits, _/bitstring>> = <<M:32>>,
    <<Acc/binary, (B bxor MTail):Bits>>.

%% The next text frame, skipping control frames other than close.
ws_recv(S, TimeoutMs) ->
    case take(S, 2, TimeoutMs) of
        {ok, <<_Fin:1, _:3, Op:4, 0:1, Len0:7>>} ->
            Len = case Len0 of
                126 -> {ok, <<L:16>>} = take(S, 2, TimeoutMs), L;
                127 -> {ok, <<L:64>>} = take(S, 8, TimeoutMs), L;
                _ -> Len0
            end,
            {ok, Payload} = take(S, Len, TimeoutMs),
            case Op of
                1 -> {ok, Payload};
                8 -> {error, <<"closed">>};
                _ -> ws_recv(S, TimeoutMs)
            end;
        {error, Reason} ->
            {error, atom_to_binary(Reason)}
    end.

take(_S, 0, _TimeoutMs) -> {ok, <<>>};
take(S, N, TimeoutMs) ->
    Buffer = case get({ws_buffer, S}) of undefined -> <<>>; B -> B end,
    case Buffer of
        <<Wanted:N/binary, Rest/binary>> ->
            put({ws_buffer, S}, Rest),
            {ok, Wanted};
        _ ->
            case gen_tcp:recv(S, 0, TimeoutMs) of
                {ok, More} ->
                    put({ws_buffer, S}, <<Buffer/binary, More/binary>>),
                    take(S, N, TimeoutMs);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

ws_close(S) ->
    gen_tcp:close(S),
    erase({ws_buffer, S}),
    nil.

putenv(Name, Value) ->
    os:putenv(binary_to_list(Name), binary_to_list(Value)),
    nil.

now_ms() ->
    erlang:monotonic_time(millisecond).
