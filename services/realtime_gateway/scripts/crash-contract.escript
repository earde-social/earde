%% Kills one process linked to the gateway's main process in a running
%% `gleam run` node, as an essential subsystem crash would.
%% Usage: escript scripts/crash-contract.escript <node> <cookie>
main([NodeName, Cookie]) ->
    {ok, _} = net_kernel:start([list_to_atom("crash_probe_" ++ os:getpid()), shortnames]),
    erlang:set_cookie(list_to_atom(Cookie)),
    Node = list_to_atom(NodeName),
    pong = net_adm:ping(Node),
    Info = fun(P, Key) -> rpc:call(Node, erlang, process_info, [P, Key]) end,
    %% main runs inside the frame Gleam's generated runner starts it from.
    [Main] = [P || P <- rpc:call(Node, erlang, processes, []),
                   case Info(P, current_stacktrace) of
                       {current_stacktrace, Stack} ->
                           lists:any(fun({_, F, _, _}) -> F =:= run_module end, Stack);
                       _ -> false
                   end],
    {links, Links} = Info(Main, links),
    [Victim | _] = [P || P <- Links, is_pid(P),
                         Info(P, trap_exit) =:= {trap_exit, false}],
    io:format("killing ~p, linked to main ~p~n", [Victim, Main]),
    rpc:call(Node, erlang, exit, [Victim, kill]),
    ok.
