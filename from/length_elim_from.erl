-module(length_elim_from).

-compile(export_all).

-record(t_paren, {a=[], types = []}).

%% Unexpeted term in group list
find(Mod, _GrNames, _TCs, [BadTerm | _Gs], Known, _Defs, _FindAll) ->
    Where = if length(Known) == 0 ->
        atom_to_list(Mod)++":groups/0";
                true ->
                    "group "++atom_to_list(lists:last(Known))++
                        " in "++atom_to_list(Mod)++":groups/0"
            end,
    Term = io_lib:format("~tp", [BadTerm]),
    E = "Bad term "++lists:flatten(Term)++" in "++Where,
    throw({error,list_to_atom(E)});
%% No more groups
find(_Mod, _GrNames, _TCs, [], _Known, _Defs, _) ->
    [].


number_to_hex(N) ->
    case string:to_lower(erlang:integer_to_list(N, 16)) of
        H when length(H) < 2 ->
            lists:append(H, "0");
        H ->
            lists:reverse(H)
    end.


wait_for_port_reply(Socket, SoFar) ->
    receive
        {tcp, Socket, Data0} ->
%	    io:format("got ~p~n", [Data0]),
            case SoFar ++ Data0 of
                [$w, Result | Rest] ->
                    case Result of
                        0 ->
                            wait_for_port_reply_cont(Socket, Rest);
                        _ ->
                            port_please_failure(),
                            wait_for_close(Socket, noport)
                    end;
                Data when length(Data) < 2 ->
                    wait_for_port_reply(Socket, Data);
                Garbage ->
                    port_please_failure(),
                    {error, {garbage_from_epmd, Garbage}}
            end;
        {tcp_closed, Socket} ->
            port_please_failure(),
            closed
    after 10000 ->
        port_please_failure(),
        gen_tcp:close(Socket),
        noport
    end.

type(maps, from_list, 1, Xs, Opaques) ->
    strict(maps, from_list, 1, Xs,
        fun ([List]) ->
            case t_is_nil(List, Opaques) of
                true -> t_from_term(#{});
                false ->
                    T = t_list_elements(List, Opaques),
                    case t_tuple_subtypes(T, Opaques) of
                        unknown -> t_map();
                        Stypes when length(Stypes) >= 1 ->
                            t_sup([begin
                                       [K, V] = t_tuple_args(Args, Opaques),
                                       t_map([], K, V)
                                   end || Args <- Stypes])
                    end
            end
        end, Opaques).

lengthelim5() ->
    if length(element(1, '$1')) == 1 ->
            %% there must be exactly one utype in the list
            #t_paren{type = hd(element(1, '$1'))};
        length(element(1, '$1')) == 0 ->
            return_error(element(2, '$1'), "syntax error before: ')'");
        true ->
            return_error(element(2, '$1'), "syntax error before: ','")
    end.