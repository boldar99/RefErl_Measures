-module(case_in_case_from).

-compile(export_all).


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_dumper.erl"
caseincase1() ->
    case open_files(Tab, Semantics, Storage, InPlace, InitBy) of
        true ->
            case Semantics of
                disc_copies when Tab /= schema ->
                    mnesia_log:append({mODULE,Tab}, {{Tab, Key}, Val, Op}),
                    ok;
                _ ->
                    dets_insert(Op,Tab,Key,Val,Storage)
            end;
        false ->
            ignore
    end.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_dumper.erl"
caseincase2() ->
    case mnesia_recover:wait_for_decision(D, InitBy) of
        {Tid, committed} ->
            do_insert_rec(Tid, mnesia_tm:new_cr_format(Rec), InPlace, InitBy, LogV);
        {Tid, aborted} ->
            case InitBy of
                startup ->
                    mnesia_schema:undo_prepare_commit(Tid, mnesia_tm:new_cr_format(Rec));
                _ ->
                    ok
            end
    end.

caseincase3() ->
    case get(mnesia_activity_state) of
        {_Mod, Tid, #tidstore{store=Store}} when element(1, Tid) =:= tid ->
            case OrigKey of
                undefined ->
                    snmp_order_keys(Store, Tab, RowIndex, []);
                _ ->
                    case ets_match(Store, {{Tab,OrigKey}, '_', '$1'}) of
                        [] ->  snmp_order_keys(Store,Tab,RowIndex,[OrigKey]);
                        Ops ->
                            case lists:last(Ops) of
                                [delete] -> snmp_get_next_index(Tab, Next);
                                _ -> snmp_order_keys(Store,Tab,RowIndex,[OrigKey])
                            end
                    end
            end;
        _ ->
            case Next of
                endOfTable -> endOfTable;
                _ -> {ok, Next}
            end
    end.



