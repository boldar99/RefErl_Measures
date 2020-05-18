-module(case_in_case_to).

-compile(export_all).


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_dumper.erl"
caseincase1(Tab, Semantics, Storage, InPlace, InitBy) ->
    case {open_files(Tab, Semantics, Storage, InPlace, InitBy), Semantics} of
        {true, disc_copies} when Tab /= schema ->
            mnesia_log:append({mODULE , Tab}, {{Tab, Key}, Val, Op}),
            ok;
        {true, _} -> dets_insert(Op, Tab, Key, Val, Storage)
        ;
        {false, _} -> ignore
    end.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_dumper.erl"
caseincase2(D, InitBy, Rec, InPlace, InitBy, LogV) ->
    case {mnesia_recover:wait_for_decision(D, InitBy), InitBy} of
        {{Tid, committed}, _} ->
            do_insert_rec(Tid,
                mnesia_tm:new_cr_format(Rec), InPlace, InitBy, LogV);
        {{Tid, aborted}, startup} ->
            mnesia_schema:undo_prepare_commit(Tid,
                mnesia_tm:new_cr_format(Rec));
        {{Tid, aborted}, _} -> ok
    end. 

caseincase3(OrigKey, Tab, RowIndex, Next, Store) ->
    case {get(mnesia_activity_state), OrigKey, Next} of
        {{_Mod, Tid, #tidstore{store=Store}}, undefined, _} when element(1, Tid) =:= tid ->
            snmp_order_keys(Store, Tab, RowIndex, []);
        {{_Mod, Tid, #tidstore{store=Store}}, _, _} ->
            case ets_match(Store, {{Tab,OrigKey}, '_', '$1'}) of
                [] ->  snmp_order_keys(Store,Tab,RowIndex,[OrigKey]);
                Ops ->
                    case lists:last(Ops) of
                        [delete] -> snmp_get_next_index(Tab, Next);
                        _ -> snmp_order_keys(Store,Tab,RowIndex,[OrigKey])
                    end
            end;
        {_, _, endOfTable} ->
            endOfTable;
        _ -> {ok, Next}
    end.




