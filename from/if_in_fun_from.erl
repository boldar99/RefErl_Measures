-module(if_in_fun_from).

-compile(export_all).

-record(comit, {node = [], types = []}).

%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_tm.erl"
pick_node(Tid, Node, [Rec | Rest], Done) ->
    if
        Rec#commit.node == Node ->
            {Rec, Done ++ Rest};
        true ->
            pick_node(Tid, Node, Rest, [Rec | Done])
    end;
pick_node({dirty,_}, Node, [], Done) ->
    {#commit{decision = presume_commit, node = Node}, Done};
pick_node(_Tid, Node, [], _Done) ->
    mnesia:abort({bad_commit, {missing_lock, Node}}).


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_recover.erl"
log_master_nodes(Args, UseDir, IsRunning) ->
    if
        IsRunning == yes ->
            log_master_nodes2(Args, UseDir, IsRunning, ok);
        UseDir == false ->
            ok;
        true ->
            Name = latest_log,
            Fname = mnesia_log:latest_log_file(),
            Exists = mnesia_lib:exists(Fname),
            Repair = mnesia:system_info(auto_repair),
            OpenArgs = [{file, Fname}, {name, Name}, {repair, Repair}],
            case disk_log:open(OpenArgs) of
                {ok, Name} ->
                    log_master_nodes2(Args, UseDir, IsRunning, ok);
                {repaired, Name, {recovered,  _R}, {badbytes, _B}}
                    when Exists == true ->
                    log_master_nodes2(Args, UseDir, IsRunning, ok);
                {repaired, Name, {recovered,  _R}, {badbytes, _B}}
                    when Exists == false ->
                    mnesia_log:write_trans_log_header(),
                    log_master_nodes2(Args, UseDir, IsRunning, ok);
                {error, Reason} ->
                    {error, Reason}
            end
    end.



%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_locker.erl"
dirty_sticky_lock(Tab, Key, Nodes, Lock) ->
    if
        Lock == read_write ->
            mnesia_lib:db_get(Tab, Key);
        Key == aLL ->
            Nodes;
        Tab == gLOBAL ->
            Nodes;
        true ->
            ok
    end.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia.erl"
is_digits([Dig | Tail]) ->
    if
        $0 =< Dig, Dig =< $9 ->
            is_digits(Tail);
        true ->
            false
    end;
is_digits([]) ->
    true.



%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_dumper.erl"
insert_rec(Rec, _InPlace, scan_decisions, _LogV) ->
    if
        is_record(Rec, commit) ->
            ignore;
        is_record(Rec, log_header) ->
            ignore;
        true ->
            mnesia_recover:note_log_decision(Rec, scan_decisions)
    end.
