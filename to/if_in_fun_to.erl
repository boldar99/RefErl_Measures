-module(if_in_fun_to).

-compile(export_all).

-record(comit, {node = [], types = []}).

%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_tm.erl"
pick_node(Tid, Node, [Rec | Rest], Done) when Rec#commit.node == Node ->
            {Rec, Done ++ Rest};
pick_node(Tid, Node, [Rec | Rest], Done) ->
    pick_node(Tid, Node, Rest, [Rec | Done]);
pick_node({dirty,_}, Node, [], Done) ->
    {#commit{decision = presume_commit, node = Node}, Done};
pick_node(_Tid, Node, [], _Done) ->
    mnesia:abort({bad_commit, {missing_lock, Node}}).


log_master_nodes(Args, UseDir, IsRunning) when IsRunning == yes ->
    log_master_nodes2(Args, UseDir, IsRunning, ok);
log_master_nodes(Args, UseDir, IsRunning) when UseDir == false -> ok;
log_master_nodes(Args, UseDir, IsRunning) ->
    Name = latest_log,
    Fname = mnesia_log:latest_log_file(),
    Exists = mnesia_lib:exists(Fname),
    Repair = mnesia:system_info(auto_repair),
    OpenArgs = [{file, Fname}, {name, Name}, {repair, Repair}],
    case disk_log:open(OpenArgs) of
        {ok, Name} ->
            log_master_nodes2(Args, UseDir, IsRunning, ok);
        {repaired, Name, {recovered, _R}, {badbytes, _B}}
        when Exists == true ->
            log_master_nodes2(Args, UseDir, IsRunning, ok);
        {repaired, Name, {recovered, _R}, {badbytes, _B}}
        when Exists == false ->
            mnesia_log:write_trans_log_header(),
            log_master_nodes2(Args, UseDir, IsRunning, ok);
        {error, Reason} ->
            {error, Reason}
    end.


dirty_sticky_lock(Tab, Key, Nodes, Lock) when Lock == read_write ->
    mnesia_lib:db_get(Tab, Key);
dirty_sticky_lock(Tab, Key, Nodes, Lock) when Key == aLL -> Nodes;
dirty_sticky_lock(Tab, Key, Nodes, Lock) when Tab == gLOBAL -> Nodes;
dirty_sticky_lock(Tab, Key, Nodes, Lock) -> ok.


is_digits([Dig]) when (48 =< Dig) andalso (Dig =< 57) -> is_digits(Tail);
is_digits([Dig]) -> false;
is_digits([]) ->
    true.


insert_rec(Rec, _InPlace, scan_decisions, _LogV) when is_record(Rec, commit) ->
    ignore;
insert_rec(Rec, _InPlace, scan_decisions, _LogV) when
        is_record(Rec, log_header) ->
    ignore;
insert_rec(Rec, _InPlace, scan_decisions, _LogV) ->
    mnesia_recover:note_log_decision(Rec, scan_decisions).
