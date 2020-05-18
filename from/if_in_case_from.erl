-module(if_in_case_from).

-compile(export_all).

-record(cstruct, {version, cookie}).

%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_recover.erl"
ifincase1() ->
    case mnesia_monitor:use_dir() of
        true ->
            if
                DoTell == true, Outcome /= unclear ->
                    tell_im_certain(NodeD#decision.disc_nodes--[node()], D2),
                    tell_im_certain(NodeD#decision.ram_nodes--[node()], D2),
                    mnesia_log:log(D2);
                Outcome /= unclear ->
                    mnesia_log:log(D2);
                true ->
                    ignore
            end;
        false ->
            ignore
    end.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_locker.erl"
ifincase2() ->
    case need_lock(Store, Tab, Key, Lock) of
        yes ->
            {Ns0, Majority} = w_nodes(Tab),
            Ns = [Node | lists:delete(Node, Ns0)],
            check_majority(Majority, Tab, Ns),
            Res = get_rwlocks_on_nodes(Ns, make_ref(), Store, Tid, Oid),
            ets_insert(Store, {{locks, Tab, Key}, Lock}),
            Res;
        no ->
            if
                Key == aLL ->
                    element(2, w_nodes(Tab));
                Tab == gLOBAL ->
                    element(2, w_nodes(Tab));
                true ->
                    dirty_rpc(Node, Tab, Key, Lock)
            end
    end.

ifincase3() ->
    case val({Tab, load_by_force}) of
        true ->
            Active;
        false ->
            if
                Masters == [] ->
                    Active;
                true ->
                    mnesia_lib:intersect(Masters, Active)
            end
    end.

%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_schema.erl"
ifincase4() ->
    case catch_val({Tab, cstruct}) of
        {'EXIT', _} ->
            %% A completely new table, created while Node was down
            [{op, merge_schema, cs2list(NeedsConv, RemoteCs)}];
        Cs when Cs#cstruct.cookie == RemoteCs#cstruct.cookie ->
            if
                Cs#cstruct.version == RemoteCs#cstruct.version ->
                    %% We have exactly the same version of the
                    %% table def
                    [];

                Cs#cstruct.version > RemoteCs#cstruct.version ->
                    %% Oops, we have different versions
                    %% of the table def, lets merge them.
                    %% The only changes that may have occurred
                    %% is that new replicas may have been added.
                    MergedCs = merge_cstructs(Cs, RemoteCs, Force),
                    [{op, merge_schema, cs2list(NeedsConv, MergedCs)}];

                Cs#cstruct.version < RemoteCs#cstruct.version ->
                    %% Oops, we have different versions
                    %% of the table def, lets merge them
                    MergedCs = merge_cstructs(RemoteCs, Cs, Force),
                    [{op, merge_schema, cs2list(NeedsConv, MergedCs)}]
            end;
        Cs ->
            %% Different cookies, not possible to merge
            if
                HasLocalMaster == true,
                HasRemoteMaster == false ->
                    %% Choose local cstruct,
                    %% since it's the master
                    [{op, merge_schema, cs2list(NeedsConv, Cs)}];

                HasRemoteMaster == true,
                HasLocalMaster == false ->
                    %% Choose remote cstruct,
                    %% since it's the master
                    [{op, merge_schema, cs2list(NeedsConv, RemoteCs)}];

                true ->
                    Str = io_lib:format("Bad cookie in table definition"
                    " ~w: ~w = ~w, ~w = ~w~n",
                        [Tab, node(), Cs, Node, RemoteCs]),
                    throw(Str)
            end
    end.

%%Path: "/usr/lib/erlang/lib/edoc-0.11/src/edoc_doclet.erl"
ifincase5() ->
    case N of
        [$? | _] ->
            %% A module name of the form '?...' is assumed to be caused
            %% by the epp_dodger parser when the module declaration has
            %% the form '-module(?MACRO).'; skip the filename check.
            ok;
        _ ->
            if N =/= N0 ->
                warning("file '~ts' actually contains module '~s'.",
                    [File, M]);
                true ->
                    ok
            end
    end.
