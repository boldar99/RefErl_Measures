-module(if_in_if_to).

-compile(export_all).

%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_controller.erl"
ifinif1() ->
    if
        Copies == [node()] ->
            %% Only one copy holder and it is local.
            %% It may also be a local contents table
            {true, {Tab, local_only}};
        LocalContent == true ->
            {true, {Tab, local_content}};
        LocalMaster == true ->
            %% We have a local master
            {true, {Tab, local_master}};
        RemoteMaster == true ->
            %% Wait for remote master copy
            false;
        (Storage == ram_copies) and ((Disc == []) andalso
            ((DiscOnly == []) andalso (Ext == []))) -> %% Nobody has copy on disc
            {true, {Tab, ram_only}};
        Storage == ram_copies -> %% Some other node has copy on disc
            false;
        AccessMode == read_only ->
            %% No one has been able to update the table,
            %% i.e. all disc resident copies are equal
            {true, {Tab, read_only}};
        BetterCopies /= [], Masters /= [node()] ->
            %% There are better copies on other nodes
            %% and we do not have the only master copy
            false;
        true ->
            {true, {Tab, initial}}
    end.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_schema.erl"
ifinif2() ->
    if
        (Tab == schema) and (Storage /= ram_copies) ->
            mnesia:abort({badarg, Tab, Storage});
        (Tab == schema) and (IsRunning == true) ->
            mnesia:abort({already_exists, Tab, Node});
        Tab == schema -> ignore;
        Storage == ram_copies ->
            ignore;
        IsRunning == true ->
            ignore;
        IsRunning == false ->
            mnesia:abort({not_active, schema, Node})
    end.
