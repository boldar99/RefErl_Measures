-module(case_in_if_to).

-compile(export_all).

-record(t_spec, {name}).

-record(t_name, {module = [], name = []}).

%%Path: "/usr/lib/erlang/lib/edoc-0.11/src/edoc_tags.erl"
caseinif1() ->
    case N of 
        _ when length(As) /= A ->
            throw_error(Line, "@spec arity does not match.");
        undefined ->
            Spec#t_spec{name = #t_name{module = [], name = F}};
        #t_name{module = [], name = F} ->
            Spec;
        _ ->
            throw_error(Line, "@spec name does not match.")
    end.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_schema.erl"
caseinif2() ->
    case Type of
        _ when size(NewObj) /= NewArity -> exit({"Bad arity", Obj, NewObj});
        _ when NewObj == Obj ->
            transform_obj(Tab, RecName, Key, Fun, Rest,
                NewArity, Type, Ws, Ds);
        _ when (RecName == element(1, NewObj)) andalso
        (Key == element(2, NewObj)) ->
            transform_obj(Tab, RecName, Key, Fun, Rest, NewArity,
                Type, [NewObj | Ws], Ds);
        bag when NewObj == delete ->
            transform_obj(Tab, RecName, Key, Fun, Rest,
                NewArity, Type, Ws, Ds);
        _ when NewObj == delete ->
            transform_obj(Tab, RecName, Key, Fun, Rest, NewArity,
                Type, Ws, [NewObj | Ds]);
        _ -> exit({"Bad key or Record Name", Obj, NewObj})
    end.

caseinif3() ->
    case {FromS, ToS} of
        _ when NotActive == true -> mnesia:abort({not_active, Tab, node()});
        {ram_copies, disc_copies} when Tab == schema ->
            case mnesia:system_info(schema_location) of
                opt_disc ->
                    ignore;
                _ ->
                    mnesia:abort({combine_error, Tab, node(),
                        "schema_location must be opt_disc"})
            end,
            Dir = mnesia_lib:dir(),
            case opt_create_dir(true, Dir) of
                ok ->
                    purge_dir(Dir, []),
                    mnesia_log:purge_all_logs(),
                    set(use_dir, true),
                    mnesia_log:init(),
                    Ns = val({current, db_nodes}), %mnesia_lib:running_nodes(),
                    F = fun(U) -> mnesia_recover:log_mnesia_up(U) end,
                    lists:foreach(F, Ns),

                    mnesia_dumper:raw_named_dump_table(Tab, dmp),
                    mnesia_checkpoint:tm_change_table_copy_type(Tab, FromS,
                        ToS);
                {error, Reason} ->
                    mnesia:abort(Reason)
            end;
        {disc_copies, ram_copies} when Tab == schema ->
            Ltabs = val({schema, local_tables}) -- [schema],
            Dtabs = [L || L<-Ltabs,
                val({L, storage_type}) /= ram_copies],
            verify([], Dtabs, {"Disc resident tables", Dtabs, N});
        _ when Tab == schema -> mnesia:abort({combine_error, Tab, ToS});
        _ when (
        element(1, FromS) == ext) orelse (element(1, ToS) == ext) ->
            if
                ToS == ram_copies ->
                    create_ram_table(Tab, Cs);
                true ->
                    ok
            end,
            mnesia_dumper:dump_to_logfile(FromS, Tab),
            mnesia_checkpoint:tm_change_table_copy_type(Tab, FromS, ToS);
        _ when FromS == ram_copies ->
            case mnesia_monitor:use_dir() of
                true ->
                    Dat = mnesia_lib:tab2dcd(Tab),
                    case mnesia_lib:exists(Dat) of
                        true ->
                            mnesia:abort({combine_error, Tab, node(),
                                "Table dump exists"});
                        false ->
                            case ToS of
                                disc_copies ->
                                    mnesia_log:ets2dcd(Tab, dmp);
                                disc_only_copies ->
                                    mnesia_dumper:raw_named_dump_table(Tab, dmp)
                            end,
                            mnesia_checkpoint:tm_change_table_copy_type(Tab,
                                FromS,
                                ToS)
                    end;
                false ->
                    mnesia:abort({has_no_disc, node()})
            end;
        _ when (FromS == disc_copies) andalso (ToS == disc_only_copies) ->
            mnesia_dumper:raw_named_dump_table(Tab, dmp);
        _ when FromS == disc_only_copies ->
            Type = Cs#cstruct.type,
            create_ram_table(Tab, Cs),
            Datname = mnesia_lib:tab2dat(Tab),
            Repair = mnesia_monitor:get_env(auto_repair),
            case mnesia_lib:dets_to_ets(Tab, Tab, Datname, Type, Repair, no) of
                loaded -> ok;
                Reason ->
                    Err = "Failed to copy disc data to ram",
                    mnesia:abort({system_limit, Tab, {Err, Reason}})
            end;
        _ -> ignore
    end.
