-module(case_in_fun_from).

-compile(export_all).

-record(yecc, {
          shift_reduce = [],
          reduce_reduce = []
         }).

-record(t_union, {a=[], types = []}).	% union type 't1|...|tN'

-record(hash_state, {n_fragments, types = []}).	% union type 't1|...|tN'

%%Path: "/usr/lib/erlang/lib/edoc-0.11/src/edoc_parser.erl"
union(Ts) ->
    case Ts of
        [T] -> T;
        _ -> #t_union{types = lists:reverse(Ts)}
    end.


%%Path: "/usr/lib/erlang/lib/parsetools-2.1.8/src/yecc.erl"
add_conflict(Conflict, St) ->
    case Conflict of
        {Symbol, StateN, _, {reduce, _, _, _}} ->
            St#yecc{reduce_reduce = [{StateN,Symbol} |St#yecc.reduce_reduce]};
        {Symbol, StateN, _, {accept, _}} ->
            St#yecc{reduce_reduce = [{StateN,Symbol} |St#yecc.reduce_reduce]};
        {Symbol, StateN, _, {shift, _, _}} ->
            St#yecc{shift_reduce = [{StateN,Symbol} | St#yecc.shift_reduce]};
        {_Symbol, _StateN, {one_level_up, _, _}, _Confl} ->
            St
    end.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_recover.erl"
filter_outcome(Val) ->
    case Val of
        unclear -> unclear;
        aborted -> aborted;
        presume_abort -> aborted;
        committed -> committed;
        pre_commit -> unclear
    end.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_frag_hash.erl"
match_spec_to_frag_numbers(#hash_state{n_fragments = N} = State, MatchSpec) ->
    case MatchSpec of
        [{HeadPat, _, _}] when is_tuple(HeadPat), tuple_size(HeadPat) > 2 ->
            KeyPat = element(2, HeadPat),
            case has_var(KeyPat) of
                false ->
                    [key_to_frag_number(State, KeyPat)];
                true ->
                    lists:seq(1, N)
            end;
        _ ->
            lists:seq(1, N)
    end;
match_spec_to_frag_numbers(OldState, MatchSpec) ->
    State = convert_old_state(OldState),
    match_spec_to_frag_numbers(State, MatchSpec).



%%Path: "/usr/lib/erlang/lib/parsetools-2.1.8/src/leex.erl"
parse_rules(Ifile, NextLine, Ms, REAs, As, N, St) ->
    case NextLine of
        {ok,vODE_HEAD ++ _Rest,_} ->
            parse_rules_end(Ifile, NextLine, REAs, As, St);
        {ok,Chars,L0} ->
            %%io:fwrite("~w: ~p~n", [L0,Chars]),
            case collect_rule(Ifile, Chars, L0) of
                {ok,Re,Atoks,L1} ->
                    {ok,REA,A,St1} = parse_rule(Re, L0, Atoks, Ms, N, St),
                    parse_rules(Ifile, nextline(Ifile, L1, St), Ms,
                        [REA|REAs], [A|As], N+1, St1);
                {error,E} -> add_error(E, St)
            end;
        {eof,_} ->
            parse_rules_end(Ifile, NextLine, REAs, As, St)
    end.
