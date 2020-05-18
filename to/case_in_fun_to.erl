-module(case_in_fun_to).

-compile(export_all).

-record(yecc, {
          shift_reduce = [],
          reduce_reduce = []
         }).

-record(t_union, {a=[], types = []}).	% union type 't1|...|tN'

-record(hash_state, {n_fragments, types = []}).	% union type 't1|...|tN'

%%Path: "/usr/lib/erlang/lib/edoc-0.11/src/edoc_parser.erl"
union([T]) -> 
    T;
union(Ts) ->
    #t_union{types = lists:reverse(Ts)}.


add_conflict(Conflict = {Symbol, StateN, _, {reduce, _, _, _}}, St) ->
    St#yecc{reduce_reduce=[{StateN, Symbol} | St#yecc.reduce_reduce]};
add_conflict(Conflict = {Symbol, StateN, _, {accept, _}}, St) ->
    St#yecc{reduce_reduce=[{StateN, Symbol} | St#yecc.reduce_reduce]};
add_conflict(Conflict = {Symbol, StateN, _, {shift, _, _}}, St) ->
    St#yecc{shift_reduce=[{StateN, Symbol} | St#yecc.shift_reduce]};
add_conflict(Conflict = {_Symbol, _StateN, {one_level_up, _, _}, _Confl}, St) ->
    St.


filter_outcome(Val = unclear) -> unclear;
filter_outcome(Val = aborted) -> aborted;
filter_outcome(Val = presume_abort) -> aborted;
filter_outcome(Val = committed) -> committed;
filter_outcome(Val = pre_commit) -> unclear.


%%Path: "/usr/lib/erlang/lib/mnesia-4.16.2/src/mnesia_frag_hash.erl"
match_spec_to_frag_numbers(#hash_state{n_fragments = N} = State, MatchSpec = [{HeadPat, _, _}]) when is_tuple(HeadPat), tuple_size(HeadPat) > 2 ->
    KeyPat = element(2, HeadPat),
    case has_var(KeyPat) of
        false ->
            [key_to_frag_number(State, KeyPat)];
        true ->
            lists:seq(1, N)
    end;
match_spec_to_frag_numbers(#hash_state{n_fragments = N} = State, MatchSpec) -> 
    lists:seq(1, N);
match_spec_to_frag_numbers(OldState, MatchSpec) ->
    State = convert_old_state(OldState),
    match_spec_to_frag_numbers(State, MatchSpec).



%%Path: "/usr/lib/erlang/lib/parsetools-2.1.8/src/leex.erl"
parse_rules(Ifile, NextLine = {ok, cODE_HEAD ++ _Rest,_}, Ms, REAs, As, N, St) ->
    parse_rules_end(Ifile, NextLine, REAs, As, St);
parse_rules(Ifile, NextLine = {ok,Chars,L0}, Ms, REAs, As, N, St) ->
    %%io:fwrite("~w: ~p~n", [L0,Chars]),
    case collect_rule(Ifile, Chars, L0) of
        {ok,Re,Atoks,L1} ->
            {ok,REA,A,St1} = parse_rule(Re, L0, Atoks, Ms, N, St),
            parse_rules(Ifile, nextline(Ifile, L1, St), Ms,
                        [REA|REAs], [A|As], N+1, St1);
        {error,E} -> add_error(E, St)
    end;
parse_rules(Ifile, NextLine = {eof,_}, Ms, REAs, As, N, St) ->
    parse_rules_end(Ifile, NextLine, REAs, As, St).
