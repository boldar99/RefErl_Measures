-module(hd_tl_from).

-compile(export_all).

constrained_keys(Maps) ->
    lists:foldl(fun(TmpMap, AccKeys) ->
        [Key || Key <- AccKeys, maps:is_key(Key, TmpMap)]
                end,
        maps:keys(hd(Maps)), tl(Maps)).

parse_preprocessed_file(Epp, File, InCorrectFile) ->
    case epp:parse_erl_form(Epp) of
        {ok,Form} ->
            case Form of
                {attribute,_,file,{File,_}} ->
                    parse_preprocessed_file(Epp, File, true);
                {attribute,_,file,{_OtherFile,_}} ->
                    parse_preprocessed_file(Epp, File, false);
                {function,L,F,A,Cs} when InCorrectFile ->
                    {CLs,LastCL} = find_clause_lines(Cs, []),
                    %% tl(CLs) cause we know the start line already
                    [{atom_to_list(F),A,get_line(L),LastCL} | tl(CLs)] ++
                    parse_preprocessed_file(Epp, File, true);
                _ ->
                    parse_preprocessed_file(Epp, File, InCorrectFile)
            end;
        {error,Reason={_L,epp,{undefined,_Macro,none}}} ->
            throw({error,Reason,InCorrectFile});
        {error,_Reason} ->
            parse_preprocessed_file(Epp, File, InCorrectFile);
        {warning,_} ->
            parse_preprocessed_file(Epp, File, InCorrectFile);
        {eof,_Location} ->
            []
    end.


build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function,anno(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.



cluster_split(0, [], Res, Cases, _Clust) ->
    L = lists:reverse(Cases),
    {H,_} = hd(L),
    {T,_} = hd(Cases),
    [{dense,hipe_icode:const_value(H),hipe_icode:const_value(T),L}|Res];
cluster_split(N, [], Res, Cases, [N|_] = Clust) ->
    cluster_split(N-1, [], Res, Cases, Clust);
cluster_split(N,Sofar,Res,Cases,[N|Clust]) ->
    {H,_} = hd(Sofar),
    {T,_} = lists:last(Sofar),
    cluster_split(N-1,[],[{dense,hipe_icode:const_value(H),hipe_icode:const_value(T),Sofar}|Res],Cases,[N|Clust]);
cluster_split(N,Sofar,Res,[C|Cases],[_|Clust]) ->
    cluster_split(N,[C|Sofar],Res,Cases,Clust).


find_cycle(Moves, Cycle) ->
    To = lists:nth(lists:last(Cycle), Moves),
    if To =:= hd(Cycle) -> Cycle;
        true -> find_cycle(Moves, Cycle ++ [To])
    end.
