-module(hd_tl_to).

-compile(export_all).

constrained_keys(Maps = [Maps_Hd|Maps_Tl]) ->
    lists:foldl(fun(TmpMap, AccKeys) ->
        [Key || Key <- AccKeys, maps:is_key(Key, TmpMap)]
                end,
        maps:keys(Maps_Hd), Maps_Tl).

parse_preprocessed_file(Epp, File, InCorrectFile) ->
    case epp:parse_erl_form(Epp) of
        {ok,Form} ->
            case Form of
                {attribute,_,file,{File,_}} ->
                    parse_preprocessed_file(Epp, File, true);
                {attribute,_,file,{_OtherFile,_}} ->
                    parse_preprocessed_file(Epp, File, false);
                {function,L,F,A,Cs} when InCorrectFile ->
                    {CLs=[_|Cls_Tl],LastCL} = find_clause_lines(Cs, []),
                    %% tl(CLs) cause we know the start line already
                    [{atom_to_list(F),A,get_line(L),LastCL} | Cls_Tl] ++
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


build_function(Cs=[Cs_Hd|_]) ->
    Name = element(3, Cs_Hd),
    Arity = length(element(4, Cs_Hd)),
    {function,anno(Cs_Hd),Name,Arity,check_clauses(Cs, Name, Arity)}.



cluster_split(0, [], Res, Cases = [Cases_Hd|_], _Clust) ->
    L = [L_Hd|_] = lists:reverse(Cases),
    {H,_} = L_Hd,
    {T,_} = Cases_Hd,
    [{dense,hipe_icode:const_value(H),hipe_icode:const_value(T),L}|Res];
cluster_split(N, [], Res, Cases, [N|_] = Clust) ->
    cluster_split(N-1, [], Res, Cases, Clust);
cluster_split(N,Sofar = [Sofar_Hd|_],Res,Cases,[N|Clust]) ->
    {H,_} = Sofar_Hd,
    {T,_} = lists:last(Sofar),
    cluster_split(N-1,[],[{dense,hipe_icode:const_value(H),hipe_icode:const_value(T),Sofar}|Res],Cases,[N|Clust]);
cluster_split(N,Sofar,Res,[C|Cases],[_|Clust]) ->
    cluster_split(N,[C|Sofar],Res,Cases,Clust).


find_cycle(Moves, Cycle = [Cycle_Hd|_]) ->
    To = lists:nth(lists:last(Cycle), Moves),
    if To =:= Cycle_Hd -> Cycle;
        true -> find_cycle(Moves, Cycle ++ [To])
    end.
