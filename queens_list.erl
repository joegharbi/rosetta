-module(queens_list).
-export([queens/1]).

queens([Arg]) ->
     List = lists:seq(1,list_to_integer(Arg)),
    %  List = lists:seq(1,Arg),
     % io:fwrite("~p~n",[List]),
     queens(list_to_integer(Arg), List).
    %  queens(Arg, List).
queens(0, _) -> [[]];
queens(N, List) ->
     [[Row | Columns] || Columns <- queens(N-1, List),
          % Row <- [1,2,3,4,5,6,7,8,9,10,11,12,13] -- Columns,
          Row <- List -- Columns,
          safe(Row, Columns, 1)].

safe(_Row, [], _N) -> true;
safe(Row, [Column|Columns], N) ->
     (Row /= Column + N) andalso (Row /= Column - N) andalso
          safe(Row, Columns, (N+1)).