%%%-------------------------------------------------------------------
%%% @author Patryk
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created : 07. sty 2017 12:54
%%%-------------------------------------------------------------------
-module('Warcaby').
-compile({parse_transform, guardian}).
-compile([export_all]).
-author("Patryk").

%funkcje

displayBoard([]) -> io:fwrite("~n");
displayBoard([{e,{_,C}}|T]) when C == 8 -> io:fwrite(".~n"), displayBoard(T);
displayBoard([{bM,{_,C}}|T]) when C == 8 -> io:fwrite("b~n"), displayBoard(T);
displayBoard([{wM,{_,C}}|T]) when C == 8 -> io:fwrite("w~n"), displayBoard(T);
displayBoard([{e,_}|T])  -> io:fwrite("."), displayBoard(T);
displayBoard([{bM,_}|T]) -> io:fwrite("b"), displayBoard(T);
displayBoard([{wM,_}|T]) -> io:fwrite("w"), displayBoard(T).

getPossibleMoves({e,_}) -> [];
getPossibleMoves({bM,{X, Y}}) -> if
                                   X == 8 ->
                                     [];
                                  Y == 1 ->
                                    [{{X + 1, Y + 1},{X, Y}}];
                                  Y == 8 ->
                                    [{{X + 1, Y - 1},{X, Y}}];
                                  true ->
                                    [{{X + 1, Y - 1},{X, Y}},{{X + 1, Y + 1},{X, Y}}]
                                end;
getPossibleMoves({wM,{X, Y}}) -> if
                                   X == 1 ->
                                     [];
                                  Y == 1 ->
                                    [{{X - 1, Y + 1},{X, Y}}];
                                  Y == 8 ->
                                    [{{X - 1, Y - 1},{X, Y}}];
                                  true ->
                                    [{{X - 1, Y - 1},{X, Y}},{{X - 1, Y + 1},{X, Y}}]
                                end.

getWhitePositions([]) -> [];
getWhitePositions([{wM,{X,Y}}|T]) -> [{wM,{X,Y}} | getWhitePositions(T)];
getWhitePositions([_|T]) -> getWhitePositions(T).

getBlackPositions([]) -> [];
getBlackPositions([{bM,{X,Y}}|T]) -> [{bM,{X,Y}} | getBlackPositions(T)];
getBlackPositions([_|T]) -> getBlackPositions(T).

filterPossibleMovesIfAllies([],_) -> [];
filterPossibleMovesIfAllies([H|T],Pos) when checkIfExists(H,Pos) == false  -> [H | filterPossibleMovesIfAllies(T,Pos)];
filterPossibleMovesIfAllies([H|T],Pos) when checkIfExists(H,Pos) == true -> filterPossibleMovesIfAllies(T,Pos).

mappedMoves([]) -> [];
mappedMoves([H|T]) -> [getPossibleMoves(H) | mappedMoves(T)].

filteredMovesAllies([],_) -> [];
filteredMovesAllies([H|T],Pos) -> filterPossibleMovesIfAllies(H,Pos) ++ filteredMovesAllies(T,Pos).

checkIfExists(_,[]) -> false;
checkIfExists(X,[H|_]) when element(2,H) == element(1,X) -> true;
checkIfExists(X,[H|T]) when element(2,H) /= element(1,X) -> checkIfExists(X,T).

checkIfExistsOnJumpPosition(_,[]) -> false;
checkIfExistsOnJumpPosition({X,Y},_) when ((X < 1) or (X > 8) or (Y < 1) or (Y > 8)) -> true;
checkIfExistsOnJumpPosition(X,[H|_]) when element(2,H) == X -> true;
checkIfExistsOnJumpPosition(X,[H|T]) when element(2,H) /= X -> checkIfExistsOnJumpPosition(X,T).

checkIfCanJump({{X1,Y1},{X2,Y2}},AP,EP) -> case {X2-X1,Y2-Y1} of
                                             {1,1} -> X = checkIfExistsOnJumpPosition({X2-2,Y2-2},AP++EP),
                                                        if
                                                          (X == true) ->
                                                            [];
                                                          true ->
                                                            [{{X2-2,Y2-2},{X2,Y2}}]
                                                        end;
                                             {1,-1} -> X = checkIfExistsOnJumpPosition({X2-2,Y2+2},AP++EP),
                                                       if
                                                         (X == true) ->
                                                           [];
                                                         true ->
                                                           [{{X2-2,Y2+2},{X2,Y2}}]
                                                       end;
                                             {-1,1} -> X = checkIfExistsOnJumpPosition({X2+2,Y2-2},AP++EP),
                                                       if
                                                         (X == true) ->
                                                           [];
                                                         true ->
                                                           [{{X2+2,Y2-2},{X2,Y2}}]
                                                       end;
                                             {-1,-1} ->  X = checkIfExistsOnJumpPosition({X2+2,Y2+2},AP++EP),
                                                       if
                                                         (X == true) ->
                                                           [];
                                                         true ->
                                                           [{{X2+2,Y2+2},{X2,Y2}}]
                                                       end
                                           end.
filterForJumpMoves([],_,_) -> [];
filterForJumpMoves([H|T],AP,EP) when checkIfExists(H,EP) == true -> checkIfCanJump(H,AP,EP) ++ filterForJumpMoves(T,AP,EP);
filterForJumpMoves([H|T],AP,EP) when checkIfExists(H,EP) == false -> [H] ++ filterForJumpMoves(T,AP,EP).

checkIfPossibleJumpMoves([]) -> false;
checkIfPossibleJumpMoves([{{X1,_},{X2,_}}|_]) when abs(X2 - X1) == 2 -> true;
checkIfPossibleJumpMoves([{{X1,_},{X2,_}}|T]) when abs(X2 - X1) /= 2 -> checkIfPossibleJumpMoves(T).

getOnlyJumpMoves([]) -> [];
getOnlyJumpMoves([{{X1,Y1},{X2,Y2}}|T]) when abs(X2 - X1) == 2 -> [{{X1,Y1},{X2,Y2}}] ++ getOnlyJumpMoves(T);
getOnlyJumpMoves([{{X1,_},{X2,_}}|T]) when abs(X2 - X1) /= 2 -> getOnlyJumpMoves(T).

extract_integer([]) -> [];
extract_integer([H|T]) when ((H >= $1) and (H =< $9)) -> [H] ++ extract_integer(T);
extract_integer([_|T]) -> extract_integer(T).

checkIfCorrectMove(Move,H) when (((element(1,string:to_integer(Move))) >= 0) and ((element(1,string:to_integer(Move))) =< length(H))) ->
  lists:nth((element(1,string:to_integer(Move))),H);
checkIfCorrectMove(_,H) -> getMove(H).

getMove(H) -> Move = io:get_line("Choose move ! : "),
  checkIfCorrectMove(Move,H).

updateBoard(_,_,_,[]) -> [];
updateBoard({AX,{X,Y}},P1,P2,[H|T]) when ({X,Y} == element(2,H)) -> [{AX,{X,Y}}] ++ updateBoard({AX,{X,Y}},P1,P2,T);
updateBoard(P1,{AX,{X,Y}},P2,[H|T]) when ({X,Y} == element(2,H)) -> [{AX,{X,Y}}] ++ updateBoard(P1,{AX,{X,Y}},P2,T);
updateBoard(P1,P2,{AX,{X,Y}},[H|T]) when ({X,Y} == element(2,H)) -> [{AX,{X,Y}}] ++ updateBoard(P1,P2,{AX,{X,Y}},T);
updateBoard(P1,P2,P3,[H|T]) -> [H] ++ updateBoard(P1,P2,P3,T).

updateBoard(_,_,[]) -> [];
updateBoard({AX,{X,Y}},P1,[H|T]) when ({X,Y} == element(2,H)) -> [{AX,{X,Y}}] ++ updateBoard({AX,{X,Y}},P1,T);
updateBoard(P1,{AX,{X,Y}},[H|T]) when ({X,Y} == element(2,H)) -> [{AX,{X,Y}}] ++ updateBoard(P1,{AX,{X,Y}},T);
updateBoard(P1,P2,[H|T]) -> [H] ++ updateBoard(P1,P2,T).

makeMove({{X1,Y1},{X2,Y2}},AX,B) when abs(X2 - X1) == 2 -> updateBoard({AX,{X1,Y1}},{e,{((X2+X1)/2),((Y2+Y1)/2)}},{e,{X2,Y2}},B);
makeMove({{X1,Y1},{X2,Y2}},AX,B) -> updateBoard({AX,{X1,Y1}},{e,{X2,Y2}},B).

getAllPosibleMovesForWhite(Board) ->
  BP = getBlackPositions(Board),
  WP = getWhitePositions(Board),
  MMov = mappedMoves(WP),
  FMAl = filteredMovesAllies(MMov,WP),
  FMAE = filterForJumpMoves(FMAl,WP,BP),
  case checkIfPossibleJumpMoves(FMAE) of
    true ->
      getOnlyJumpMoves(FMAE);
    false ->
      FMAE
  end.

getAllPosibleMovesForBlack(Board) ->
  BP = getBlackPositions(Board),
  WP = getWhitePositions(Board),
  MMov = mappedMoves(BP),
  FMAl = filteredMovesAllies(MMov,BP),
  FMAE = filterForJumpMoves(FMAl,BP,WP),
  case checkIfPossibleJumpMoves(FMAE) of
    true ->
      getOnlyJumpMoves(FMAE);
    false ->
      FMAE
  end.

spawnProcessForMove(_,_,_,[],_) -> ok;
spawnProcessForMove(B,H,PID,[G|T],CP) -> spawn('Warcaby',getComputerMove,[makeMove(G,CP,B),H+1,PID,G]),
  spawnProcessForMove(B,H,PID,T,CP).

getBestMoveFromProcesses(_,Value,0,ok) -> Value;
getBestMoveFromProcesses(H,Value,P,ok) ->
  case H rem 2 of
    0 ->
      receive
        {V,Move} when V >= element(1,Value) ->
          getBestMoveFromProcesses(H,{V,Move},P-1,ok);
        {_,_}  ->
          getBestMoveFromProcesses(H,Value,P-1,ok)
      end;
    1 ->
      receive
        {V,Move} when V =< element(1,Value) ->
          getBestMoveFromProcesses(H,{V,Move},P-1,ok);
        {_,_}  ->
          getBestMoveFromProcesses(H,Value,P-1,ok)
      end
  end.

getBestMoveFromProcesses(_,Value,0) -> Value;
getBestMoveFromProcesses(H,Value,P) ->
  case H rem 2 of
    0 ->
      receive
        {V,_} when V >= Value ->
          getBestMoveFromProcesses(H,V,P-1);
        {_,_}  ->
          getBestMoveFromProcesses(H,Value,P-1)
      end;
    1 ->
      receive
        {V,_} when V =< Value ->
          getBestMoveFromProcesses(H,V,P-1);
        {_,_}  ->
          getBestMoveFromProcesses(H,Value,P-1)
      end
  end.

heuristic(BP,WP) -> length(BP) - length(WP).

getComputerMove(B,3,PID,Move) -> PID ! {heuristic(getBlackPositions(B),getWhitePositions(B)),Move};
getComputerMove(B,H,PID,Move) ->
  if
    H rem 2 == 0 ->
      CPlayer = bM,
      M = getAllPosibleMovesForBlack(B);
    true ->
      CPlayer = wM,
      M = getAllPosibleMovesForWhite(B)
  end,
  BP = getBlackPositions(B),
  WP = getWhitePositions(B),
  HeuV = heuristic(BP,WP),
  if
    H == 0 ->
      spawnProcessForMove(B,H,self(),M,CPlayer),
      ChosenMove = getBestMoveFromProcesses(H,{HeuV,{{0,0},{0,0}}},length(M),ok),
      element(2,ChosenMove);
    true ->
      spawnProcessForMove(B,H,self(),M,CPlayer),
      ChosenValue = getBestMoveFromProcesses(H,HeuV,length(M)),
      PID ! {ChosenValue,Move}
  end.


play(_,over) -> ok;
play(Board,play) ->
  FMAJ = getAllPosibleMovesForWhite(Board),
  io:fwrite(io_lib:format("~p~n",[FMAJ])),
  L = getMove(FMAJ),
  UpdatedBoardAfterMove = makeMove(L,wM,Board),
  displayBoard(UpdatedBoardAfterMove),
  M = getAllPosibleMovesForBlack(UpdatedBoardAfterMove),
  BP = getBlackPositions(UpdatedBoardAfterMove),
  if
    ((length(BP) == 0)) ->
      io:fwrite("Wygrales ! :)~n"),
      play(UpdatedBoardAfterMove,over);
    (length(M) == 0) ->
      io:fwrite("Remis ! :)~n"),
      play(UpdatedBoardAfterMove,over);
    true ->
      ok
  end,
  CompMove = getComputerMove(UpdatedBoardAfterMove,0,2,5),
  UpdatedBoardAfterMove1 = makeMove(CompMove,bM,UpdatedBoardAfterMove),
  displayBoard(UpdatedBoardAfterMove1),
  WP = getWhitePositions(UpdatedBoardAfterMove1),
  MW = getAllPosibleMovesForWhite(UpdatedBoardAfterMove1),
  if
    ((length(WP) == 0)) ->
      io:fwrite("Przegrales ! :(~n"),
      play(UpdatedBoardAfterMove1,over);
    (length(MW) == 0) ->
      io:fwrite("Remis ! :)~n"),
      play(UpdatedBoardAfterMove1,over);
    true ->
      play(UpdatedBoardAfterMove1,play)
  end.
start() ->
  Board = [{e,{1,1}},{bM,{1,2}},{e,{1,3}},{bM,{1,4}},{e,{1,5}},{bM,{1,6}},{e,{1,7}},{bM,{1,8}},
    {bM,{2,1}},{e,{2,2}},{bM,{2,3}},{e,{2,4}},{bM,{2,5}},{e,{2,6}},{bM,{2,7}},{e,{2,8}},
    {e,{3,1}},{bM,{3,2}},{e,{3,3}},{bM,{3,4}},{e,{3,5}},{bM,{3,6}},{e,{3,7}},{bM,{3,8}},
    {e,{4,1}},{e,{4,2}},{e,{4,3}},{e,{4,4}},{e,{4,5}},{e,{4,6}},{e,{4,7}},{e,{4,8}},
    {e,{5,1}},{e,{5,2}},{e,{5,3}},{e,{5,4}},{e,{5,5}},{e,{5,6}},{e,{5,7}},{e,{5,8}},
    {wM,{6,1}},{e,{6,2}},{wM,{6,3}},{e,{6,4}},{wM,{6,5}},{e,{6,6}},{wM,{6,7}},{e,{6,8}},
    {e,{7,1}},{wM,{7,2}},{e,{7,3}},{wM,{7,4}},{e,{7,5}},{wM,{7,6}},{e,{7,7}},{wM,{7,8}},
    {wM,{8,1}},{e,{8,2}},{wM,{8,3}},{e,{8,4}},{wM,{8,5}},{e,{8,6}},{wM,{8,7}},{e,{8,8}}],
  displayBoard(Board),
  play(Board,play).