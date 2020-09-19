alphaBeta(Pos,Alpha,Beta,GoodPos,Val):-
  moves(Pos,PosList),!
  boundedBest(Poslist,Alpha,Beta,GoodPos,Val);
  staticVal(Pos,Val). % Static value of Pos

boundedBest([Pos|PosList],Alpha,Beta,GoodPos,GoodVal):-
  alphaBeta(Pos,Alpha,Beta,_,Val),
  goodEnough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal).

goodEnough([]],_,_,Pos,Val,Pos,Val):-!. % no other candidate

goodEnough(_,Alpha,Beta,Pos,Val,Pos,Val):-
   minToMove(Pos), Val>Beta,! % maximizer attained upper bound
   ;
   maxToMove(Pos), Val<Alpha,!. % minimizer attained lower bound

goodEnough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal):-
  newBounds(Alpha,Beta,Pos,Val,NewAlpha,NewBeta), % refiene bounds
  boundedBest(PosList,NewAlpha,NewBeta,Pos1,Val1),
  betterOf(Pos,Val,Pos1,Val1,GoodPos,GoodVal).

newBounds(Alpha,Beta,Pos,Val,Val,Beta):-
  minToMove(Pos), Val>Alpha,!. % maximizer increased lower bound

newBounds(Alpha,Beta,Pos,Val,Alpha,Val):-
  maxToMove(Pos), Val<Beta,!. % minumizer decreased upper bound

newBounds(Alpha,Beta,_,_,Alpha,Beta). % otherwise bounds unchanged

betterOf(Pos,Val,Pos1,Val1,Pos,Val):- % Pos better than Pos1
  minToMove(Pos), Val>Val1,!
  ;
  maxToMove(Pos), Val<Val1,!.

betterOf(_,_,Pos1,Val1,Pos1,Val1).  %otherwise Pos1 better
