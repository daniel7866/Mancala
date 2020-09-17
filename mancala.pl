%pocket(pos, player, stones count)
%turn(player) = who can play now
:- dynamic pocket/3.
:- dynamic turn/1.


cleanUp:-
  retractall(pocket(_,_,_)),
  retractall(turn(_)).
start:-
  start(5),
  assert(pocket(bank,human,0)),
  assert(pocket(bank,cpu,0)),
  assert(turn(human)).%human starts first

start(-1).
start(Pos):-
  assert(pocket(Pos,human,4)),%game starts with 4 stones in each pocket
  assert(pocket(Pos,cpu,4)),
  NextPos is Pos-1,
  start(NextPos).

%Game has ended if one side has no stones left
ended:-
  isEmptyRow(5,human);
  isEmptyRow(5,cpu).

isEmptyRow(-1).
isEmptyRow(Pos,BoardSide):-
  pocket(Pos,BoardSide,0),
  NextPos is Pos-1,
  isEmptyRow(NextPos,BoardSide).

%put a stone in the pocket
putInPocket(Pos,BoardSide,NumOfStonesToPut):-
  pocket(Pos,BoardSide,NumOfStones),
  UpNumOfStones is NumOfStones+NumOfStonesToPut,
  retract(pocket(Pos,BoardSide,NumOfStones)),
  assert(pocket(Pos,BoardSide,UpNumOfStones)).

%empty this pocket - take out all of it's rocks
emptyCurrPocket(Pos,BoardSide,NumOfStones):-
  pocket(Pos,BoardSide,NumOfStones),
  retract(pocket(Pos,BoardSide,NumOfStones)),
  assert(pocket(Pos,BoardSide,0)).

%dir gets current position and brings the next position
dir(CurrPos,human,NextPos,BoardSide):-
  integer(CurrPos),
  CurrPos<5,
  BoardSide = human,
  NextPos is CurrPos+1.
dir(5,human,bank,human):-% player is human
  turn(human).
dir(bank,human,5,cpu):-% player is human
  turn(human).
dir(5,human,5,cpu):-% player is cpu
  turn(cpu).

%dir gets current position and brings the next position
dir(CurrPos,cpu,NextPos,BoardSide):-
  integer(CurrPos),
  CurrPos>0,
  BoardSide = cpu,
  NextPos is CurrPos-1.
dir(0,cpu,bank,cpu):-% player is human
  turn(cpu).
dir(bank,cpu,0,human):-% player is cpu
  turn(cpu).
dir(0,cpu,0,human):-% player is cpu
  turn(human).

%Check if current pocket is empty
isEmptyPocket(Pos,BoardSide):-
  pocket(Pos,BoardSide,0).

between1(Low,X,High):-
  integer(X),
  X=<High,
  X>=Low.

% check that the current player chooses a pocket on his side of the board that is not empty
validPocket(Pos,BoardSide):-
  turn(BoardSide),
  between1(0,Pos,5),
  not(isEmptyPocket(Pos,BoardSide)).

%The player will choose the pocket he wants to empty and put one stone in each of the next pockets
move(Pos,BoardSide):-
  validPocket(Pos,BoardSide),
  emptyCurrPocket(Pos,BoardSide,NumOfStones),
  dir(Pos,BoardSide,NextPos,NextBoardSide),%get the next pocket
  move(NextPos,NextBoardSide,NumOfStones).%put one stone in each of the next pockets

move(Pos,_,0):-%when we are out of stones - we stop
  Pos = bank;
  switchTurns.
move(Pos,BoardSide,NumOfStones):-
  putInPocket(Pos,BoardSide,1),%put one in current pocket
  CurrNumOfStones is NumOfStones-1,
  dir(Pos,BoardSide,NextPos,NextBoardSide),
  move(NextPos,NextBoardSide,CurrNumOfStones).%put a stone in the next cell

switchTurns:-
  turn(human),
  retract(turn(human)),
  assert(turn(cpu)).
switchTurns:-
  turn(cpu),
  retract(turn(cpu)),
  assert(turn(human)).
