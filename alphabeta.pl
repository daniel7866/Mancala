/*
%get representation of the board
boardState(HumanStones0,HumanStones1,HumanStones2,HumanStones3,HumanStones4,HumanStones5,HumanBank,CpuStones0,CpuStones1,CpuStones2,CpuStones3,CpuStones4,CpuStones5,CpuBank):-
  pocket(0, human,HumanStones0),
  pocket(1, human,HumanStones1),
  pocket(2, human,HumanStones2),
  pocket(3, human,HumanStones3),
  pocket(4, human,HumanStones4),
  pocket(5, human,HumanStones5),
  pocket(bank, human,HumanBank),
  pocket(0, cpu,CpuStones0),
  pocket(1, cpu,CpuStones1),
  pocket(2, cpu,CpuStones2),
  pocket(3, cpu,CpuStones3),
  pocket(4, cpu,CpuStones4),
  pocket(4, cpu,CpuStones5),
  pocket(bank, cpu,CpuBank).


  %set the board to boardState in parameter
  setBoard(boardState(A,B,C,D,E,F,G,H,I,J,K,L,M,N)):-
    retractall(pocket(_,_,_)),
    assert(pocket(0,human,A)),
    assert(pocket(1,human,B)),
    assert(pocket(2,human,C)),
    assert(pocket(3,human,D)),
    assert(pocket(4,human,E)),
    assert(pocket(5,human,F)),
    assert(pocket(bank,human,G)),
    assert(pocket(0,cpu,H)),
    assert(pocket(1,cpu,I)),
    assert(pocket(2,cpu,J)),
    assert(pocket(3,cpu,K)),
    assert(pocket(4,cpu,L)),
    assert(pocket(5,cpu,M)),
    assert(pocket(bank,cpu,N)).
*/






% ---------------------------------------------------------------------
%              20596 - Prolog & Artificial Intelligence
%                      Maman 17 - Final Project
% ---------------------------------------------------------------------
% Programmers:
%   Name: Daniel Fogel
%   ID: 208778654
%   Name: Hila Deri
%   ID: 301785861
% ---------------------------------------------------------------------
% File Name: Mancala.pl
% ---------------------------------------------------------------------
% Description:
% Mancala 2X8 board game application against the computer, implemented
% with AlphaBeta search algorithm.
% ---------------------------------------------------------------------
% Synopsis:
%
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% internally used dynamic predicates & data structures:
% ---------------------------------------------------------------------
:- dynamic pocket/3. % pocket(pos, player, stones count) - represents a
% specific pocekt in the board game.
:- dynamic turn/1. % turn(player) - represents who can play now.
:- dynamic winner/1. % winner (player) - represents the game's winner.

% ---------------------------------------------------------------------
% memory cleaning of dynamic predicates:
% ---------------------------------------------------------------------
cleanUp:-
  retractall(pocket(_,_,_)), % cleans pocket(pos, player, stones count)
  retractall(turn(_)), % cleans turn(player)
  retractall(winner(_)). % cleans winner(player)

% ---------------------------------------------------------------------
% initializing the board game:
% ---------------------------------------------------------------------
% starts with initializing 5 pockets for each player
% then initializing the banks for both players.
start:-
  start(5),
  assert(pocket(bank,human,0)),
  assert(pocket(bank,cpu,0)),
  random_member(Player,[cpu,human]), % use SWI built-in predicate random_member
  assert(turn(Player)).  % to choose the player that play first

% initializing the 5 pockets of each player in the board:
start(-1). % stop after 5 pockets for each player
start(Pos):-
  assert(pocket(Pos,human,4)), % game starts with 4 stones
  assert(pocket(Pos,cpu,4)), % in each pocket for each player
  NextPos is Pos-1,
  start(NextPos). % recursivly updating the next pocket

% ---------------------------------------------------------------------
% checking if a specific pocket is empty:
% ---------------------------------------------------------------------
isEmptyPocket(Pos,BoardSide):-
  pocket(Pos,BoardSide,0).

% ---------------------------------------------------------------------
% checking if a specific row is empty(the entire row):
% ---------------------------------------------------------------------
% BoardSide is the player side of the board :
% we check if this entire row is empty
isEmptyRow(BoardSide):-
  isEmptyRow(5,BoardSide). % there are 6 pockets( 0 - 5 )

% overloading:
isEmptyRow(-1,_). % break the recursion - we are done with the entire row

isEmptyRow(Pos,BoardSide):-
  pocket(Pos,BoardSide,0), % pocket(Pos,BoardSide, 0 stones = empty)
  NextPos is Pos-1,
  isEmptyRow(NextPos,BoardSide). % recursion

% ---------------------------------------------------------------------
% put chosen number of stones in a specific pocket:
% check how many stones in the pockets
% retract the pocket and assert it again with new number of stones
% ---------------------------------------------------------------------
putInPocket(Pos,BoardSide,NumOfStonesToPut):-
  pocket(Pos,BoardSide,NumOfStones),
  UpNumOfStones is NumOfStones+NumOfStonesToPut,
  retract(pocket(Pos,BoardSide,NumOfStones)),
  assert(pocket(Pos,BoardSide,UpNumOfStones)).

% ---------------------------------------------------------------------
% empty this pocket - take out all of it's stones:
% check how many stones in the pockets
% retract the pocket and assert it again with new number of stones (0)
% ---------------------------------------------------------------------
emptyCurrPocket(Pos,BoardSide,NumOfStones):-
  pocket(Pos,BoardSide,NumOfStones),
  retract(pocket(Pos,BoardSide,NumOfStones)),
  assert(pocket(Pos,BoardSide,0)).

% ---------------------------------------------------------------------
% (dir recalls direction):
% dir gets current pocket and brings the next pocket.
% dir is written twice and seperated between the human side of the board
% and the cpu side of the board
% the direction is counter clockwise
% the cpu is in the upper row and the human in the bottom row
% cpu goes: 5->4->3->2->1->0
% human goes: 0->1->2->3->4->5
% ---------------------------------------------------------------------
/*************** human side of the board***************/
dir(CurrPos,human,NextPos,BoardSide):-
  integer(CurrPos), % position can be a bank - not an integer
  CurrPos<5,
  BoardSide = human, % if we are less than 5 we stay on current BoardSide
  NextPos is CurrPos+1.

dir(5,human,bank,human):- % player is human-we go to the human's bank
  turn(human).

dir(bank,human,5,cpu):- % player is human and we are in his bank -
  turn(human). % we go counter clockwise to the cpu's row - start at pocket 5

dir(5,human,5,cpu):- % player is cpu - we skip the human's bank
  turn(cpu).

/*************** cpu side of the board ***************/
dir(CurrPos,cpu,NextPos,BoardSide):-
  integer(CurrPos), % position can be a bank - not an integer
  CurrPos>0,
  BoardSide = cpu, % if we are more than 0 we stay on current BoardSide
  NextPos is CurrPos-1.

dir(0,cpu,bank,cpu):- % player is cpu-we go to the cpu's bank
  turn(cpu).

dir(bank,cpu,0,human):- % player is cpu and we are in his bank -
  turn(cpu). % we go counter clockwise to the humans's row - start at pocket 0

dir(0,cpu,0,human):- % player is human - we skip the cpu's bank
  turn(human).

% ---------------------------------------------------------------------
% checking pocket's validness:
% ---------------------------------------------------------------------
% check that the current player chooses a pocket on his side of the board
%  that is not empty, & pocket is not a bank
validPocket(Pos,BoardSide):-
  turn(BoardSide),
  between1(0,Pos,5),
  not(isEmptyPocket(Pos,BoardSide)).

% help predicate for the validPocket predicate
between1(Low,X,High):-
  integer(X),
  X=<High,
  X>=Low.

% ---------------------------------------------------------------------
% move from current pocket:
% we collect all of the stones from current pocket
% then we put one stone in each of the next pockets in the direction
% untill we run out of stones and we stop.
% if we stop at a bank - we get another turn
% if we stop at other pockets we switch turns
% CAPTURED - if we stop at an empty pocket in our side of the board
% we check if the parallel pocket in the other side is empty
% if it's not - we take our stone and all of the parallel pocket's stones
% and put them in our bank
% ---------------------------------------------------------------------
% The player will choose the pocket he wants to empty and put one stone in
% each of the next pockets
move(Pos,BoardSide):-
  validPocket(Pos,BoardSide),
  emptyCurrPocket(Pos,BoardSide,NumOfStones),
  dir(Pos,BoardSide,NextPos,NextBoardSide), % get the next pocket
  move(NextPos,NextBoardSide,NumOfStones). % put one stone in each of the next pockets

move(_,_,0):- % when we are out of stones - we stop
  switchTurns.

move(bank,BoardSide,1):- % when the last stone is in the bank - we get another turn
  putInPocket(bank,BoardSide,1),
  switchTurns. %*************************************NEEDS TO BE REMOVED!!!!! ***********************

move(Pos,BoardSide,1):-
  (captured(Pos,BoardSide); % CAPTURED as explained before
  putInPocket(Pos,BoardSide,1)), % if not "captured" just put the stone in the pocket
  switchTurns.

% overloading for move(Pos,BoardSide)
% after we finished emptying the chosen pockets and took out his stones
% we each one of them in the next pockets in the direction(counter clockwise)
move(Pos,BoardSide,NumOfStones):-
  putInPocket(Pos,BoardSide,1), % put one in current pocket
  CurrNumOfStones is NumOfStones-1,
  dir(Pos,BoardSide,NextPos,NextBoardSide),
  move(NextPos,NextBoardSide,CurrNumOfStones). % put a stone in the next cell

/*************** switch turns - human to cpu ***************/
switchTurns:-
  turn(human),
  retract(turn(human)),
  assert(turn(cpu)).

/*************** switch turns - cpu to human ***************/
switchTurns:-
  turn(cpu),
  retract(turn(cpu)),
  assert(turn(human)).

/*************** captured  ***************/
%  if we stop at an empty pocket in our side of the board
% we check if the parallel pocket in the other side is empty
% if it's not - we take our stone and all of the parallel pocket's stones
% and put them in our bank
% captured is written twice and seperated between the human side of the board
% and the cpu side of the board
/*************** human's turn ***************/
captured(Pos,human):-
  isEmptyPocket(Pos,human), % check that we fell on an empty pocket
  turn(human),
  not(isEmptyPocket(Pos,cpu)), % the parallel pocket is not empty
  emptyCurrPocket(Pos,cpu,CpuNumOfStones), % take out the stones from parallel
  pocket(bank,human,BankNumOfStones),
  retract(pocket(bank,human,BankNumOfStones)),
  UpBankNumOfStones is BankNumOfStones + CpuNumOfStones + 1,
  assert(pocket(bank,human,UpBankNumOfStones)). % put it in human's bank

/*************** cpu's turn ***************/
captured(Pos,cpu):-
  isEmptyPocket(Pos,cpu), % check that we fell on an empty pocket
  turn(cpu),
  not(isEmptyPocket(Pos,human)), % the parallel pocket is not empty
  emptyCurrPocket(Pos,human,HumanNumOfStones), % take out the stones from parallel
  pocket(bank,cpu,BankNumOfStones),
  retract(pocket(bank,cpu,BankNumOfStones)),
  UpBankNumOfStones is BankNumOfStones + HumanNumOfStones + 1,
  assert(pocket(bank,cpu,UpBankNumOfStones)). % put it in cpu's bank

% ---------------------------------------------------------------------
% collect all stones from all pockets in a specific row
% ---------------------------------------------------------------------
collectRow(BoardSide):-
  collectRow(5,BoardSide). % overloading - 5 pockets in a row
collectRow(-1,_). % finished the entire row
collectRow(Pos,BoardSide):-
  emptyCurrPocket(Pos,BoardSide,NumOfStones),
  pocket(bank,BoardSide,BankNumOfStones),
  UpBankNumOfStones is NumOfStones + BankNumOfStones,
  retract(pocket(bank,BoardSide,BankNumOfStones)),
  assert(pocket(bank,BoardSide,UpBankNumOfStones)),
  NewPos is Pos-1,
  collectRow(NewPos,BoardSide). % recursivly call the next pocket

% ---------------------------------------------------------------------
% Check that the game was ended:
% the game ends when one row is empty
% then we take all the stones in the remaining row to the bank
% of the remaining row's player
% after that check who has more stones in the bank - it's the winner
% ---------------------------------------------------------------------
gameEnded:-
  gameEnded1, % one row is empty and other row collected
  pocket(bank,human,HumanNumOfStones),
  pocket(bank,cpu,CpuNumOfStones),
  ((HumanNumOfStones>CpuNumOfStones,assert(winner(human)));
  (HumanNumOfStones<CpuNumOfStones,assert(winner(cpu)));
  (HumanNumOfStones = CpuNumOfStones,assert(winner(tie)))).%no one won - it's a tie

% check if a row is empty - collect the other
gameEnded1:-
  isEmptyRow(human), % human's row is empty
  collectRow(cpu). % collect all stones from cpu's row

% check if a row is empty - collect the other
gameEnded1:-
  isEmptyRow(cpu), % cpu's row is empty
  collectRow(human). % collect all stones from human's row







/************************************************************************
*






*/
insert(X,List,[X|List]).

% a boardstate contains all pockets with their number of stones on the board
boardState(FinalList):-
  boardState([],List,5), % using overloading with accumulator
  getNumOfStones(bank,human,HumanBank),
  getNumOfStones(bank,cpu,CpuBank),
  insert(bank-human-HumanBank,List,List1), % add the banks seperatly
  insert(bank-cpu-CpuBank,List1,FinalList).

% overloading
boardState(StateList,StateList,-1).
boardState(Acc,StateList,Pos):-
  NextPos is Pos-1,
  getNumOfStones(Pos,human,HumanNumOfStones),
  getNumOfStones(Pos,cpu,CpuNumOfStones),
  insert(Pos-human-HumanNumOfStones,Acc,Acc1),
  insert(Pos-cpu-CpuNumOfStones,Acc1,Acc2),
  boardState(Acc2,StateList,NextPos).

getNumOfStones(Pos,BoardSide,NumOfStones):-
  pocket(Pos,BoardSide,NumOfStones).

getPocket(Pos,BoardSide,NumOfStones):-
  pocket(Pos,BoardSide,NumOfStones).

% seWtBoard - gets a Boardstate which is a list of all pockets on the board
% deletes all pockets on current board game and assert the pockets from recivied list
setBoard([]).
setBoard([Pos-BoardSide-NumOfStones|Tail]):-
  retractall(pocket(_,_,_)), % delete the current board
  write(Pos-BoardSide-NumOfStones),nl,
  assert(pocket(Pos,BoardSide,NumOfStones)), % Pocket = pocket(Pos, Boardside, Stones)
  setBoard(Tail).


% moves gets a state and creates a list of all possible states from the given state (for alpha beta)
% it sets the board to the given state, then tries to play from all valid pockets (of current player)
% and all valid moves are appended to the list.
moves(State,FinalList):-
  moves(State,[],FinalList,5). % calls recursivly and try all pockets from 5 to 0.

moves(State,FinalList,FinalList,-1). % we're done searching all the pockets (possible states)

%overloading
moves(State,Tail,FinalList,Pos):-
  setBoard(State),
  turn(BoardSide), % who is playing
  move(Pos,BoardSide), % if valid move
  boardState(NewState), % save the new board state
  NextPos is Pos-1, % go to the next pocket
  moves(State,[NewState|Tail],FinalList,NextPos). % append current state to the list

moves(State,Tail,FinalList,Pos):- %if Pos' pocket is not a valid move - go to the next pocket
  NewPos is Pos-1,
  moves(State,Tail,FinalList,NewPos).

staticVal(State,Val):-
  setBoard(State),
  pocket(bank,cpu,CpuBank),
  pocket(bank,human,HumanBank),
  Val is CpuBank-HumanBank.

minToMove(_):-
  turn(human).

maxToMove(_):-
  turn(cpu).


alphaBeta:-
  turn(cpu),boardState(OriginalState),
  alphaBeta(OriginalState,-9999,9999,GoodState,Val),
  retract(turn(_)),
  assert(turn(cpu)),
  setBoard(OriginalState),
  write(GoodState).
%alpha beta:
alphaBeta(State,Alpha,Beta,GoodState,Val):-
  not(gameEnded),moves(State,StateList),!,
  boundedBest(Statelist,Alpha,Beta,GoodState,Val);
  staticVal(State,Val). % Static value of Pos

boundedBest([Pos|PosList],Alpha,Beta,GoodPos,GoodVal):-
  alphaBeta(Pos,Alpha,Beta,_,Val),
  goodEnough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal).

goodEnough([],_,_,Pos,Val,Pos,Val):-!. % no other candidate

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
