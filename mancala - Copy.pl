% ---------------------------------------------------------------------
%              20596 - Prolog & Artificial Intelligence
%                      Maman 17 - Final Project
% ---------------------------------------------------------------------
% Programmer:
%   Name: Daniel Fogel
%   ID: 208778654
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
  putInPocket(bank,BoardSide,1).
  %switchTurns. %*************************************NEEDS TO BE REMOVED!!!!! ***********************

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


insert(X,List,[X|List]). % just a simple insertion to a list
% ---------------------------------------------------------------------
% get current board state in a list:
% Pos taht led To this state-BoardSidePocket that Led To this State-[pos-boardside-stones|...]-whose turn it is now in this state
% Pos and BoardSide both= _ because they are unkown
% ---------------------------------------------------------------------

getCurrentState(_-_-List-Player):-
  getCurrentState([],List1,5), % overloading
  pocket(bank,cpu,CpuBank),
  pocket(bank,human,HumanBank),
  insert(bank-cpu-CpuBank,List1,List2), % insert the banks as well
  insert(bank-human-HumanBank,List2,List),
  turn(Player). % who plays now in this state

getCurrentState(Acc,Acc,-1). % after done with all pockets from 5 to 0
getCurrentState(Acc,List,Pos):- % using accumulator
  pocket(Pos,cpu,CpuNumOfStones),
  pocket(Pos,human,HumanNumOfStones),
  insert(Pos-cpu-CpuNumOfStones,Acc,Acc1),
  insert(Pos-human-HumanNumOfStones,Acc1,Acc2),
  NextPos is Pos-1,
  getCurrentState(Acc2,List,NextPos).

% ---------------------------------------------------------------------
% set current board from a state given in a list:
% pos led to state - BoardSidePocket led to state - [pos-boardside-stones|...] - current player on this state
% pos and BoardSidePocket are _ becase we don't care about them
% ---------------------------------------------------------------------
setBoard(_-_-[]-CurrPlayer):-
  retractall(turn(_)),
  assert(turn(CurrPlayer)).
setBoard(_-_-[Pos-Player-NumOfStones|Tail]-CurrPlayer):-
  retractall(pocket(Pos,Player,_)),
  assert(pocket(Pos,Player,NumOfStones)),
  setBoard(_-_-Tail-CurrPlayer).
% ---------------------------------------------------------------------
% get all possible moves from a specific board state
% the moves will be in a list like this:
% [PosPlayed-BoardSide-The_state_after_the_change|...]
% the state of the given change is a list [state]-Player => Player = who plays in this state
% ---------------------------------------------------------------------
moves(State,PossibleMoves):-
  turn(Player),
  getCurrentState(OriginalState), % get the state before any changes are made
  moves(State,[],PossibleMoves,5), % overloading (use accumulator)
  setBoard(OriginalState), % reset the board as it was before
  retractall(turn(_)),
  assert(turn(Player)).

% after done checking all pockets from 5 to 0
moves(_,PossibleMoves,PossibleMoves,-1).
moves(State,Acc,PossibleMoves,Pos):-
  setBoard(State),
  move(Pos,BoardSide), % change the board and get the current state
  getCurrentState(_-_-AfterMove-Player),
  setBoard(State), % reset the board before the change
  insert(Pos-BoardSide-AfterMove-Player,Acc,Acc1), % write the board and the pos that led to it
  NextPos is Pos-1,
  moves(State,Acc1,PossibleMoves,NextPos).
moves(State,Acc,PossibleMoves,Pos):- % if pocket is not valid - skip it
  NextPos is Pos-1,
  moves(State,Acc,PossibleMoves,NextPos).

% ---------------------------------------------------------------------
%-------------------------------AlphaBeta------------------------------
% ---------------------------------------------------------------------

% minToMove(State) -> State = [PosPlayed-BoardSide-State]
% A given state saves the pocket and boardSide that led to that state
% This is the OPPOSITE from the meaning in the book in page 285
minToMove(_-human-_-_). % if human played last - he is a minimum player
maxToMove(_-cpu-_-_). % if cpu played last - he is a maximum player

% staticVal gets the huristic value
% the huristic value is how much stones does cpu has in the bank more than human:
% cpu wants to have more stones than human - max player.
% human wants to have more stones than cpu - smaller value = min player.
staticVal(State,Val):-
  getCurrentState(OriginalState),
  setBoard(State),
  pocket(bank,human,HumanBank),
  pocket(bank,cpu,CpuBank),
  Val is CpuBank-HumanBank,
  setBoard(OriginalState).

runAlphaBeta(Depth,GoodState,GoodVal):-
  getCurrentState(_-_-State-Player),alphaBeta(Depth,_-_-State-Player,-9999,9999,GoodState,GoodVal).
alphaBeta(Depth,_-_-State-Player,Alpha,Beta,GoodState,Val):-
  Depth>0,
  moves(_-_-State-Player,StateList),StateList\=[]!, % if game ended the stateList is empty list
  Depth1 is Depth-1,
  boundedBest(Depth1,StateList,Alpha,Beta,GoodState,Val);
  staticVal(_-_-State-Player,Val).

boundedBest(Depth,[State|StateList],Alpha,Beta,GoodState,GoodVal):-
  alphaBeta(Depth,State,Alpha,Beta,_,Val),
  goodEnough(Depth,StateList,Alpha,Beta,State,Val,GoodState,GoodVal).

goodEnough(_,[],_,_,State,Val,State,Val). % no moves - game ended

goodEnough(_,_,Alpha,Beta,State,Val,State,Val):-
  maxToMove(State),Val>Beta,!; % i am a maximum - max played to reach State
  minToMove(State),Val<Alpha,!.% i am a minimum - min played to reach State

goodEnough(Depth,StateList,Alpha,Beta,State,Val,GoodState,GoodVal):-
  newBounds(Alpha,Beta,State,Val,NewAlpha,NewBeta), % refine bounds
  boundedBest(Depth,StateList,NewAlpha,NewBeta,State1,Val1),
  betterOf(State,Val,State1,Val1,GoodState,GoodVal).

% if a max node can reach value greater than alpha - we can raise alpha:
% obviously a max node won't prefer something smaller
newBounds(Alpha,Beta,State,Val,Val,Beta):-
  maxToMove(State),Val > Alpha,!.

% if a min node can reach value smaller than beta - we can make beta smaller:
% obviously a min node won't prefer something greater
newBounds(Alpha,Beta,State,Val,Alpha,Val):-
  minToMove(State),Val<Beta,!.

newBounds(Alpha,Beta,_,_,Alpha,Beta). % bounds unchanged

betterOf(State,Val,State1,Val1,State,Val):-
  maxToMove(State),Val>Val1,!; % if max led to this state - he wants the bigger value (opposite from the book)
  minToMove(State),Val<Val1,!. % if min led to this state - he wants the lower value (opposite from the book)

betterOf(_,_,State1,Val1,State1,Val1).