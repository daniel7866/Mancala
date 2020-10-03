% ---------------------------------------------------------------------
%              20596 - Prolog And Artificial Intelligence
%               Maman 17 - Final Project - Mancala game
%                     AI using alphaBeta
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
% To run the game - call the predicate MainGameLoop
% This is the "main method" of the game
% It will welcome the player and ask him if he wants to view a tutorial
% as well as selecting the game mode, and the difficulty.
% There are two game modes: player vs computer
%                           computer vs computer
% computer vs computer is an automatic game saved to a text file that
% the user can view later.
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% dynamic predicates:
% ---------------------------------------------------------------------
:- dynamic pocket/3. % pocket(pos, player, stones count) - represents a
% specific pocekt in the board game.
:- dynamic turn/1. % turn(player) - represents who can play now.
:- dynamic winner/1. % winner (player) - represents the game's winner.
:- dynamic difficulty/1. % the difficulty is the depth for alpha beta

% ---------------------------------------------------------------------
% memory cleaning of dynamic predicates:
% ---------------------------------------------------------------------
cleanUp:-
  retractall(pocket(_,_,_)), % cleans pocket(pos, player, stones count)
  retractall(turn(_)), % cleans turn(player)
  retractall(difficulty(_)),
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
start(-1):-!. % stop after 5 pockets for each player
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
  move(NextPos,NextBoardSide,NumOfStones),!. % put one stone in each of the next pockets

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
  turn(human),!,
  retract(turn(human)),
  assert(turn(cpu)).

/*************** switch turns - cpu to human ***************/
switchTurns:-
  turn(cpu),!,
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
  %nl,write("You captured "),write(CpuNumOfStones+1),write(" stones!"),nl.

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
  %nl,write("Cpu captured "),write(HumanNumOfStones+1),write(" stones!"),nl.

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
  ((HumanNumOfStones>CpuNumOfStones,assert(winner(human)));%who has more stones in the bank is the winner
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
% get current game state in a list:
% a game state is made like this:
% it has the pockets and their values inside a list=List
% someone pressed on a pocket to get to this state - since we don't know what it is we mark it as _-_
% two underScores: one for pos and one for boardSide
% we need those two for the alphaBeta search - so when it looks at a state it knows where it came from
% and that way it can decide if it's max or min.
% next we have Plaer = whose turn it is in the game right now
% so we get: _pos-_boardSide-List-Player
% ---------------------------------------------------------------------

getCurrentState(_-_-List-Player):-
  getCurrentState([],List1,5),!, % overloading
  pocket(bank,cpu,CpuBank),
  pocket(bank,human,HumanBank),
  insert(bank-cpu-CpuBank,List1,List2), % insert the banks as well
  insert(bank-human-HumanBank,List2,List),
  turn(Player),!. % who plays now in this state

getCurrentState(Acc,Acc,-1):-!. % after done with all pockets from 5 to 0
getCurrentState(Acc,List,Pos):- % using accumulator
  pocket(Pos,cpu,CpuNumOfStones),
  pocket(Pos,human,HumanNumOfStones),
  insert(Pos-cpu-CpuNumOfStones,Acc,Acc1),
  insert(Pos-human-HumanNumOfStones,Acc1,Acc2),
  NextPos is Pos-1,
  getCurrentState(Acc2,List,NextPos),!.

% ---------------------------------------------------------------------
% set current board from a state given in a list:
% pos led to state - BoardSidePocket led to state - [pos-boardside-stones|...] - current player on this state
% pos and BoardSidePocket are _ becase we don't care about them
% ---------------------------------------------------------------------
setBoard(_-_-[]-CurrPlayer):-
  retractall(turn(_)),retractall(winner(_)),
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
  setBoard(State),
  moves(State,[],PossibleMoves,5), % overloading (use accumulator)
  setBoard(OriginalState),!, % reset the board as it was before
  retractall(turn(_)),
  assert(turn(Player)).

% after done checking all pockets from 5 to 0
moves(_,PossibleMoves,PossibleMoves,-1):-!.
moves(State,Acc,PossibleMoves,Pos):-
  move(Pos,BoardSide),!, % change the board and get the current state
  getCurrentState(_-_-AfterMove-Player),
  setBoard(State), % reset the board before the change
  insert(Pos-BoardSide-AfterMove-Player,Acc,Acc1), % write the board and the pos that led to it
  NextPos is Pos-1,!,
  moves(State,Acc1,PossibleMoves,NextPos).
moves(State,Acc,PossibleMoves,Pos):- % if pocket is not valid - skip it
  NextPos is Pos-1,!,
  moves(State,Acc,PossibleMoves,NextPos).

% ---------------------------------------------------------------------
%-------------------------------AlphaBeta------------------------------
% ---------------------------------------------------------------------

% minToMove(State) -> State = [PosPlayed-BoardSide-State]
% A given state saves the pocket and boardSide that led to that state
% This is the OPPOSITE from the meaning in the book in page 285
minToMove(_-human-_-_). % if human played last - he is a minimum player
maxToMove(_-cpu-_-_). % if cpu played last - he is a maximum player

% staticVal gets the huristic value when the depth is 0 on the alphaBeta
% the huristic value is how much stones cpu has in the bank more than human:
% cpu wants to have more stones than human - max player.
% human wants to have more stones than cpu - smaller value = min player
% so heuristic value is simply what cpu has in his bank minus what human player has in his bank
staticVal(State,Val):-
  setBoard(State),
  pocket(bank,human,HumanBank),
  pocket(bank,cpu,CpuBank),
  Val is CpuBank-HumanBank.

%This predicate is getting the heuristic value in case the game has ended
%The heuristic value is simply what is in the cpu's bank minus what is in the human player's bank
%The reason we need an extra predicate for this case is so the computer won't think it has more moves
%even if the opponent's row is empty and the game is done and there is more depth to go
staticValGameEnded(State,Val):-
  setBoard(State),
  gameEnded,
  pocket(bank,human,HumanBank),
  pocket(bank,cpu,CpuBank),
  Val is CpuBank-HumanBank.

%This predicate will run alphaBeta
%It's getting the desired depth to limit alphaBeta(The difficulty of the game will decide on the depth)
%it's getting back goodState which is the right move to do from the current state of the game
%it's getting back it's heuristic value as well
runAlphaBeta(Depth,GoodState,GoodVal):-
  getCurrentState(_-_-State-Player),!,%get the current state of the game - to run alphaBeta from
  alphaBeta(Depth,_-_-State-Player,-9999,9999,GoodState,GoodVal),!,%run alphaBeta(alpha=-9999,beta=9999 instead of infinity)
  setBoard(_-_-State-Player),!.%set the board as it was before the alphaBeta search

%This is the alphaBeta predicate
%it's parameters are:
% Depth of search
% _-_-State-Player = the current node which is the state of the game
% reminder - a game state is made from:
%     pos-boardSide that led to current state (underScore because we don't care about them in this matter)
%     State: the state of the baord = all the pockets and their values
%     Player: current player's turn in this game state
% alpha and beta and the goodState and it's value
alphaBeta(Depth,_-_-State-Player,Alpha,Beta,GoodState,Val):-
  (Depth>0, % depth left to explore
  moves(_-_-State-Player,StateList),StateList\=[],!, % StateList is the possible moves from current state = if game ended the stateList is empty list
  Depth1 is Depth-1,!,
  boundedBest(Depth1,StateList,Alpha,Beta,GoodState,Val)); % work on possible moves
  ((StateList==[],!,staticValGameEnded(_-_-State-Player,Val));% if StateList is empty - the game has ended(nowhere to go from here)
  staticVal(_-_-State-Player,Val),!).%if the depth is 0 we are done searching

boundedBest(Depth,[State|StateList],Alpha,Beta,GoodState,GoodVal):-
  alphaBeta(Depth,State,Alpha,Beta,_,Val),%run alphaBeta on States list's head
  goodEnough(Depth,StateList,Alpha,Beta,State,Val,GoodState,GoodVal).%find the best in the list recursivly

goodEnough(_,[],_,_,State,Val,State,Val):-!. % no moves - game ended - return current state

%pruning of the alphaBeta tree is in this predicate below:
%first row: if the state is max, and the value is above Beta
%           that means that we already got the best we need
%           and no matter what values are next - we will take this one = Prune the tree here!
%--------------------------------------------------------------------------------------------
%second row: if the state is min, and the value is below Alpha
%           that means that we already got the best we need
%           and no matter what values are next - we will take this one = Prune the tree here!
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


% ---------------------------------------------------------------------
% --------------User Interface and main game loop----------------------
% ---------------------------------------------------------------------

%mainGameLoop is the "main menu" of the game:
%it starts off by displaying a welcome message to the player which asks
%the player if he wants to view the manual for the game
%then player is asked to choose between two game modes:
% player vs cpu
% or an automatic game(computer vs itself)
mainGameLoop:-
  write("Welcome to mancala! made by Daniel Fogel"),nl,nl,
  write("Would you like to view a manual and a tutorial of the game? (y or n) followed by a period and press Enter"),nl,nl,
  write("If you wish to quit the game - you can always do it from here simply by typing 'exit' followed by a period and press Enter"),nl,nl,
  repeat,read(Ans),
  ((Ans=='y',printManual,nl,!);
   (Ans=='n',!);
   (Ans=='exit',!);
   (write("You have to choose between y or n followed by a period and press Enter"),nl,fail)), % if player input is invalid
   (((Ans=='exit'),write("GoodBye!"),nl,sleep(1));
  chooseGameMode(GameMode),nl,nl,
  (((GameMode==1),playerVsCpuGame);
    (GameMode==2),cpuVsCpuGame)).

%This is the game mode for the automatic game:
cpuVsCpuGame:-
  start,%initialize the board
  nl,write("Game is being played, writing it to a file called 'automaticGame'..."),nl,
  tell(automaticGame),
  printBoard,
  cpuVsCpuGameLoop,%go to the desired gameMode loop
  cleanUp,%after the game is over - retract all
  told,write("Game is done! You can view the file 'automaticGame' in your game folder."),nl,
  write("Do you want to restart? (y/n) followed by a period and press Enter"),nl,
  !,repeat,read(Ans),
  ((Ans=='y',!,nl,nl,nl,mainGameLoop); % if the player wants to play again - go to the main menu of the game
   (Ans=='n',!,nl,write("Good Bye!"),nl,!);
   (write("You have to choose between y or no"),nl,fail)).  % if player input is invalid


%This predicate - cpuVsCpuGameLoop has 3 implementations:
% 1. game has ended - announce the winner
% 2. it's the cpu's turn - run alphaBeta and make the move automatically
% 3. it's the human player's turn - run alphaBeta and make the move automatically.
cpuVsCpuGameLoop:-
  gameEnded,!,printBoard,write("Game over!"),nl,winner(WINNER),
  (((WINNER == tie),
  write("It's a tie!"),nl);
  write("The winner is "),write(WINNER),nl),
  write("Cpu got "),pocket(bank,cpu,CPU_SCORE),write(CPU_SCORE),
  write(" stones"),nl,
  write("Human player got "),pocket(bank,human,HUMAN_SCORE),write(HUMAN_SCORE),
  write(" stones"),nl.

cpuVsCpuGameLoop:-
  turn(cpu),!,write("Cpu's turn"),nl,
  runAlphaBeta(4,Pocket-BoardSide-_-_,_),move(Pocket,BoardSide),% on the automatic game the depth is 4
  write("cpu chose pocket "),write(Pocket),nl,
  printBoard,cpuVsCpuGameLoop.

cpuVsCpuGameLoop:-
  turn(human),!,write("Human's turn"),nl,
  runAlphaBeta(4,Pocket-BoardSide-_-_,_),move(Pocket,BoardSide),% on the automatic game the depth is 4
  write("human chose pocket "),write(Pocket),nl,
  printBoard,cpuVsCpuGameLoop.

%This is the game mode for the player vs cpu
playerVsCpuGame:-
  chooseDifficulty,nl,
  start,%initialize the board
  printBoard,
  playerVsCpuGameLoop. % start the game

%This predicate - playerVsCpuGameLoop has 3 implementations:
% 1. game has ended - announce the winner
% 2. it's the cpu's turn - run alphaBeta and make the move automatically
% 3. it's the human player's turn - get the desired move from him as an input and make the move.
playerVsCpuGameLoop:-
  gameEnded,!,
  printBoard,
  write("Game over!"),nl,winner(WINNER), % if the game ended - announce the winner
  (((WINNER == tie),
  write("It's a tie!"),nl);
  write("The winner is "),write(WINNER),nl),
  write("Cpu got "),pocket(bank,cpu,CPU_SCORE),write(CPU_SCORE),
  write(" stones"),nl,
  write("Human player got "),pocket(bank,human,HUMAN_SCORE),write(HUMAN_SCORE),
  write(" stones"),nl,
  difficulty(Difficulty),
  write("You have played the "),write(Difficulty),write(" difficulty"),nl,nl,

  cleanUp, % after we are done playing - clean memory
  write("Do you want to play again? (y/n)"),nl,!,
  repeat,read(Ans),
  ((Ans=='y',!,nl,nl,nl,mainGameLoop); % if the player wants to play again - go to the main menu of the game
   (Ans=='n',!,write("Thank you for playing!"),nl,write("Good Bye!"),nl,!);
   (write("You have to choose between y or no"),nl,fail)).  % if player input is invalid

% if it's the turn of cpu - run alphaBeta
playerVsCpuGameLoop:-
  turn(cpu),!,
  write("Cpu's turn"),nl,
  getDifficultyDepth(DepthForAlphaBeta), % get the depth for alphaBeta search
  runAlphaBeta(DepthForAlphaBeta,Pocket-BoardSide-_-_,_),move(Pocket,BoardSide), % get alphaBeta's move recommendation and act on it
  write("cpu chose pocket "),write(Pocket),nl,sleep(3),
  printBoard,
  playerVsCpuGameLoop.

% if it's the turn of human player - read from him
playerVsCpuGameLoop:-
  turn(human),!,
  write("It's your turn"),nl,
  getInput(PlayerInput),((PlayerInput=='stop',write("Game stopped, going back to startup screen..."),cleanUp,nl,nl,sleep(2),mainGameLoop)
  ;
  (move(PlayerInput,human),
  printBoard,playerVsCpuGameLoop)).

% In this game there are two game modes:
% You vs the computer - gameMode 1
% An automatic game - the computer vs itself - gameMode 2
chooseGameMode(GameMode):-
  write("What game mode do you want? (1 or 2) followed by a period and press Enter"),nl,
  write("1. You vs Computer"),nl,
  write("2. Computer vs Computer - an automatic game which is saved to a text file"),nl,
  repeat,read(GameMode),
  ((GameMode == 1,!;GameMode == 2,!);
   write("Choose between 1. or 2."),nl,fail).

% This predicate prints an instructions' manual of the game
% At the end of the manual the player will be asked if he wants to view a live tutorial as well
printManual:-
  nl,nl,write("Game manual:"),nl,
  write("Mancala is a two person board game"),nl,
  write("This game has a board with two sides - one for each player"),nl,
  write("Each player has 6 pockets: pockets 0-5 and a bank"),nl,
  write("At the beginning - all pockets have four stones and the banks are empty"),nl,
  write("The goal is to get more stones in your bank to beat the other player"),nl,
  write("The game is finished when one row is empty,"),nl,
  write("then the player in the opposite row collects all stones remaining in his row."),nl,
  write("Then we count who has more stones in his bank."),nl,
  write("Player with most stones wins!"),nl,
  write("When you choose a pocket you empty it,"),nl,
  write("Then you put one stone in each of the next pockets counter clockwise untill you ran out of stones"),nl,nl,
  write("Speacial moves:"),nl,
  write("Free turn: If your last stone landed in your bank - you get a free turn!"),nl,
  write("Captured: If your last stone landed in an empty pocket and there are stones in the same pocket on the opposite side"),nl,
  write("you'll take all of them to your bank!"),nl,nl,

  write("When it's your turn to play - you can type 'stop' to stop playing and go back to startup screen."),nl,
  write("On startup screen - you can type 'exit' to quit the game."),nl,nl,
  write("Remember: Every time you are being asked to type you need to type your selection followed by a period and press Enter"),nl,nl,
  write("Would you like to view a tutorial on how to play? (y/n)"),nl,
  !,repeat,read(Ans),
  ((Ans=='n');
   (Ans=='y',viewTutorial);
   (write("You have to choose between y or n followed by a period and press Enter"),fail)).

% This predicate will show the user a tutorial on how to play
%  -It's showing the player how the board works
%  -It's showing the player how to gain an extra turn
%  -It's showing the player how to caputre opponent's stones
viewTutorial:-
  start,
  write("This is your board:"),nl,
  printBoard,
  write("Your pockets are on the bottom, and your opponent's are on the top"),nl,
  (turn(human);switchTurns),
  write("This is your turn - to gain extra turn press pocket 2, because that way the last stone will land in your bank"),nl,
  !,repeat,read(ChosenPocket2),
  ((ChosenPocket2 is 2);(write("Type 2 followed by a period and press Enter"),nl,fail)),
  move(2,human),
  printBoard,
  write("Great! now you know how to win another turn!"),nl,nl,

  %next stage in the tutorial - how to capture opponent's stones
  write("Now let's teach you how to capture!"),nl,nl,
  cleanUp,
  start,(turn(human);switchTurns),
  sleep(3),
  emptyCurrPocket(5,human,_),
  printBoard,
  write("Pocket 5 in your side of the board is empty, and in the same pocket on your opponent's side there are stones."),nl,
  write("To gain them all, choose pocket 1 which has 4 stones and that way, the last stone will fall on pocket number 5."),nl,
  write("And because pocket number 5 is empty, and the opponent's pocket 5 is not, you will take his stones + the one on your side"),nl,
  write("Choose pocket 1"),nl,
  !,repeat,read(ChosenPocket1),write("read"),nl,
  ((ChosenPocket1 is 1);(write("Type 1 followed by a period and press Enter"),nl,fail)),
  move(1,human),
  printBoard,
  nl,write("Great! you are ready to play!"),nl,nl,nl,cleanUp.


% Player chooses difficulty for the game:
% The difficulty will be the depth for the alphaBeta search
chooseDifficulty:-
  write("Please choose your difficulty (1 2 3 or 4) followed by a period and press Enter"),nl,
  write("Type '1.' for Easy"),nl,
  write("Type '2.' for Medium"),nl,
  write("Type '3.' for Hard"),nl,
  write("Type '4.' for Extreme"),nl,
  !,repeat,read(Ans),
  ((between1(1,Ans,4);(write("Type a number between 1 and 4 followed by a period"),nl,fail))),!, % if player input is invalid
  (((Ans == 1, Difficulty = easy);
  (Ans == 2, Difficulty = medium);
  (Ans == 3, Difficulty = hard);
  (Ans == 4, Difficulty = extreme)),
  assert(difficulty(Difficulty)));fail.

%This predicate will get the depth for alphaBeta search from the difficulty the player has chosen
getDifficultyDepth(Depth):-
  difficulty(Lvl),
  (
  (Lvl = easy,Depth is 1);
  (Lvl = medium,Depth is 2);
  (Lvl = hard,Depth is 4);
  (Lvl = extreme,Depth is 8)
  ).
%This predicate gets a pocket number from the player - or stops the game if the player wishes to do so
%The number is between 0 and 5
%If the player chose an empty pocket or invalid number it will prompt a message
%and let the player try again
getInput(ChosenPocket):-
  write("Please choose your pocket (integer between 0 - 5 or 'stop' for stopping the game) followed by a period and press Enter."),nl,
  repeat,read(ChosenPocket),((ChosenPocket=='stop');
  not(
  (((not(integer(ChosenPocket));not(between1(0,ChosenPocket,5))),
   write("Must be in range [0,5]"),nl,write("Try again!"),nl);
  (isEmptyPocket(ChosenPocket,human),
   write("Cannot choose empty pocket!"),nl,write("Try again!"),nl)))),!. % if player input is invalid

% ---------------------------------------------------------------------
% --------------Print the board----------------------------------------
% ---------------------------------------------------------------------
printBoard:-
  printBoardNames(cpu,0),
  printBoardLines(0),
  printBoardValues(cpu,0),
  printBoardLines(0),
  printBanks,
  printBoardNames(human,0),
  printBoardLines(0),
  printBoardValues(human,0),
  printBoardLines(0),!,
  nl,nl,nl,nl,nl,nl,nl,nl,nl,
  nl,nl,nl,nl,nl,nl,nl,nl,nl.
  %nl,nl,nl,nl,nl,nl,nl,nl,nl.

%---------print boardLines--------------
printBoardLines(6):-
  nl.
printBoardLines(N):-
  write("          "),%10 spaces = pocket
  write("----------"),
  N1 is N+1,
  printBoardLines(N1).


%---------print cpu board--------------
printBoardNames(Player,6):-
  nl.
printBoardNames(Player,N):-
  ((Player==cpu,write("          "),%10 spaces = pocket
  write("   cpu - "));
  (Player==human,write("          "),%10 spaces = pocket
  write(" human - "))),
  write(N),
  N1 is N+1,
  printBoardNames(Player,N1).

printBoardValues(Player,6):-
  nl.
printBoardValues(Player,N):-
  write("          "),%10 spaces = pocket
  write("|   "),((Player==cpu,pocket(N,cpu,CNT));
  (Player==human),pocket(N,human,CNT)),write(CNT),
  write("    |"),
  N1 is N+1,
  printBoardValues(Player,N1).

% the banks are printed seperatley in their own rows
printBanks:-
  pocket(bank,cpu,CPUBANK),
  pocket(bank,human,HUMANBANK),
  write(" cpu bank "),
  write("                                                                                                              "),%110 spaces
  write("human bank"),nl,
  write("----------"),
  write("                                                                                                              "),%110 spaces
  write("----------"),nl,
  write("|   "),write(CPUBANK),
  write("    |"),
  write("                                                                                                              "),%110 spaces
  write("|   "),write(HUMANBANK),
  write("    |"),nl,
  write("----------"),
  write("                                                                                                              "),%110 spaces
  write("----------"),
  nl.
