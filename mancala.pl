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
  move(Pos,BoardSide), % change the board and get the current state
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

staticValGameEnded(State,Val):-
  %getCurrentState(OriginalState),
  setBoard(State),
  gameEnded,
  pocket(bank,human,HumanBank),
  pocket(bank,cpu,CpuBank),
  Val is CpuBank-HumanBank.
  %setBoard(OriginalState).

runAlphaBeta(Depth,GoodState,GoodVal):-
  getCurrentState(_-_-State-Player),
  alphaBeta(Depth,_-_-State-Player,-9999,9999,GoodState,GoodVal),
  setBoard(_-_-State-Player).

alphaBeta(Depth,_-_-State-Player,Alpha,Beta,GoodState,Val):-
  (Depth>0,
  moves(_-_-State-Player,StateList),StateList\=[],!, % if game ended the stateList is empty list
  Depth1 is Depth-1,
  boundedBest(Depth1,StateList,Alpha,Beta,GoodState,Val));
  ((StateList==[],staticValGameEnded(_-_-State-Player,Val));
  staticVal(_-_-State-Player,Val)).

boundedBest(Depth,[State|StateList],Alpha,Beta,GoodState,GoodVal):-
  alphaBeta(Depth,State,Alpha,Beta,_,Val),
  goodEnough(Depth,StateList,Alpha,Beta,State,Val,GoodState,GoodVal).

goodEnough(_,[],_,_,State,Val,State,Val). % no moves - game ended

%pruning
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
  welcomeMessage,nl,nl,
  chooseGameMode(GameMode),nl,nl,
  (((GameMode==1),playerVsCpuGame);
    (GameMode==2),cpuVsCpuGame).

%the automatic game:
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
  runAlphaBeta(2,Pocket-BoardSide-_-_,_),move(Pocket,BoardSide),
  write("cpu chose pocket "),write(Pocket),nl,
  printBoard,cpuVsCpuGameLoop.

cpuVsCpuGameLoop:-
  turn(human),!,write("Human's turn"),nl,
  runAlphaBeta(2,Pocket-BoardSide-_-_,_),move(Pocket,BoardSide),
  write("human chose pocket "),write(Pocket),nl,
  printBoard,cpuVsCpuGameLoop.


playerVsCpuGame:-
  chooseDifficulty,
  start,%initialize the board
  printBoard,
  playerVsCpuGameLoop,
  cleanUp, % after we are done playing - clean memory
  write("Do you want to play again? (y/n)"),nl,!,
  repeat,read(Ans),
  ((Ans=='y',!,nl,nl,nl,mainGameLoop); % if the player wants to play again - go to the main menu of the game
   (Ans=='n',!,write("Thank you for playing!"),nl,write("Good Bye!"),nl,!);
   (write("You have to choose between y or no"),nl,fail)).  % if player input is invalid

%This predicate - playerVsCpuGameLoop has 3 implementations:
% 1. game has ended - announce the winner
% 2. it's the cpu's turn - run alphaBeta and make the move automatically
% 3. it's the human player's turn - get the desired move from him as an input and make the move.
playerVsCpuGameLoop:-
  gameEnded,!,printBoard,write("Game over!"),nl,winner(WINNER), % if the game ended - announce the winner
  (((WINNER == tie),
  write("It's a tie!"),nl);
  write("The winner is "),write(WINNER),nl),
  write("Cpu got "),pocket(bank,cpu,CPU_SCORE),write(CPU_SCORE),
  write(" stones"),nl,
  write("Human player got "),pocket(bank,human,HUMAN_SCORE),write(HUMAN_SCORE),
  write(" stones"),nl.

playerVsCpuGameLoop:-
  turn(cpu),!,write("Cpu's turn"),nl,difficulty(DepthForAlphaBeta),
  runAlphaBeta(DepthForAlphaBeta,Pocket-BoardSide-_-_,_),move(Pocket,BoardSide),
  write("cpu chose pocket "),write(Pocket),nl,sleep(3),
  printBoard,playerVsCpuGameLoop.
playerVsCpuGameLoop:-
  turn(human),!,write("It's your turn"),nl,
  getInput(ChosenPocket),move(ChosenPocket,human),
  printBoard,playerVsCpuGameLoop.

% In this game there are two game modes:
% You vs the computer - gameMode 1
% An automatic game - the computer vs itself - gameMode 2
chooseGameMode(GameMode):-
  write("What game mode do you want? (1 or 2) followed by a period and press Enter"),nl,
  write("1. You vs Computer"),nl,
  write("2. Computer vs Computer - an automatic game"),nl,
  repeat,read(GameMode),
  ((GameMode == 1,!;GameMode == 2,!);
   write("Choose between 1. or 2."),nl,fail).

welcomeMessage:-
  write("Welcome to mancala! made by Daniel Fogel"),nl,
  write("Would you like to view the game manual? (y or n) followed by a period and press Enter"),nl,
  repeat,read(Ans),
  ((Ans=='y',printManual,nl,!);
   (Ans=='n',!);
   (write("You have to choose between y or n followed by a period and press Enter"),nl,fail)). % if player input is invalid

% This predicate prints an instructions' manual of the game
printManual:-
  nl,nl,write("Game manual:"),nl,
  write("Mancala is a two person board game"),nl,
  write("This game has a board with two sides - one for each player"),nl,
  write("Each player has 6 pockets: pockets 0-5 and a bank"),nl,
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
  write("you'll take all of them to your bank!"),nl.

% Player chooses difficulty for the game:
% The difficulty will be the depth for the alphaBeta search
chooseDifficulty:-
  write("Please choose your difficulty (1 2 3 or 4) followed by a period and press Enter"),nl,
  write("Type '1.' for Easy"),nl,
  write("Type '2.' for Medium"),nl,
  write("Type '3.' for Hard"),nl,
  write("Type '4.' for Extreme  ~warning: this will cause longer respond time from the computer"),nl,
  !,repeat,read(Ans),
  ((between1(1,Ans,4);(write("Type a number between 1 and 4 followed by a period"),nl,fail))),!, % if player input is invalid
  assert(difficulty(Ans));fail.
%This predicate gets a pocket number from the player
%The number is between 0 and 5
%If the player chose an empty pocket or invalid number it will prompt a message
%and let the player try again
getInput(ChosenPocket):-
  write("Please choose your pocket (integer between 0 - 5) followed by a period and press Enter."),nl,
  repeat,read(ChosenPocket),
  not(
  (((not(integer(ChosenPocket));not(between1(0,ChosenPocket,5))),
   write("Must be in range [0,5]"),nl,write("Try again!"),nl);
  (isEmptyPocket(ChosenPocket,human),
   write("Cannot choose empty pocket!"),nl,write("Try again!"),nl))),!. % if player input is invalid

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
