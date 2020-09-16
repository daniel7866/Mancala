%pocket(pos, player, stones count)
%turn(player) = who can play now
:- dynamic pocket/3.
:- dynamic turn/1.

start:-
  start(5),
  assert(pocket(bank,human,0)),
  assert(pocket(bank,cpu,0)),
  assert(turn(human)).%human starts first

start(-1).
start(N):-
  assert(pocket(N,human,4)),%game starts with 4 stones in each pocket
  assert(pocket(N,cpu,4)),
  N1 is N-1,
  start(N1).

%Game has ended if one side has no stones left
ended:-
  is_empty(5,human);
  is_empty(5,cpu).

is_empty(-1).
is_empty(N,Player):-
  pocket(N,Player,0),
  N1 is N-1,
  is_empty(N1,Player).

%put a stone in the pocket
putInPocket(Pos,Player):-
  pocket(Pos,Player,N),
  N1 is N+1,
  retract(pocket(Pos,Player,N)),
  assert(pocket(Pos,Player,N1)).

%empty this pocket - take out all of it's rocks
empty(Pos,Player,N):-
  pocket(Pos,Player,N),
  retract(pocket(Pos,Player,N)),
  assert(pocket(Pos,Player,0)).
