%Base case: Integers
has_type(Gamma, N, int) :- integer(N) , !.

%Base case: Booleans
has_type(Gamma, true, bool) :- !.
has_type(Gamma, false, bool):- !.

%Variables
has_type([],var(X),T) :- fail.
has_type([pair(X,T)|Tail],var(X),T) :- !.
has_type([pair(Y,T1)|Tail],var(X),T) :- has_type(Tail,var(X),T),!.

%Arithmetic Operations
has_type(Gamma, plus(E1,E2), int) :- has_type(Gamma, E1, int), has_type(Gamma, E2, int).
has_type(Gamma, times(E1,E2), int) :- has_type(Gamma, E1, int), has_type(Gamma, E2, int).
has_type(Gamma, exp(E1,E2), int) :- has_type(Gamma, E1, int), has_type(Gamma, E2, int).

%Logical Operations over Booleans
has_type(Gamma, negation(E), bool) :- has_type(Gamma, E, bool).
has_type(Gamma, and(E1,E2), bool) :- has_type(Gamma, E1, bool), has_type(Gamma, E2, bool).
has_type(Gamma, or(E1,E2), bool) :- has_type(Gamma, E1, bool), has_type(Gamma, E2, bool).
has_type(Gamma, xor(E1,E2), bool) :- has_type(Gamma, E1, bool), has_type(Gamma, E2, bool).
has_type(Gamma, xnor(E1,E2), bool) :- has_type(Gamma, E1, bool), has_type(Gamma, E2, bool).
has_type(Gamma, nand(E1,E2), bool) :- has_type(Gamma, E1, bool), has_type(Gamma, E2, bool).
has_type(Gamma, nor(E1,E2), bool) :- has_type(Gamma, E1, bool), has_type(Gamma, E2, bool).

%Logical operations over integers
has_type(Gamma, equals(E1,E2), bool) :- has_type(Gamma, E1, int), has_type(Gamma, E2, int).
has_type(Gamma, greater_than(E1,E2), bool) :-  has_type(Gamma, E1, int), has_type(Gamma, E2, int).
has_type(Gamma, lesser_than(E1,E2), bool) :-  has_type(Gamma, E1, int), has_type(Gamma, E2, int).
has_type(Gamma, is_divisible(E1,E2), bool) :- has_type(Gamma, E1, int), has_type(Gamma, E2, int).

%Extra
%Implemented record(a homogenous container) as a list associated with a type T.
%Therefore operations over records of different types are forbidden. 
has_type(Gamma, ([]), (record,T)):-!.
has_type(Gamma,[X|Tail],(record,T)):-has_type(Gamma,X,T),has_type(Gamma,Tail,(record,T)).

has_type(Gamma, union(E1,E2), (record,T)) :- has_type(Gamma, E1, (record,T)), has_type(Gamma, E2, (record,T)).
has_type(Gamma, intersect(E1,E2), (record,T)) :- has_type(Gamma, E1, (record,T)), has_type(Gamma, E2, (record,T)).
has_type(Gamma, diff(E1,E2), (record,T)) :- has_type(Gamma, E1, (record,T)), has_type(Gamma, E2, (record,T)).
has_type(Gamma, symm_diff(E1,E2), (record,T)) :- has_type(Gamma, E1, (record,T)), has_type(Gamma, E2, (record,T)).
has_type(Gamma, length(E), int) :- has_type(Gamma,E,(record,T)).

%Some Useful Test cases

% ?- has_type(Gamma,intersect([1,2,3],[]),T).
% T = (record, int).

% ?- has_type(Gamma,intersect([],[]),T).
% T = (record, _).

% ?- has_type(Gamma,intersect([1],[]),T).
% T = (record, int).

% ?- has_type(Gamma,intersect([1],[true]),T).
% false.

% ?- has_type(Gamma,intersect([1],union([1,2,3],[4,5])),T).
% T = (record, int).

% ?- has_type(Gamma,intersect([1],union([1,2,3],[4,true])),T).
% false.

% ?- has_type(Gamma,intersect([1],diff([1,2,3],[])),T).
% T = (record, int).

% ?- has_type(Gamma,[1,2,3],T).
% T = (record, int).

% ?- has_type(Gamma,[],T).
% T = (record, _).

% ?- has_type(Gamma,[true,false],T).
% T = (record, bool).

% ?- has_type(Gamma,[true,1],T).
% false.

% ?- has_type(Gamma,[false,1],T).
% false.

% ?- has_type(Gamma,[[1,2,3],[3,4]],T).
% T = (record, record, int).

% ?- has_type(Gamma,union([1,2,3],[4,5,6]),T).
% T = (record, int).

% ?- has_type(Gamma,intersect([1,2,3],[4,5,6]),T).
% T = (record, int).

% ?- has_type(Gamma,intersect([1,2,3],[true,false]),T).
% false.

% ?- has_type([pair(x1,int),pair(x2,int),pair(x3,bool)],or(greater_than(var(x1),var(x2)),var(x3)),T).
% T = bool.

% ?- has_type([pair(x1,int),pair(x2,int),pair(x1,bool)],or(greater_than(var(x1),var(x2)),var(x3)),T).
% false.

% ?- has_type(Gamma,or(var(x),greater_than(4,var(x))),T).
% Gamma = [pair(x, bool), pair(x, int)|_],
% T = bool.

% ?- has_type(Gamma,or(var(x),equals(3,5)),T).
% Gamma = [pair(x, bool)|_],
% T = bool.

% ?- has_type(Gamma,or(greater_than(var(x1),times(plus(var(x3),5),var(x2))),equals(3,5)),T).
% Gamma = [pair(x1, int), pair(x3, int), pair(x2, int)|_],
% T = bool.

% ?- has_type([pair(x1,bool)|Z],or(greater_than(var(x1),times(plus(var(x3),5),var(x2))),equals(3,5)),T).
% Z = [pair(x1, int), pair(x3, int), pair(x2, int)|_],
% T = bool.

% ?- has_type([pair(x1,bool),pair(x2,int),pair(x3,int)],or(greater_than(var(x1),times(plus(var(x3),5),var(x2))),equals(3,5)),T)
% .
% false.

% ?- has_type([pair(x1,bool),pair(x2,int),pair(x3,bool)],or(greater_than(var(x1),times(plus(var(x3),5),var(x2))),equals(3,5)),T
% ).
% false.

% ?- has_type([pair(x1,t1),pair(x2,t2)],var(x2),t2).
% true.

% ?- has_type(Gamma,or(var(x1),var(x2)),T).
% Gamma = [pair(x1, bool), pair(x2, bool)|_],
% T = bool.

% ?- has_type([pair(x1,int),pair(x2,int)],greater_than(var(x1),var(x2)),T).
% T = bool.

% ?- has_type([pair(x1,int),pair(x2,bool)],greater_than(var(x1),var(x2)),T).
% false.

% ?- has_type([pair(x1,int),pair(x2,int),pair(x3,bool)],or(greater_than(var(x1),var(x2)),var(x3)),T).
% T = bool.
