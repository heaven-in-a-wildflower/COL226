%PART-A

mem(X,[]) :- fail.
mem(X,[X|T]). %Definition has been modified to accomodate multiple truth paths
mem(X,[Y|T]) :- mem(X,T).

mem((X,X), ref_clos(R,S)) :- mem(X,S).
mem((X,Y), ref_clos(R,S)) :- mem((X,Y),R).
% mem((a,a), ref_clos([(a,b)],[b]])).
% mem((a, a), trans_clos([], [a])).

mem((X,Y), sym_clos(R,S)) :- mem((X,Y),R).
mem((X,Y), sym_clos(R,S)) :- mem((Y,X),R).

mem((X,Y), trans_clos(R,S,History)) :- mem((X,Y),R).
mem((X,Y), trans_clos(R,S,History)) :- mem((X,Z),R), not(mem((Z,Y), History)), mem((Z,Y),trans_clos(R,S,[(X,Y)|History])).
% History parameter has been included to eliminate infinite loops
% To check for membership, History must be replaced by the empty list in test cases

% mem((a, c), trans_clos([(a, b), (b, c)], [a, b, c],[])).
% mem((a, d), trans_clos([(a, b), (b, c), (c, d)], [a, b, c, d],[])).
% mem((a, d), trans_clos([(a, b), (b, c)], [a, b, c],[])).
% mem((x, w), trans_clos([(x, y), (y, z)], [x, y, z, w],[]))).
% mem((x, w), trans_clos([(x, y), (y, w)], [x, y, z],[]))).
% mem((a, a), trans_clos([], [a],[])).
% mem((a, a), trans_clos([], [],[])).
% mem((a, b), trans_clos([], [a,b],[])).
% mem((1,4),trans_clos([(1,2),(3,4),(1,3)],[1,2,3],[])).
% mem((1,4),trans_clos([(1,3),(3,4),(1,2)],[1,2,3],[])).


mem((X,Y), ref_sym_clos(R,S)) :- mem((X,Y), sym_clos(R,S)).
mem((X,Y), ref_sym_clos(R,S)) :- mem((X,Y), ref_clos(sym_clos(R,S),S)).
% mem((c, c), ref_sym_clos([(a, b), (b, c)], [a, b, c])).
% mem((c, b), ref_sym_clos([(a, b), (b, c)], [a, b, c, d])).
% mem((c, b), ref_sym_clos([(a, b), (b, c)], [a, c, d])).
% mem((a, a), ref_sym_clos([], [a])).

mem((X,Y), ref_trans_clos(R,S,History)) :- mem((X,Y), ref_clos(R,S)).
mem((X,Y), ref_trans_clos(R,S,History)) :- mem((X,Y), ref_clos(trans_clos(R,S,History),S)).
% To check for membership, History must be replaced by the empty list in test cases
% mem((a, a), ref_trans_clos([(a, b), (b, c)], [a, b, c],[])).
% mem((a, d), ref_trans_clos([(a, b), (b, c), (c, d)], [a, b, c, d],[])).
% mem((c, c), ref_trans_clos([(c,b),(b,a)],[a,b],[])).
% mem((a, d), ref_trans_clos([(a, b), (b, c)], [a, b, c],[])).
% mem((x, w), ref_trans_clos([(x, y), (y, z)], [x, y, z, w],[]))).
% mem((x, w), ref_trans_clos([(x, y), (y, w)], [x, y, z],[]))).
% mem((a, a), ref_trans_clos([], [a],[])).
% mem((a, a), ref_trans_clos([], [],[])).
% mem((a, b), ref_trans_clos([], [a,b],[])).

mem((X,Y), trans_sym_clos(R,S,History)) :- mem((X,Y), sym_clos(R,S)).
mem((X,Y), trans_sym_clos(R,S,History)) :- mem((X,Y), trans_clos(sym_clos(R,S),S,History)).
% mem((2,3), trans_sym_clos([(1,2),(1,3)],[1,2,3],[])) #Differentiates between trans_clos(sym_clos) and sym_clos(trans_clos)
% To check for membership, History must be replaced by the empty list in test cases
% mem((d, a), trans_sym_clos([(a, b), (b, c), (c, d)], [a, b, c, d],[])).
% mem((x, w), trans_sym_clos([(x, y), (y, w)], [x, y, z],[]))).
% mem((a, b), trans_sym_clos([(b,a)], [a,b],[])).
% mem((a, a), trans_sym_clos([], [],[])).
% mem((a, b), trans_sym_clos([], [a, b],[])).
% mem((a, c), trans_sym_clos([(a, b), (b, a), (b, c)], [a, b, c],[])).
% mem((c, a), trans_sym_clos([(a, b), (b, a), (b, c)], [a, b, c],[])).

mem((X,Y), trans_sym_ref_clos(R,S,History)) :- mem((X,Y), trans_sym_clos(R,S,History)).
mem((X,Y), trans_sym_ref_clos(R,S,History)) :- mem((X,Y), ref_clos(trans_sym_clos(R,S,History),S)).
% To check for membership, History must be replaced by the empty list in test cases
% mem((d, a), trans_sym_ref_clos([(a, b), (b, c), (c, d)], [a, b, c, d],[])).
% mem((x, w), trans_sym_ref_clos([(x, y), (y, w)], [x, y, z],[]))).
% mem((a, b), trans_sym_ref_clos([(b,a)], [a,b],[])).
% mem((a, a), trans_sym_ref_clos([], [],[])).
% mem((a, b), trans_sym_ref_clos([], [a, b],[])).
% mem((a, c), trans_sym_ref_clos([(a, b), (b, a), (b, c)], [a, b, c],[])).
% mem((c, a), trans_sym_ref_clos([(a, b), (b, a), (b, c)], [a, b, c],[])).
% mem((2,5),trans_sym_ref_clos([(1,2),(2,3),(1,5)],[1,2,3,5],[])).
% mem((1,1),trans_sym_ref_clos([(1,2),(2,3),(1,5)],[1,2,3,5],[])).
% mem((1,2),trans_sym_ref_clos([(1,3),(3,1)],[1,2,3],[])).
% mem((5,2),trans_sym_ref_clos([(1,2),(2,3),(1,5)],[1,2,3,5],[])).

%*******************************************************************************
% PART-B
/*  del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */ 
del(X, [ ] , [ ]) :- !.
del(X, [X|R], Z) :- del(X, R, Z), !.
del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.
% del(1,[1,2,3,1],X).
%  del(1,[1,2,3],X).
% del(1,[2,3],X).

/*  remdups(L, L1) remove duplicates from a list L to get L1 */
remdups([ ], [ ]) :- !.
remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).
  
/* append(L1, L2, L3) -- append lis  L1 to list L2 to get list  L3 */
append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).
% append([1,2,3],[4,5,6],X).
% append([1,2],[],X).
% append([1,2],X,[1,2,1,2]).

/* mapcons(X,L1, L2) --  cons the element X to each list in L1 to get L2 */
mapcons(X, [ ], [ ]) :- !.
mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).
% mapcons(1,[2,3,4],X).
% mapcons(1,[[3,4],[5,6]],X).
% mapcons(1,[(3,4),(5,6)],X).
% mapcons(1,[],X).
% mapcons(1,[1],X).


/* powerI( S, P1): Here is an implementation of powerset of S */

/* Assuming no duplicates in S1, S2

 here is an implementation of union of S1, S2 */
unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).
% Test cases for unionI/3
% unionI([], [a, b, c],X).
% unionI([a, b], [], X)).
% unionI([a], [b, c], X).
% unionI([b, c], [a], X).
% unionI([a, b], [a], X).
% unionI([], [], X).
% unionI([a, b, c], [b, c, d],X).

powerI([ ], [ [ ] ]) :- !.
powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).
% Test cases for powerI/2
% powerI([], X).
% powerI([a], X).
% powerI([a, b], X).
% powerI([a, b, c], X).
% powerI([a, b], [[a,b],[a],[b],[]]).
% powerI([a, b], [[],[a],[b],[a,b]]).
% powerI([a, b], [[a],[b],[a,b],[]]).
% powerI(X, [[a],[b],[a,b],[]]). ??

member(X,[]) :- fail.
member(X,[X|T]) :- !.
member(X,[Y|T]) :- member(X,T).

intersection([],X,[]).
intersection([X|T],Y,[X|Z]) :- member(X,Y), intersection(T,Y,Z).
intersection([X|T],Y,Z) :- not(member(X,Y)), intersection(T,Y,Z).
% Test cases for intersection/3
% intersection([], [a, b, c], X).
% intersection([a, b, c], [], X).
% intersection([a, b, c], [c, f, a], X).
% intersection([a, b, c], [x, y, z], X).
% intersection([a, b, c], [a, b, c], X).
% intersection([a, b, c], [x, y, z, c, b, a], X).
% intersection([],[],X).

difference([],Y,[]).
difference([X|T],Y,[X|Z]) :- not(member(X,Y)), difference(T,Y,Z).
difference([X|T],Y,Z) :- member(X,Y), difference(T,Y,Z).
% difference([], [a, b, c], X).
% difference([a, b, c], [], X).
% difference([a, b, c], [c, f, a], X).
% difference([a, b, c], [x, y, z], X).
% difference([a, b, c], [a, b, c], X).
% difference([a, b, c], [x, y, z, c, b, a], X).
% difference([],[],X).

cartesian([],S,[]).
cartesian(S,[],[]).
pairing(X,[Y|T],[[X,Y]|Z]) :- pairing(X,T,Z).
pairing(X,[],[]).
cartesian([X|T],Y,C) :- pairing(X,Y,W), cartesian(T,Y,Z), append(W,Z,C).
% cartesian([], [1, 2], X).
% cartesian([a], [1, 2], X).
% cartesian([a], [1], X).
% cartesian([a], [], X).
% cartesian([a, b], [1, 2], X).
% cartesian([b], [1, 2], X).
% cartesian([a], [1, 2],X).
% cartesian([a], [1, 2], [[a, 1], [a, 2], [a, 1], [a, 2]],X)).
% cartesian([a], [2], [[a, 1], [a, 2], [a, 1], [a, 2]],X))).

%Q7
%To check if the power set obtained from two different orderings of the set under consideration are equal, 
%we can perform an elemnt by elemnt comparison of the power sets.
%Alternatively, the power sets are equal if the corresponding(parent) sets are equal.
%Therefore, we might as well apply eqset procedure as stated in the notes.