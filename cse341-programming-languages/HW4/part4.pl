% Gebze Technical University
% Computer Engineering Department
% CSE341 Programming Languages
% Homework 4 - Part 4
% Author: Ömer Faruk Bitikçioğlu

% Define a Prolog predicate “element(E,S)” that returns true if E is in S
element(E, [E|_]).
element(E,[_|Tail]) :-
    element(E,Tail).

% Define a Prolog predicate “union(S1,S2,S3)” that returns true if S3 is the union of S1 and S2
union([],Set,Set).
union([Head|Tail],Set,Set2):-
    member(Head,Set),!,
    union(Tail,Set,Set2).
union([Head|Tail],Set,[Head|Set2]):-
    not(member(Head,Set)),!,
    union(Tail,Set,Set2).


% Define a Prolog predicate “intersect(S1,S2,S3)” that returns true if S3 is the intersection of of S1 and S2
intersect([],_,[]).
intersect([Head|Tail],Set,[Head|Set2]):-
    member(Head,Set),!,
    intersect(Tail,Set,Set2).
intersect([Head|Tail],Set,Set2):-
    not(member(Head,Set)),
    intersect(Tail,Set,Set2).


% Define a Prolog predicate “equivalent(S1,S2)” that returns true if S1 and S2 are equivalent sets
equivalent(S1, S2) :- subset(S1, S2), subset(S2, S1).