% Gebze Technical University
% Computer Engineering Department
% CSE341 Programming Languages
% Homework 4 - Part 2
% Author: Ömer Faruk Bitikçioğlu

% knowledge base
flight(edirne, edremit).
flight(edremit, erzincan).
flight(istanbul, izmir).
flight(izmir, isparta).
flight(isparta, burdur).
flight(istanbul, antalya).
flight(antalya, konya).
flight(antalya, gaziantep).
flight(istanbul, gaziantep).
flight(istanbul, ankara).
flight(istanbul, van).
flight(istanbul, rize).
flight(ankara, konya).
flight(ankara, van).
flight(van, rize).

distance(edirne, edremit, 251).
distance(edremit, erzincan, 992).
distance(izmir, isparta, 308).
distance(isparta, burdur, 24).
distance(istanbul, izmir, 328).
distance(istanbul, antalya, 482).
distance(istanbul, gaziantep, 847).
distance(istanbul, ankara, 351).
distance(istanbul, van, 1262).
distance(istanbul, rize, 967).
distance(antalya, konya, 192).
distance(antalya, gaziantep, 592).
distance(ankara, konya, 227).
distance(ankara, van, 920).
distance(van, rize, 373).

% rules, SD stands for Shortest Distance
sroute(A, B, SD) :-
    A \= B, distance(A, B, X), SD is X; distance(B, A, X), SD is X.

sroute(A, B, SD) :-
    A \= B, findall(X, distance(A, C, X), Bag), sroute(C, B, X2), shortest(Bag, X1), SD is X1 + X2.

sroute(A, B, SD) :-
    A \= B, findall(X, distance(B, C, X), Bag), sroute(C, A, X2), shortest(Bag, X1), SD is X1 + X2.

shortest([X], SD) :- SD is X.
shortest([X,Y|Tail], SD) :-
    X > Y -> shortest([Y|Tail], SD); shortest([X|Tail], SD).