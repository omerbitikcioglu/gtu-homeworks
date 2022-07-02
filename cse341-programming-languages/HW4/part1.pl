% Gebze Technical University
% Computer Engineering Department
% CSE341 Programming Languages
% Homework 4 - Part 1
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

% rules
route(A, B) :-
    flight(A, B), A \= B.

route(A, B) :-
    flight(B, A), A \= B.

route(A, B) :-
    flight(A, X), route(X, B), A \= B.

route(A, B) :-
    flight(B, X), route(X, A), A \= B.