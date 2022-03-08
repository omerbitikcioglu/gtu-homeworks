% Gebze Technical University
% Computer Engineering Department
% CSE341 Programming Languages
% Homework 4 - Part 3
% Author: Ömer Faruk Bitikçioğlu

% facts
when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

% Define/write a predicate “schedule(S,P,T)” that associates a student to a place and time of class
schedule(Student, Class, Time) :- enroll(Student, C), when(C, T), Class is C, Time is T.

% Define/write another predicate “usage(P,T)” that gives the usage times of a classroom
usage(Room, Time) :- where(C, Room), when(C, T), Time is T.

% Define/write another predicate “conflict(X,Y)” that gives true if X and Y conflicts due to classroom or time
conflict(Class_X, Class_Y) :- where(Class_X, Room), where(Class_Y, Room).
conflict(Class_X, Class_Y) :- when(Class_X, Time), when(Class_Y, Time).

% Define/write another predicate “meet(X,Y)” that gives true if student X and student Y are present in the same classroom at the same time
meet(Student_X, Student_Y) :- enroll(Student_X, X), enroll(Student_Y, X).
meet(Student_X, Student_Y) :- enroll(Student_X, X), enroll(Student_Y, Y), when(X, T), when(Y, T), where(X, C), where(Y, C).