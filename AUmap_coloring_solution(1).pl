% This program finds possible ways to colour a map with given no. of colours so that neighbouring 
% states do not share the same colour. Input to the program would be the colour list, state list and 
% list of adjacencies of a state with others.
% The design of the program is to use pure prolog paradigm. It is to be used solely for education purposes 
% in the area of search, constraint programming and logic.
%
% Author: Tin Aung Win
% May 2020
%


% Color list

colorList([red,green,blue]).

% States list

states([sa,nt,q,wa,nsw,v,t]).

% Adjacent state list

adjacent(wa,[nt, sa]). 
adjacent(nt,[wa,sa,q]).
adjacent(sa,[wa,nt,q,nsw,v]).         
adjacent(q,[nt,sa,nsw]). 
adjacent(nsw,[v,sa,q]).         
adjacent(v,[nsw,sa]).
adjacent(t,[]).

/* Helper predicates. */

head([],[]).
head([A|B], A).

tail([],[]).
tail([_|B], B).

concatenate(X,Y,Z) :- cons(X,Y,Z),!.

cons([],[],[]).
cons([X],[],[X]).
cons([],[X],[X]).
cons(X,[],[X]).
cons([],X,[X]).
cons(X,[H|T],[X,H|T]):-!.
cons(X,Y,[X,Y]).

% ------- Map coloring predicates ----------

%pickColor and enumerateColor recurse throuh the color list and provide next available color, if any.

enumerateColor([],_) :-
	!,
	fail.

enumerateColor([AvailColor|_], AvailColor).

enumerateColor([_|Rest], AvailColor) :-
	enumerateColor(Rest, AvailColor).	

pickColor(AvailColor) :-
		colorList(ColorList),
		enumerateColor(ColorList, AvailColor).

% setColor create state,color pair.

setColor(X,Y, [X,Y]).

% pickAdjacentStateColor collect all colors painted in adjacent states, if any.

pickAdjacentStateColor([],_,SC).

pickAdjacentStateColor([X|AS], [[State|Color] | T], SC) :- 
	member([X|C1],[[State|Color]|T]), 
	concatenate(C1, Temp, SC1), 
	append(C1, Temp, SC1),
	pickAdjacentStateColor(AS,[[State|Color] | T],SC2),
	!,
	concatenate(SC1, SC2, SC3),flatten(SC3,SC).

pickAdjacentStateColor([H|AT], CurrentColor, SC) :- 
	pickAdjacentStateColor(AT, CurrentColor, SC).

% getAvailColor interate through the neighbouring state colors, if any, and pick the first available color which is not used yet

getAvailColor([],_,_) :- 
	!,
	fail.

getAvailColor(AvailableColorList, CurrentColorList, Color) :- 
	permutation(AvailableColorList, CurrentColorList),
	!,
	fail.

getAvailColor(AvailableColorList, CurrentColorList, Color) :- 
	subtract(AvailableColorList, CurrentColorList, [Color|_]). 

getAvailColor(AvailableColorList, CurrentColorList, Color) :- 
	subtract(AvailableColorList, CurrentColorList, [_|RemainingList]),
	getAvailColor(RemainingList, CurrentColorList, Color).
 
% get color picks next available color.

getColor([],X):-
	!,
	pickColor(X).

getColor(CL,C):-
	colorList(X),
	getAvailColor(X, CL, C).

% painttTheStates picks available color for a state and update the state,color pair in the current list of painted states.

paintTheStates(State, ListofPaintedStateAndColorPairs, UpdatedStateColorPairs) :- 
	adjacent(State, AdjacentStateList), 
	pickAdjacentStateColor(AdjacentStateList, ListofPaintedStateAndColorPairs, UsedColor), 
	getColor(UsedColor,AvailableColor), 
	setColor(State,AvailableColor,NewStateColorPair), 
	concatenate(NewStateColorPair, ListofPaintedStateAndColorPairs, UpdatedStateColorPairs).

% paintAll recurse through the entire statelist.

paintAll([], R1, Result):- =(R1,Result).

paintAll([H|T], ListofPaintedStateAndColorPairs, Result):- 
	paintTheStates(H, ListofPaintedStateAndColorPairs, UpdatedListofStateAndColorPairs), 
	paintAll(T, UpdatedListofStateAndColorPairs, Result).

% paint initial the first state and color pair for paintAll to carry out the rest.

paint([H|T], Color, Result) :- 
	setColor(H,Color,StateAndColorPair), 
	paintAll(T, [StateAndColorPair], Result).

% paintmap find all possible solutions for given states with all colors 
 
paintmap(Result) :- 
	states([H|T]), 
	pickColor(AColor),
	paint([H|T], AColor, Result).
