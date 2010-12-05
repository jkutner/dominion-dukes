
% Now that we have gone through all the cards, we can total up the Dukes
score([], DukesInHand, DuchiesInHand, Score) :-
    Score is DukesInHand*DuchiesInHand.

score([duchy|Rest], DukesInHand, DuchiesInHand, Score) :-
    Y is DuchiesInHand+1,
    score(Rest, DukesInHand, Y, X),
    Score is 3+X.

score([duke|Rest], DukesInHand, DuchiesInHand, Score) :-
    Y is DukesInHand+1,
    score(Rest, Y, DuchiesInHand, Score).
    
score(Hand, Score) :-
  score(Hand, 0, 0, Score).

play(0, _, Hand, Hand, []) :- !.
    %score(Hand, Score), !.

play(_, 0, Hand, Hand, []) :- !.
    %score(Hand, Score), !.

% take a duchy
play(DukesRemaining, DuchiesRemaining, Hand, FinishedHand, Scores) :-
    X is DuchiesRemaining-1,
    append([duchy], Hand, NewHand),
    play(DukesRemaining, X, NewHand, FinishedHand, PostScores),
	score(NewHand, Score),
	append(PostScores, [Score], Scores).

% take a duke
play(DukesRemaining, DuchiesRemaining, Hand, FinishedHand, Scores) :-
    X is DukesRemaining-1,
    append([duke], Hand, NewHand),
    play(X, DuchiesRemaining, NewHand, FinishedHand, PostScores),
	score(NewHand, Score),
	append(PostScores, [Score], Scores).

play(DukesRemaining, DuchiesRemaining, Hand, Scores) :-
    play(DukesRemaining, DuchiesRemaining, [], Hand, Scores).
    
%high_score(DukesRemaining, DuchiesRemaining, Hand, Scores) :-
%    setof(Score, play(DukesRemaining, DuchiesRemaining, Hand, Score), Set),
%    max_list(Set, Score).
  
%high_score(8, 8, Hand, Scores).

best_play(DukesRemaining, DuchiesRemaining, Scores) :-
	findall(Z, play(DukesRemaining, DuchiesRemaining, _, Z), Set),
	write(Set),
	do_it(Set, Scores).
	
do_it(SetOfSets, Set) :-
	do_it(SetOfSets, 1, Set).
	
do_it(SetOfSets, _, Set) :-
	length(SetOfSets, 1), 
	nth(1, SetOfSets, Set),
	!.
		
do_it(SetOfSets, N, Set) :-
	max_n(SetOfSets, N, Max), %find the max Nth value of a set in setofsets and call it Max
	sets_with_n_of(SetOfSets, N, Max, SetOfSetsWithMaxAtN), %find all sets in setofsets that have Max as the Nth value
	M is N+1,
	do_it(SetOfSetsWithMaxAtN, M, Set).

max_n([A|Set], N, Max) :-
	length(Set, 0),
	nth(N, A, Max),
	!.

max_n([A|Set], N, Max) :-
	max_n(Set, N, M),
	nth(N, A, X),
	X =< M, 
	Max is M, 
	!.
	
max_n([A|_], N, Max) :-
	nth(N, A, Max).	 % we can assume A[N] is the new max
	
sets_with_n_of([], _, _, []) :- !.
	
sets_with_n_of([A|SetOfSets], N, Max, SetOfSetsWithMaxAtN) :-
	\+ nth(N, A, Max),
	sets_with_n_of(SetOfSets, N, Max, SetOfSetsWithMaxAtN).
	
sets_with_n_of([A|SetOfSets], N, Max, SetOfSetsWithMaxAtN) :-
	nth(N, A, Max),
	sets_with_n_of(SetOfSets, N, Max, R),
	append([A], R, SetOfSetsWithMaxAtN).
		
elements_that_have_highest_nth_value([A|Rest], Set, N, Result) :-
	nth(N, A, X),
	max_n(Set, N, Max),
	X >= Max,
	M is N+1,
	elements_that_have_highest_nth_value(Rest, Set, M, R),
	append([A], R, Result).
			
elements_that_have_highest_nth_value([A|Rest], Set, N, Result) :-
	nth(N, A, X),
	max_n(Set, N, Max),
	X < Max,
	M is N+1,
	elements_that_have_highest_nth_value(Rest, Set, M, Result).