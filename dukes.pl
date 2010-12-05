%-------- Scoring a hand

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

%--------- Building up all possible hands

play(0, _, Hand, Hand) :- !.

play(_, 0, Hand, Hand) :- !.

% take a duchy
play(DukesRemaining, DuchiesRemaining, Hand, FinishedHand) :-
    X is DuchiesRemaining-1,
    append([duchy], Hand, NewHand),
    play(DukesRemaining, X, NewHand, FinishedHand).

% take a duke
play(DukesRemaining, DuchiesRemaining, Hand, FinishedHand) :-
    X is DukesRemaining-1,
    append([duke], Hand, NewHand),
    play(X, DuchiesRemaining, NewHand, FinishedHand).

play(DukesRemaining, DuchiesRemaining, Hand) :-
    play(DukesRemaining, DuchiesRemaining, [], Hand).

%---------- Main entry for program

best_play(DukesRemaining, DuchiesRemaining) :-
    findall(Z, play(DukesRemaining, DuchiesRemaining, Z), SetOfHands),
    %write(SetOfHands),nl,
    max_hand(SetOfHands, BestHand),
    reverse(BestHand, PrintableBestHand),
    write('Best Hand: '),write(PrintableBestHand),nl,
    score_by_round(BestHand, Scores),
    reverse(Scores, PrintableScores),
    write('Score by Round: '),write(PrintableScores),nl.
    
%---------- The algorithm to find the best ordered hand out of all hands

max_hand(SetOfSets, Set) :-
    max_hand(SetOfSets, 1, Set).
    
max_hand([Set|[]], _, Set) :- !.
        
max_hand(SetOfSets, N, Set) :-
    max_n(SetOfSets, N, Max), 
    sets_with_n_of(SetOfSets, N, Max, SetOfSetsWithMaxAtN), 
    M is N+1,
    max_hand(SetOfSetsWithMaxAtN, M, Set).

%---------- Determine the highest scoring hand at a giving round (n) for the given hands

max_n([Set|RemainingSets], N, Max) :-
    length(RemainingSets, 0),
    nth_score(N, Set, Max), 
    tail(Set, N, Tail),
    score(Tail, Max),
    !.

max_n([Set|RemainingSets], N, Max) :-
    max_n(RemainingSets, N, M),
    nth_score(N, Set, X),
    X =< M, 
    Max is M, 
    !.
    
max_n([A|_], N, Max) :-
    nth_score(N, A, Max). % if we got this far, then this is the new max
    
%----------- Find all the hands that have a score of Max at round N

sets_with_n_of([], _, _, []) :- !.
    
sets_with_n_of([A|SetOfSets], N, Max, SetOfSetsWithMaxAtN) :-
    \+ nth_score(N, A, Max),
    sets_with_n_of(SetOfSets, N, Max, SetOfSetsWithMaxAtN).
    
sets_with_n_of([A|SetOfSets], N, Max, SetOfSetsWithMaxAtN) :-
    nth_score(N, A, Max),
    sets_with_n_of(SetOfSets, N, Max, R),
    append([A], R, SetOfSetsWithMaxAtN).
    
nth_score(N, Set, Score) :-
    tail(Set, N, Tail),
    score(Tail, Score).
    
%----------- Returns all N elements of a Set after and including the Nth element

tail(Set, N, Set) :-
    length(Set, L),
    L =< N,
    !.

tail([_|R], N, Tail) :- 
    tail(R, N, Tail).

%------------ Recursively score each round of a given hand
    
score_by_round(Hand, Scores) :-
    score(Hand, Score),
    score_by_round_recursive(Hand, RemainingScores),
    append([Score], RemainingScores, Scores).

score_by_round_recursive([_|[]], []) :- !.

score_by_round_recursive([_|Hand], Scores) :-
    score(Hand, Score),
    score_by_round_recursive(Hand, RemainingScores),
    append([Score], RemainingScores, Scores).
