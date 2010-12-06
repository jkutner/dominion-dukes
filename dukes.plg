%-------- Scoring a deck

% Now that we have gone through all the cards, we can total up the Dukes
score([], DukesInDeck, DuchiesInDeck, Score) :-
    Score is DukesInDeck*DuchiesInDeck.

score([duchy|Rest], DukesInDeck, DuchiesInDeck, Score) :-
    Y is DuchiesInDeck+1,
    score(Rest, DukesInDeck, Y, X),
    Score is 3+X.

score([duke|Rest], DukesInDeck, DuchiesInDeck, Score) :-
    Y is DukesInDeck+1,
    score(Rest, Y, DuchiesInDeck, Score).
    
score(Deck, Score) :-
  score(Deck, 0, 0, Score).

%--------- Building up all possible decks

play(0, _, Deck, Deck) :- !.

play(_, 0, Deck, Deck) :- !.

% take a duchy
play(DukesRemaining, DuchiesRemaining, Deck, FinishedDeck) :-
    X is DuchiesRemaining-1,
    append([duchy], Deck, NewDeck),
    play(DukesRemaining, X, NewDeck, FinishedDeck).

% take a duke
play(DukesRemaining, DuchiesRemaining, Deck, FinishedDeck) :-
    X is DukesRemaining-1,
    append([duke], Deck, NewDeck),
    play(X, DuchiesRemaining, NewDeck, FinishedDeck).

play(DukesRemaining, DuchiesRemaining, Deck) :-
    play(DukesRemaining, DuchiesRemaining, [], Deck).

%---------- Main entry for program

best_play(DukesRemaining, DuchiesRemaining) :-
    findall(Z, play(DukesRemaining, DuchiesRemaining, Z), SetOfDecks),
    %write(SetOfDecks),nl,
    max_deck(SetOfDecks, BestDeck),
    reverse(BestDeck, PrintableBestDeck),
    write('Best Deck: '),write(PrintableBestDeck),nl,
    score_by_round(BestDeck, Scores),
    reverse(Scores, PrintableScores),
    write('Score by Round: '),write(PrintableScores),nl.
    
%---------- The algorithm to find the best ordered deck out of all decks

max_deck(SetOfDecks, Set) :-
    max_deck(SetOfDecks, 1, Set).
    
max_deck([Set|[]], _, Set) :- !.

max_deck([FirstSet|_], N, FirstSet) :-
    length(FirstSet, L),
    N > L.
        
max_deck(SetOfDecks, N, Set) :-
    max_n(SetOfDecks, N, Max), 
    write(N),write(' -> '),write(Max),
    decks_with_n_of(SetOfDecks, N, Max, SetOfDecksWithMaxAtN),
    write(', '), length(SetOfDecksWithMaxAtN, L), write(L),nl,
    M is N+1,
    max_deck(SetOfDecksWithMaxAtN, M, Set).

%---------- Determine the highest scoring deck at a giving round (n) for the given decks

max_n([Deck|[]], N, Max) :-
    nth_score(N, Deck, Max),
    !.

max_n([Deck|RemainingDecks], N, Max) :-
    max_n(RemainingDecks, N, M),
    nth_score(N, Deck, X),
    X =< M, 
    Max is M, 
    !.
    
max_n([Deck|_], N, Max) :-
    nth_score(N, Deck, Max). % if we got this far, then this is the new max
    
%----------- Find all the decks that have a score of Max at round N

decks_with_n_of([], _, _, []) :- !.
    
decks_with_n_of([A|SetOfDecks], N, Max, SetOfDecksWithMaxAtN) :-
    \+ nth_score(N, A, Max),
    decks_with_n_of(SetOfDecks, N, Max, SetOfDecksWithMaxAtN).
    
decks_with_n_of([A|SetOfDecks], N, Max, SetOfDecksWithMaxAtN) :-
    nth_score(N, A, Max),
    decks_with_n_of(SetOfDecks, N, Max, R),
    append([A], R, SetOfDecksWithMaxAtN).
    
%----------- Score a deck at a certain round

nth_score(N, Set, Score) :-
    tail(Set, N, Tail),
    score(Tail, Score).
    
%----------- Returns all N elements of a Set after and including the Nth element

tail(In, N, Out) :-
    tail(In, N, 1, Out).

tail(Set, N, M, Set) :-
    M = N, !.

tail([_|R], N, M, Tail) :- 
    Mpp is M+1,
    tail(R, N, Mpp, Tail).

%------------ Recursively score each round of a given deck
    
score_by_round(Deck, Scores) :-
    score(Deck, Score),
    score_by_round_recursive(Deck, RemainingScores),
    append([Score], RemainingScores, Scores).

score_by_round_recursive([_|[]], []) :- !.

score_by_round_recursive([_|Deck], Scores) :-
    score(Deck, Score),
    score_by_round_recursive(Deck, RemainingScores),
    append([Score], RemainingScores, Scores).
    
