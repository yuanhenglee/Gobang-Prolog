:- initialization main, halt.
:- dynamic white/2.
:- dynamic black/2.
:- dynamic cached_V/1.
:- dynamic cached_NV/1.
:- table relativeTo/2.
:- table adjacentTo/2.

% UTILITY

isBlack( black ).

read_predicates([H|T]) :-
    read_line_to_codes(user_input, H), 
    H \= end_of_file,
    string_codes(Input, H),
    term_string(Term, Input),
    assert(Term), 
    read_predicates(T).
read_predicates([]).

hitTimeLimit:-
    nb_getval( timeStart, TimeStart),
    statistics(runtime,[TimeStop|_]),
    T_ms is TimeStop - TimeStart, 
    false%T_ms > 4500
.

addMove( Player, [X, Y] ):-
    call( Player, X, Y);
    forall( 
        relativeTo( [X,Y],R ),
        (
            (cached_V( R ) -> retract(cached_V( R ));true),
            (cached_NV( R ) -> retract(cached_NV( R ));true)
        )
    ),
    atomic_list_concat([Player, "(", X, ",", Y, ")"], Fact),
    term_to_atom(Term, Fact),
    assert(Term)
.

undoMove( Player, [X, Y] ):-
    \+call( Player, X, Y);
    forall( 
        relativeTo( [X,Y],R ),
        (
            (cached_V( R ) -> retract(cached_V( R ));true),
            (cached_NV( R ) -> retract(cached_NV( R ));true)
        )
    ),
    atomic_list_concat([Player, "(", X, ",", Y, ")"], Fact),
    term_to_atom(Term, Fact),
    retract(Term)
.

writeMove( Player, [X, Y]):-
    write(Player), write("("), write(X), write(", "), write(Y), write(")\n")
.

determineSide( white ):-
    aggregate_all(count, white(_,_), W),
    aggregate_all(count, black(_,_), B),
    B>W
.
determineSide( black ):- \+determineSide( white ).

opponent( white, black ).
opponent( black, white ).

% BOARD PATTERN

direction( 1, 1 ).
direction( 1, -1 ).
direction( 1, 0 ).
direction( 0, 1 ).

isEmpty( [X, Y] ):- \+white( X, Y ), \+black( X, Y ) .

isValid( [X, Y] ):- X > 0, X <16, Y > 0, Y <16 .

adjacentTo( [X1,Y1], [X2,Y2] ):-
    (X2 is X1-1 ; X2 is X1 ; X2 is X1+1),
    (Y2 is Y1-1 ; Y2 is Y1 ; Y2 is Y1+1),
    (X2 \= X1; Y2 \= Y1),
    isValid( [X2, Y2] )
.

relativeTo( [X1, Y1], [X2, Y2] ):-
    between(-4,4,Offset),
    Offset \= 0,
    direction( OX, OY ),
    X2 is X1 + OX*Offset,
    Y2 is Y1 + OY*Offset,
    isValid( [X2, Y2] )
.

% 5 types of just4:
% 1
just4( Player, L , H):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    validForP( white, [X5, Y5]),
    %\+triggerLongLine( Player , X5, Y5 ),
    append( [], [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]], L ),
    append( [], [[X5,Y5]], H )
.
% 2
just4( Player, L , H):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    validForP( white, [X4, Y4]),
    %\+triggerLongLine( Player , X4, Y4 ),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    append( [], [[X1,Y1],[X2,Y2],[X3,Y3],[X5,Y5]], L ),
    append( [], [[X4,Y4]], H )
.
% 3
just4( Player, L , H):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    validForP( white, [X3, Y3]),
    %\+triggerLongLine( Player , X3, Y3 ),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    append( [], [[X1,Y1],[X2,Y2],[X4,Y4],[X5,Y5]], L ),
    append( [], [[X3,Y3]], H )
.
% 4
just4( Player, L , H):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    validForP( white, [X2, Y2]),
    %\+triggerLongLine( Player , X2, Y2 ),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    append( [], [[X1,Y1],[X3,Y3],[X4,Y4],[X5,Y5]], L ),
    append( [], [[X2,Y2]], H )
.
% 5
just4( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X2, Y2),
    X1 is X2-OX, Y1 is Y2-OY,
    validForP( white, [X1, Y1]),
    %\+triggerLongLine( Player , X1, Y1 ),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    append( [], [[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5]], L ),
    append( [], [[X1,Y1]], H )
.

%1
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    append( [], [[X1,Y1],[X2,Y2],[X3,Y3]], L ),

    HX2 is X1-OX, HY2 is Y1-OY, 
    HX1 is HX2-OX, HY1 is HY2-OY,
    HX3 is X3+OX, HY3 is Y3+OY, 
    HX4 is HX3+OX, HY4 is HY3+OY,

    (
        (validForP( white, [HX1, HY1]), validForP( white, [HX2, HY2]), append( [], [[HX1,HY1],[HX2,HY2]], H ) );
        (validForP( white, [HX2, HY2]), validForP( white, [HX3, HY3]), append( [], [[HX2,HY2],[HX3,HY3]], H ) );
        (validForP( white, [HX3, HY3]), validForP( white, [HX4, HY4]), append( [], [[HX3,HY3],[HX4,HY4]], H ) )
    )
.
% 2
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1 ),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    validForP( white, [X3, Y3]),
    X4 is X3+OX, Y4 is Y3+OY,
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5 ),
    ( 
        (validForP( white, [X4, Y4] ), call( Player, X2, Y2 ), append( [], [[X1,Y1],[X2,Y2],[X5,Y5]], L ), append( [], [[X4,Y4],[X3,Y3]], H ));
        (validForP( white, [X2, Y2] ), call( Player, X4, Y4 ), append( [], [[X1,Y1],[X5,Y5],[X4,Y4]], L ), append( [], [[X3,Y3],[X2,Y2]], H ))
    )
.
% 3
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1 ),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4 ),
    HX1 is X1-OX, HY1 is Y1-OY,
    HX2 is X4+OX, HY2 is Y4+OY,
    ( 
        (validForP( white, [HX1, HY1] ), call( Player, X2, Y2 ), validForP( white, [X3, Y3]), append( [], [[X1,Y1],[X2,Y2],[X4,Y4]], L ), append( [], [[HX1,HY1],[X3,Y3]], H ));
        (validForP( white, [HX1, HY1] ), call( Player, X3, Y3 ), validForP( white, [X2, Y2]), append( [], [[X1,Y1],[X3,Y3],[X4,Y4]], L ), append( [], [[HX1,HY1],[X2,Y2]], H ));
        (validForP( white, [HX2, HY2] ), call( Player, X2, Y2 ), validForP( white, [X3, Y3]), append( [], [[X1,Y1],[X2,Y2],[X4,Y4]], L ), append( [], [[X3,Y3],[HX2,HY2]], H ));
        (validForP( white, [HX2, HY2] ), call( Player, X3, Y3 ), validForP( white, [X2, Y2]), append( [], [[X1,Y1],[X3,Y3],[X4,Y4]], L ), append( [], [[X2,Y2],[HX2,HY2]], H ))
    )
.
% 4
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),

    validForP( white, [X2, Y2]),
    validForP( white, [X4, Y4]),

    append( [], [[X1,Y1],[X3,Y3],[X5,Y5]], L ),
    append( [], [[X2,Y2],[X4,Y4]], H )
.

% not all 2, just those can be a 3 in 1 step
just2( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    HX2 is X1-OX, HY2 is Y1-OY,
    HX1 is HX2-OX, HY1 is HY2-OY,
    HX3 is X2+OX, HY3 is Y2+OY,
    HX4 is HX3+OX, HY4 is HY3+OY,
    (
        ( validForP( white, [HX1, HY1]), validForP( white, [HX2, HY2]), append( [], [[HX1,HY1],[HX2,HY2]], H ) ); 
        ( validForP( white, [HX2, HY2]), validForP( white, [HX3, HY3]), append( [], [[HX2,HY2],[HX3,HY3]], H ) );
        ( validForP( white, [HX3, HY3]), validForP( white, [HX4, HY4]), append( [], [[HX3,HY3],[HX4,HY4]], H ) )
    ),

    append( [], [[X1,Y1],[X2,Y2]], L )
.
just2( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    HX1 is X1-OX, HY1 is Y1-OY,
    HX3 is X3+OX, HY3 is Y3+OY,
    (
        ( validForP( white, [HX1, HY1]), validForP( white, [X2, Y2]), append( [], [[HX1,HY1],[X2,Y2]], H ) ); 
        ( validForP( white, [X2, Y2]), validForP( white, [HX3, HY3]), append( [], [[X2,Y2],[HX3,HY3]], H ) )
    ),

    append( [], [[X1,Y1],[X3,Y3]], L )
.

jump3( Player, L, H ):-
    direction( OX, OY ),
    call( Player, X1, Y1 ),
    X2 is X1+OX, Y2 is Y1+OY,
    X3 is X2+OX, Y3 is Y2+OY,
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4 ),
    HX1 is X1-OX, HY1 is Y1-OY,
    validForP( white, [HX1, HY1] ),
    HX2 is X4+OX, HY2 is Y4+OY,
    validForP( white, [HX2, HY2] ),
    ( 
        (call( Player, X2, Y2 ), validForP( white, [X3, Y3]), append( [], [[X1,Y1],[X2,Y2],[X4,Y4]], L ), append( [], [[X3,Y3]], H ));
        (call( Player, X3, Y3 ), validForP( white, [X2, Y2]), append( [], [[X1,Y1],[X3,Y3],[X4,Y4]], L ), append( [], [[X2,Y2]], H ))
    )
.
straight3( Player, L, H ):-
    direction( OX, OY ),
    call( Player, X1, Y1 ),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2 ),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3 ),
    HX1 is X1-OX, HY1 is Y1-OY,
    validForP( white, [HX1, HY1] ),
    HX2 is X3+OX, HY2 is Y3+OY,
    validForP( white, [HX2, HY2] ), 
    HX3 is HX1-OX, HY3 is HY1-OY ,
    HX4 is HX2+OX, HY4 is HY2+OY ,
    (　
        (validForP( white, [HX3, HY3]),append( [], [[HX1,HY1]], H ) );
        (validForP( white, [HX4, HY4]),append( [], [[HX2,HY2]], H ) )
    ),
    append( [], [[X1,Y1],[X2,Y2],[X3,Y3]], L )
.

%// TODO all 3 H needs to do validForP
live3( Player, L, H ):- straight3( Player, L, H ) .
live3( Player, L, H ):- jump3( Player, L, H ) .

perfect5( Player, L ):-
    direction( OX, OY ),

    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),

    HX1 is X1-OX, HY1 is Y1-OY,
    \+call( Player, HX1, HY1),
    HX2 is X5+OX, HY2 is Y5+OY,
    \+call( Player, HX2, HY2),

    append( [], [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5]], L )
.

longLine( Player, L ):-
    direction( OX, OY ),

    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5),
    X6 is X5+OX, Y6 is Y5+OY,
    call( Player, X6, Y6),

    append( [], [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5],[X6,Y6]], L )
.

double4live4( Player, L1, L2 ):-
    just4( Player, L1, [H1]), just4(Player, L2,[H2]),
    H1 \= H2%, 
    %validForP( Player, H1 ), validForP( Player, H2 )
.
doubleLive3( Player, L1, L2 ):-
    live3( Player, L1, [_H1]), live3(Player, L2, [_H2]), 
    L1 \= L2%, 
    %validForP( Player, H1 ), validForP( Player, H2 )
.
live3Dead4( Player, L1, L2 ):-
    live3( Player, L1, [H1]), just4(Player, L2,[H2]),
    validForP( Player, H1 ), validForP( Player, H2 )
.
tryPotentailDouble4( white, Point ):-
    addMove( white, Point ),
    triggerDouble4( white, Point )
->  undoMove( white, Point )
;   undoMove( white, Point ), false
.
tryPotentailDouble3( white, Point ):-
    addMove( white, Point ),
    triggerDoubleLive3( white, Point )
->  undoMove( white, Point )
;   undoMove( white, Point ), false
.
tryPotentail34( Player, Point ):-
    addMove( Player, Point ),
    trigger34( Player, Point )
->  undoMove( Player, Point )
;   undoMove( Player, Point ), false
.



triggerDouble4( Player, Point ):-
    double4live4( Player, L1, L2 ),
    L1 \= L2,
    member(Point, L1), member(Point, L2)
.
triggerDoubleLive3( Player, Point ):-
    doubleLive3( Player, L1, L2 ),
    member(Point,L1), member(Point,L2)
.
triggerLongLine( Player, Point ):-
    ( longLine( Player, L1), member(Point, L1) )
.
triggerPerfect5( Player, Point ):-
    ( perfect5( Player, L1), member(Point, L1) )
.
trigger34( Player, Point ):-
    live3Dead4( Player, L1, L2 ),
    member(Point,L1), member(Point,L2),
    % prevent duplicate pattern
    member( P2, L1 ), P2\=Point, \+member( P2, L2 )
.

% BANNED MOVE

validForP( white, Point ):- isEmpty( Point ), isValid(Point) .
validForP( black, Point ):- validForP( white, Point ), cached_V(Point), \+cached_NV(Point).
validForP( black, Point ):-
    validForP( white, Point ),
    ( (cached_NV(Point), \+cached_V(Point) ) -> false;true),
    (
        (
            addMove( black, Point),
            (
                triggerPerfect5( black, Point );
                (
                    \+triggerDoubleLive3( black, Point ),
                    \+triggerDouble4( black, Point ),
                    \+triggerLongLine( black, Point )
                )
            )
        )
    ->  undoMove( black, Point), assert( cached_V(Point) )
    ;   undoMove( black, Point), assert( cached_NV(Point) ),false
    )
.

just5( Player ):- 
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),
    X5 is X4+OX, Y5 is Y4+OY,
    call( Player, X5, Y5)
.

% FIND BEST MOVE

% first step
bestMove( black, [X, Y]):- \+black( _, _ ), \+white( _, _ ), X is 8, Y is 8 .
% second step 斜指
bestMove( white, [X, Y] ):- 
    black( XB, YB ), \+white( _, _ ), 
    (
        (XB > 8, YB > 8 , X is XB-1, Y is YB-1 ); 
        (XB =< 8, YB  =< 8 , X is XB+1, Y is YB+1 ); 
        (XB > 8, YB =< 8 , X is XB-1, Y is YB+1 ); 
        (XB =< 8, YB  > 8 , X is XB+1, Y is YB-1 )
    )
.
% third step
bestMove( black, [X,Y] ):-
    aggregate_all(count, (XB,YB), black( XB,YB ), 1),
    aggregate_all(count, (XW,YW), white( XW,YW ), 1),
    black( XB, YB ), white( XW, YW ), 
    (
        % 斜指->蒲月
        ( YB > 1, XW is XB-1, YW is YB-1, X is XB+1, Y is YB-1 ); 
        ( YB > 1, XW is XB+1, YW is YB+1, X is XB+1, Y is YB-1 ); 
        ( XB < 15 , XW is XB-1, YW is YB+1, X is XB+1, Y is YB+1 ); 
        ( YB < 15 , XW is XB+1, YW is YB-1, X is XB+1, Y is YB+1 );
        % 直指->花月
        ( YB > 1, XW is XB-1, YW is YB, X is XB-1, Y is YB-1 ); 
        ( YB > 1, XW is XB+1, YW is YB, X is XB+1, Y is YB-1 ); 
        ( XB < 15 , XW is XB, YW is YB+1, X is XB+1, Y is YB+1 ); 
        ( XB < 15 , XW is XB, YW is YB-1, X is XB+1, Y is YB-1 )

    )
.

% general minmax
bestMove( Player, P ):-
    selectPossiblePos( Player, PossiblePositions ),
    %write( PossiblePositions ),
    % ! SET DEPTH !
    chooseMax( PossiblePositions, 4, -99999999, 99999999, Player, nil, (P,_Value)) %,write(Value)
.


evalScore( Player, Score ):-
    opponent( Player, OtherP ),
    (
        % already win 
        ( just5( Player ), Score is 95000000 );
        % already lose 
        ( just4( OtherP,_,[P1]),validForP( OtherP, P1 ), Score is -95000000 );
        ( double4live4( Player,_ ,_ ),  Score is 94000000 );
        ( 
            \+just4( Player, _, _),
            (
                live3( OtherP, _, _);
                ( isBlack(Player), just3( OtherP, _, [H1]), tryPotentailDouble4( OtherP, H1 ));
                ( isBlack(Player), just2( OtherP, _, [H1]), tryPotentailDouble3( OtherP, H1 ))
            ),
            Score is -94000000
        )
    ),
    false
.
evalScore( Player, Score ):-
    opponent( Player, OtherP ),
    % just 4
    (just4( Player, _, _ )-> J4P = 1; J4P = 0),
    % just 3
    aggregate_all(count, L, live3( OtherP,L,_), L3O),
    aggregate_all(count, L, live3( Player,L,_), L3P),
    aggregate_all(count, L, just3( OtherP,L,_), J3O),
    aggregate_all(count, L, just3( Player,L,_), J3P),
    % just 2
    aggregate_all(count, L, just2( OtherP,L,_), J2O),
    aggregate_all(count, L, just2( Player,L,_), J2P),
    Score is 1024*J4P - 1024*L3O + 512*L3P - 512*J3O + 256*J3P - 256*J2O + 128*J2P
.

% prevent selectPossiblePos from selecting Pos all being banned 
fixForBlack( white, _PossiblePositions ).
fixForBlack( black, PossiblePositions ):-
    member( P , PossiblePositions ),
    validForP( black, P )
.

selectPossiblePos( Player, PossiblePositions ):-
    opponent( Player, OtherP ),
    findall( H4P, (just4( Player, _, [H4P] ) ), H4Ps),
    findall( H4O, (just4( OtherP, _, [H4O] ) ), H4Os),
    append( H4Ps, H4Os, L4 ),

    findall( LH3P, (live3( Player, _, [LH3P]) ), LH3Ps),
    findall( LH3O, (live3( OtherP, _, [LH3O]) ), LH3Os),
    append( LH3Ps, LH3Os, LL3 ), 

    findall( H3P, (just3( Player, _, Hs3P), member( H3P, Hs3P )), H3Ps),
    findall( H3O, (just3( OtherP, _, Hs3O), member( H3O, Hs3O )), H3Os),
    append( H3Ps, H3Os, L3 ),

    findall( H2P, (just2( Player, _, Hs2P), member( H2P, Hs2P )), H2Ps),
    findall( H2O, (just2( OtherP, _, Hs2O), member( H2O, Hs2O )), H2Os),
    append( H2Ps, H2Os, L2 ),

    findall( PH34P, (member( PH34P, H3Ps), tryPotentail34( Player, PH34P)), PH34Ps),
    findall( PH34O, (member( PH34O, H3Os), tryPotentail34( OtherP, PH34O)), PH34Os),
    append( PH34Ps, PH34Os, PHL34 ), 

    findall( PH3P, (member( PH3P, H3Ps), tryPotentailDouble4( Player, PH3P)), PH3Ps),
    findall( PH3O, (member( PH3O, H3Os), tryPotentailDouble4( OtherP, PH3O)), PH3Os),
    append( PH3Ps, PH3Os, PHL3 ), 

    findall( PH2P, (member( PH2P, H2Ps ), tryPotentailDouble3( Player, PH2P)), PH2Ps),
    findall( PH2O, (member( PH2O, H2Os ), tryPotentailDouble3( OtherP, PH2O)), PH2Os),
    append( PH2Ps, PH2Os, PHL2 ), 
    
    append( L4, LL3, L4LL3 ),
    append( L4LL3, PHL3, L4LL3PHL3 ),
    append( L4LL3PHL3, PHL34, L4LL3PHL34 ),
    append( L4LL3PHL34, PHL2, CollectionL2 ),
    (
        ( CollectionL2 \= [], fixForBlack( Player, CollectionL2), list_to_set(CollectionL2, PossiblePositions ) );
        ( L3 \= [], fixForBlack( Player, L3), list_to_set(L3, PossiblePositions ) );
        ( findall( P, ( ( white(X,Y);black(X,Y) ),adjacentTo( [X,Y] , P ), validForP( white, P )), Adja), append(L2,Adja,CollectionL4), list_to_set(CollectionL4,PossiblePositions))
    )
.

cutoff( _T, _Depth, _Alpha, Beta, _Player, _Record, (H, Value), (H, Value) ):- ( hitTimeLimit; Value >= Beta ),!  .
cutoff( T, Depth, Alpha, Beta, Player, _Record, BestMove, (H, Value) ):- 
    Value > Alpha, Value < Beta, !,
    chooseMax( T, Depth, Value, Beta, Player, H , BestMove )
.
cutoff( T, Depth, Alpha, Beta, Player, Record, BestMove, (_H, Value) ):- 
    Value =< Alpha, !,
    chooseMax( T, Depth, Alpha, Beta, Player, Record, BestMove )
.


chooseMax( [], _Depth, Alpha, _Beta, _Player, Move, (Move, Alpha) ).
chooseMax( [H|T], Depth, Alpha, Beta, Player, Record, BestMove ):-
    minmax( H, Depth, Alpha, Beta, Player, PossibleScore ),
    %( (Depth>=5 ) ->(write(Depth),writeMove(Player,H),write(":"),write(PossibleScore),nl);true),
    cutoff( T, Depth, Alpha, Beta, Player, Record, BestMove, (H, PossibleScore) )
.

minmax( Position, Depth, Alpha, Beta, Player, ReturnS ):-
    (\+validForP( Player, Position ), ReturnS = -99999999);
    addMove( Player, Position ),
    (
        evalScore( Player, Score ),
        ( Depth = 1 ; Score >= 90000000 ; hitTimeLimit) ,
        undoMove( Player, Position ), ReturnS is Score
    );
    NewDepth is Depth - 1,
    Alpha1 is -Beta, % max/min
    Beta1 is -Alpha,
    opponent( Player, OtherP ),
    selectPossiblePos( OtherP, PossiblePositions ),
    chooseMax( PossiblePositions, NewDepth, Alpha1, Beta1, OtherP, nil, (_Move, Value)),
    undoMove( Player, Position ),
    ReturnS is -Value
.



main :-
    statistics(runtime,[TimeStart|_]),
    nb_setval( timeStart, TimeStart ),
    read_predicates(_),
    determineSide( Player ),
    time(bestMove( Player, P ) ),
    writeMove( Player, P )
.