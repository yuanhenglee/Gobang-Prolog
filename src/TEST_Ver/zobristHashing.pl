:- initialization main, halt.
:- dynamic white/2.
:- dynamic black/2.
:- table relativeTo/2.
:- table adjacentTo/2.
:- table getKey/3.
%:- table evalScore/3.
:- dynamic cached_V/1.
:- dynamic cached_NV/1.

% UTILITY

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
    T_ms is TimeStop - TimeStart, false,
    T_ms > 4800
    %write(T_ms)
.

xor_aggr( [H], H ).
xor_aggr( [H|T], Val ):-
    xor_aggr(T, Val1 ),
    Val is H xor Val1
.

getKey( Player, [X,Y], Key ):- Key is random(18446744073709551616) .

setBoard:-
    findall( Key, (white(X,Y),getKey( white, [X,Y], Key )),  Ws ),
    findall( Key, (black(X,Y),getKey( black, [X,Y], Key )),  Bs ),
    append( Ws,Bs, AllKeys ),
    xor_aggr( AllKeys, Board ),
    nb_setval( board, Board)
.

updateBoard( Player, P ):-
    getKey( Player, P, Key ),
    nb_getval( board, Cur ),
    New is Key xor Cur,
    nb_setval( board, New )
.

% BOARD PATTERN

isEmpty( [X, Y] ):-
    \+white( X, Y ),
    \+black( X, Y )
.

isValid( [X, Y] ):-
    X > 0, X <16,
    Y > 0, Y <16
.

adjacentTo( [X1,Y1], [X2,Y2] ):-
    (X2 is X1-1 ; X2 is X1 ; X2 is X1+1),
    (Y2 is Y1-1 ; Y2 is Y1 ; Y2 is Y1+1),
    (X2 \= X1; Y2 \= Y1),
    isValid( [X2, Y2] )
.

relativeTo( [X1, Y1], [X2, Y2] ):-
    between(-4,4,Offset),
    Offset \= 0,
    X2 is X1 + Offset,
    Y2 is Y1,
    isValid( [X2, Y2] )
.
relativeTo( [X1, Y1], [X2, Y2] ):-
    between(-4,4,Offset),
    Offset \= 0,
    X2 is X1,
    Y2 is Y1 + Offset,
    isValid( [X2, Y2] )
.
relativeTo( [X1, Y1], [X2, Y2] ):-
    between(-4,4,Offset),
    Offset \= 0,
    X2 is X1 + Offset,
    Y2 is Y1 + Offset,
    isValid( [X2, Y2] )
.
relativeTo( [X1, Y1], [X2, Y2] ):-
    between(-4,4,Offset),
    Offset \= 0,
    X2 is X1 + Offset,
    Y2 is Y1 - Offset,
    isValid( [X2, Y2] )
.

addMove( Player, [X, Y] ):-
    call( Player, X, Y);
    updateBoard( Player, [X,Y] ),
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
    updateBoard( Player, [X,Y] ),
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

writeMoves( Player, [[X,Y]|T] ):-
    writeMove(Player, X, Y),
    writeMoves( Player, T )
.
writeMoves( _, []).


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
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]], L ),
    list_to_set( [[X5,Y5]], H )
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
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X5,Y5]], L ),
    list_to_set( [[X4,Y4]], H )
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
    list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4],[X5,Y5]], L ),
    list_to_set( [[X3,Y3]], H )
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
    list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4],[X5,Y5]], L ),
    list_to_set( [[X2,Y2]], H )
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
    list_to_set( [[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5]], L ),
    list_to_set( [[X1,Y1]], H )
.

%1
just3( Player, L , H ):-
    direction( OX, OY ),
    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3]], L ),

    HX2 is X1-OX, HY2 is Y1-OY, 
    HX1 is HX2-OX, HY1 is HY2-OY,
    HX3 is X3+OX, HY3 is Y3+OY, 
    HX4 is HX3+OX, HY4 is HY3+OY,

    (
        (validForP( white, [HX1, HY1]), validForP( white, [HX2, HY2]), list_to_set( [[HX1,HY1],[HX2,HY2]], H ) );
        (validForP( white, [HX2, HY2]), validForP( white, [HX3, HY3]), list_to_set( [[HX2,HY2],[HX3,HY3]], H ) );
        (validForP( white, [HX3, HY3]), validForP( white, [HX4, HY4]), list_to_set( [[HX3,HY3],[HX4,HY4]], H ) )
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
        (validForP( white, [X4, Y4] ), call( Player, X2, Y2 ), list_to_set( [[X1,Y1],[X2,Y2],[X5,Y5]], L ), list_to_set( [[X4,Y4],[X3,Y3]], H ));
        (validForP( white, [X2, Y2] ), call( Player, X4, Y4 ), list_to_set( [[X1,Y1],[X5,Y5],[X4,Y4]], L ), list_to_set( [[X3,Y3],[X2,Y2]], H ))
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
        (validForP( white, [HX1, HY1] ), call( Player, X2, Y2 ), validForP( white, [X3, Y3]), list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4]], L ), list_to_set( [[HX1,HY1],[X3,Y3]], H ));
        (validForP( white, [HX1, HY1] ), call( Player, X3, Y3 ), validForP( white, [X2, Y2]), list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4]], L ), list_to_set( [[HX1,HY1],[X2,Y2]], H ));
        (validForP( white, [HX2, HY2] ), call( Player, X2, Y2 ), validForP( white, [X3, Y3]), list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4]], L ), list_to_set( [[X3,Y3],[HX2,HY2]], H ));
        (validForP( white, [HX2, HY2] ), call( Player, X3, Y3 ), validForP( white, [X2, Y2]), list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4]], L ), list_to_set( [[X2,Y2],[HX2,HY2]], H ))
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

    list_to_set( [[X1,Y1],[X3,Y3],[X5,Y5]], L ),
    list_to_set( [[X2,Y2],[X4,Y4]], H )
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
        ( validForP( white, [HX1, HY1]), validForP( white, [HX2, HY2]), list_to_set( [[HX1,HY1],[HX2,HY2]], H ) ); 
        ( validForP( white, [HX2, HY2]), validForP( white, [HX3, HY3]), list_to_set( [[HX2,HY2],[HX3,HY3]], H ) );
        ( validForP( white, [HX3, HY3]), validForP( white, [HX4, HY4]), list_to_set( [[HX3,HY3],[HX4,HY4]], H ) )
    ),

    list_to_set( [[X1,Y1],[X2,Y2]], L )
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
        ( validForP( white, [HX1, HY1]), validForP( white, [X2, Y2]), list_to_set( [[HX1,HY1],[X2,Y2]], H ) ); 
        ( validForP( white, [X2, Y2]), validForP( white, [HX3, HY3]), list_to_set( [[X2,Y2],[HX3,HY3]], H ) )
    ),

    list_to_set( [[X1,Y1],[X3,Y3]], L )
.

direction( 1, 1 ).
direction( 1, -1 ).
direction( 1, 0 ).
direction( 0, 1 ).

live4( Player, L, H ):-
    direction( OX, OY ),

    call( Player, X1, Y1),
    X2 is X1+OX, Y2 is Y1+OY,
    call( Player, X2, Y2),
    X3 is X2+OX, Y3 is Y2+OY,
    call( Player, X3, Y3),
    X4 is X3+OX, Y4 is Y3+OY,
    call( Player, X4, Y4),

    HX1 is X1-OX, HY1 is Y1-OY,
    validForP( Player , [HX1, HY1] ),
    HX2 is X4+OX, HY2 is Y4+OY,
    validForP( Player , [HX2, HY2] ),
    
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]], L ),
    list_to_set( [[HX1,HY1],[HX2,HY2]], H )
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
        (call( Player, X2, Y2 ), validForP( white, [X3, Y3]), list_to_set( [[X1,Y1],[X2,Y2],[X4,Y4]], L ), list_to_set( [[X3,Y3]], H ));
        (call( Player, X3, Y3 ), validForP( white, [X2, Y2]), list_to_set( [[X1,Y1],[X3,Y3],[X4,Y4]], L ), list_to_set( [[X2,Y2]], H ))
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
        (validForP( white, [HX3, HY3]),list_to_set( [[HX1,HY1]], H ) );
        (validForP( white, [HX4, HY4]),list_to_set( [[HX2,HY2]], H ) )
    ),
    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3]], L )
.

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

    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5]], L )
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

    list_to_set( [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4],[X5,Y5],[X6,Y6]], L )
.

% black only
doubleLive4( Player, Point ):-
    addMove( Player, Point ),
    ( 
        just4( Player, L1, [H1]), just4(Player, L2,[H2]),
        L1 \= L2, member(Point, L1), member(Point, L2),
        validForP( Player, H1 ), validForP( Player, H2 )
    )
->  undoMove( Player, Point )
;   undoMove( Player, Point ), false
.
doubleLive3( Player, Point ):-
    addMove( Player, Point),
    ( live3( Player, L1, _), live3(Player, L2, _), L1 \= L2 , member(Point,L1), member(Point,L2) )
->  undoMove( Player, Point)
;   undoMove( Player, Point), false
.
triggerLongLine( Player, Point ):-
    addMove( Player, Point),
    ( longLine( Player, L1), member(Point, L1) )
->  undoMove( Player, Point)
;   undoMove( Player, Point), false
.
triggerPerfect5( Player, Point ):-
    addMove( Player, Point),
    ( perfect5( Player, L1), member(Point, L1) )
->  undoMove( Player, Point)
;   undoMove( Player, Point), false
.


validForP( white, Point ):- isEmpty( Point ), isValid(Point) .
validForP( black, Point ):- cached_V(Point), \+cached_NV(Point).
validForP( black, Point ):-
    ( (cached_NV(Point), \+cached_V(Point) ) -> false;true),
    (
        (
            isEmpty( Point ), isValid(Point),
            (
                triggerPerfect5( black, Point );
                (
                    \+doubleLive3( black, Point ),
                    \+doubleLive4( black, Point ),
                    \+triggerLongLine( black, Point )
                )
            )
        )
    ->  assert( cached_V(Point) )
    ;   assert( cached_NV(Point) ),false
    )
.

win( Player ):- 
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

determineSide( white ):-
    aggregate_all(count, white(_,_), W),
    aggregate_all(count, black(_,_), B),
    B>W
.
determineSide( black ):- \+determineSide( white ).

opponent( white, black ).
opponent( black, white ).

% first step
bestMove( black, [X, Y]):- \+black( _, _ ), \+white( _, _ ), X is 8, Y is 8 .
% second step
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
        (XB > 8, YB > 8 , XW is XB-1, YW is YB-1, X is XB+1, Y is YB-1 ); 
        (XB =< 8, YB  =< 8 , XW is XB+1, YW is YB+1, X is XB+1, Y is YB-1 ); 
        (XB > 8, YB =< 8 , XW is XB-1, YW is YB+1, X is XB+1, Y is YB+1 ); 
        (XB =< 8, YB  > 8 , XW is XB+1, YW is YB-1, X is XB+1, Y is YB+1 )
    )
.

% general minmax
bestMove( Player, P ):-
    selectPossiblePos( Player, PossiblePositions ),
    %write( PossiblePositions ),
    % ! SET DEPTH !
    chooseMax( PossiblePositions, 3, -99999999, 99999999, Player, nil, (P,_Value))% ,write(Value)
.

% already lose    
evalScore( Player, _Board, -99999995 ):-
    opponent( Player, OtherP ),
    just4( OtherP,_,[P1]),validForP( OtherP, P1 )
.
% already win
evalScore( Player, _Board, 99999995 ):- win( Player ).
evalScore( Player, _Board, 99999995 ):- 
    just4( Player,_,[P1]),validForP( Player, P1 ), 
    just4( Player,_,[P2]), validForP( Player, P2 ), P1\=P2
.

evalScore( Player, _Board, Score ):-
    opponent( Player, OtherP ),
    % just 4
    aggregate_all(count, L, just4( Player,L,_), J4P),
    % just 3
    aggregate_all(count, L, live3( Player,L,_), L3P),
    aggregate_all(count, L, just3( Player,L,_), J3P),
    aggregate_all(count, L, live3( OtherP,L,_), L3O),
    aggregate_all(count, L, just3( OtherP,L,_), J3O),
    % just 2
    aggregate_all(count, L, just2( Player,L,_), J2P),
    aggregate_all(count, L, just2( OtherP,L,_), J2O),
    Score is 1000*J4P + 1000*L3P + 100*J3P + 10*J2P - 10000*L3O - 1000*J3O - 100*J2O
.

selectPossiblePos( Player, PossiblePositions ):-
    opponent( Player, OtherP ),
    findall( H4P, (just4( Player, _, [H4P] ), validForP( Player, H4P) ), H4Ps),
    findall( H4O, (just4( OtherP, _, [H4O] ), validForP( Player, H4O) ), H4Os),
    append( H4Ps, H4Os, L4 ), 

    findall( H3P, (just3( Player, _, Hs3P), member( H3P, Hs3P ), validForP( Player, H3P )), H3Ps),
    findall( H3O, (just3( OtherP, _, Hs3O), member( H3O, Hs3O ), validForP( Player, H3O )), H3Os),
    append( H3Ps, H3Os, L3 ), 
    (
        ( L4 \= []; L3 \= [])
    ->  ( append( L4, L3, L ), list_to_set( L, PossiblePositions ) )
    ;   (
            findall( H2P, (just2( Player, _, Hs2P), member( H2P, Hs2P ), validForP( Player, H2P )), H2Ps),
            findall( H2O, (just2( OtherP, _, Hs2O), member( H2O, Hs2O ), validForP( Player, H2O )), H2Os),
            (H2Os \= []; H2Ps \= [])
        ->  ( append( H2Ps, H2Os, L2 ), list_to_set( L2, PossiblePositions ) )
        ;   findall( P, ( ( white(X,Y);black(X,Y) ),adjacentTo( [X,Y] , P ), validForP( Player, P )), PossiblePositions)
        )
    )
.

cutoff( _T, _Depth, _Alpha, Beta, _Player, _Record, (H, Value), (H, Value) ):- ( hitTimeLimit; Value > 99999990; Value >= Beta ),!  .
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
    %( (Depth>=0, white(8,6),black(6,9) ) ->(write(Depth),writeMove(Player,H),write(":"),write(PossibleScore),nl);true),
    cutoff( T, Depth, Alpha, Beta, Player, Record, BestMove, (H, PossibleScore) )
.

minmax( Position, Depth, Alpha, Beta, Player, ReturnS ):-
    addMove( Player, Position ),
    (
        nb_getval( board, Board ),
        evalScore( Player, Board, Score ),
        ( Depth = 0 ; Score > 99999990 ; hitTimeLimit) ,
        undoMove( Player, Position ), ReturnS is Score
    );
    NewDepth is Depth - 1,
    Alpha1 is -Beta, % max/min
    Beta1 is -Alpha,
    opponent( Player, OtherP ),
    selectPossiblePos( OtherP, PossiblePositions ),
    %length( PossiblePositions, Length ),write(Depth),write(Position),write(":"),writeln(Length),
    chooseMax( PossiblePositions, NewDepth, Alpha1, Beta1, OtherP, nil, (_Move, Value)),
    undoMove( Player, Position ),
    ReturnS is -Value
.



main :-
    statistics(runtime,[TimeStart|_]),
    nb_setval( timeStart, TimeStart ),
    read_predicates(_),
    setBoard,
    determineSide( Player ),
    %opponent( Player, OtherP ),
    time(bestMove( Player, P )),
    writeMove( Player, P )
.