\ **************************************
\  RANDOM MAZE - ZUMYX EDITION (C) 2023 
\ **************************************

\ Includes

include unicode.5th
include colors.5th

\ Maze dimensions
VARIABLE W		\ Width
VARIABLE H		\ Height

23 W !
23 H !

\ Maze
CREATE MAZE W @ H @ * ALLOT


: @M ( w h -- a ) W @ * + MAZE + ;

: M! ( c w h -- ) @M C! ;
: M@ ( w h -- c ) @M C@ ;

\ Initialize the maze
MAZE W @ H @ * BL FILL

: WALL [CHAR] # ;
: FOX  [CHAR] F ;

HEX
: MEMIT
  DUP WALL = IF GREEN' 2592 UEMIT 2592 UEMIT NORM DROP EXIT THEN
  DUP FOX = IF RED' 1F98A UEMIT 32 UEMIT NORM DROP EXIT THEN
  WHITE [CHAR] . UEMIT 20 UEMIT DROP ;
DECIMAL

: .M
  CR H @ 0 DO W @ 0 DO I J M@ MEMIT LOOP CR LOOP ;

\ Random number generator

VARIABLE (SEED)
2463534242 (SEED) !

: RANDOM ( -- r )
  (SEED) @ DUP 13 LSHIFT XOR
  DUP 17 LSHIFT XOR
  DUP DUP 5 LSHIFT XOR (SEED) ! ;
: CHOOSE ( max -- r ) RANDOM UM* NIP ;

\ Maze generation

VARIABLE DW 0 DW !	\ Delta W
VARIABLE DH 0 DH !	\ Delta H
VARIABLE LEN 0 LEN !    \ Length of wall to draw

VARIABLE CW 0 CW !	\ Current W
VARIABLE CH 0 CH !	\ Current H

CREATE ROWS H @ ALLOT ROWS H @ 0 FILL	\ Selected rows
CREATE COLS W @ ALLOT COLS W @ 0 FILL   \ Selected columns
VARIABLE NROWS H @ 2/ 1- NROWS ! 		\ Number of rows left
VARIABLE NCOLS W @ 2/ 1- NCOLS ! 		\ Number of cols left


: RIGHT  ( row -- )   1 DW !  0 DH !      0 CW ! CH ! W @ LEN ! ; 
: LEFT   ( row -- )  -1 DW !  0 DH ! W @ 1- CW ! CH ! W @ LEN ! ;
: BOTTOM ( col -- )   0 DW !  1 DH !      0 CH ! CW ! H @ LEN ! ;
: TOP    ( col -- )   0 DW ! -1 DH ! H @ 1- CH ! CW ! H @ LEN ! ;


: ROW     BEGIN H @ 2/ CHOOSE DUP ROWS + C@ 0= IF DUP ROWS + 1 SWAP C! 2* -1 NROWS +! EXIT THEN DROP AGAIN ;
: COL     BEGIN W @ 2/ CHOOSE DUP COLS + C@ 0= IF DUP COLS + 1 SWAP C! 2* -1 NCOLS +! EXIT THEN DROP AGAIN ;

: DRAW  
  CW @ CH @   
  LEN @ 0 DO \ Draw until length expires
    DH @ + SWAP DW @ + SWAP 2DUP                        	( cw+dw ch+dh cw+dw ch+dh -- )
    M@ WALL = IF ELSE 
    2DUP WALL ROT ROT DH @ - SWAP DW @ - SWAP M! THEN		( cw+dw ch+dh -- )
  LOOP 2DROP ;
  
\ ROW RIGHT DRAW
\ COL TOP DRAW
0 RIGHT DRAW 
H @ 1- LEFT DRAW
0 TOP DRAW
W @ 1- BOTTOM DRAW
FOX 0 1 M!

VARIABLE N
0 N !

: RANDOMAZE
  BEGIN
    1 N +!
    NROWS @ 0= NCOLS @ 0= AND IF EXIT THEN
    2 CHOOSE 
    IF
      NCOLS @ 0<> IF COL 2 CHOOSE IF TOP ELSE BOTTOM THEN THEN 
    ELSE 
      NROWS @ 0<> IF ROW 2 CHOOSE IF LEFT ELSE RIGHT THEN THEN 
    THEN
    DRAW
    GREEN N @ cr WHITE .M CR YELLOW .S CR WHITE
    KEY DROP
  AGAIN ;

RANDOMAZE 

.M CR

