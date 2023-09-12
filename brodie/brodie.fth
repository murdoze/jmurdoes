: separ cr ." --------------------------------------------------" cr ;

: let constant ;

: var ( v "name" -- ) create , does> ;


42 let star
55 var fifi

: @+! ( v n -- u | v+n ) swap dup @ >r dup -rot  @ + swap ! r> ;


star . cr
fifi dup @ dup . 11 + swap !  cr
fifi @  .  cr
separ

fifi 10 @+! .
fifi 10 @+! .
fifi 10 @+! .
fifi 10 @+! .
star emit
separ cr cr
separ cr cr


: enum ( start count n*"name" -- )
  over + swap
  do i let loop ;


1 3  enum e1 e2 e3
e1 . e2 . e3 .

separ
separ

