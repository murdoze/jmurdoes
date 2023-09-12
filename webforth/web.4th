\ **************************************************************
\ * Static site generator in Forth
\ **************************************************************


require unicode.5th
require colors.5th


\ * Web parser itself

: ] [char] [ word count type ;

cr cr

\ ] a a a a a [ 777 111 + . cr
\ ] b b b b b cr 888 111 + . [ cr
\ ] o\h [ cr


91 word ( a a a a a a 
b b b b b b b b b b [ ) count type

