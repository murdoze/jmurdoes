Word body:
 ^PFA
 ^PFA 
 ^PFA(EXIT)


Variant 1
==========

Word PFA:

   ...
   ^CFA2
   ^extra data 2
   ^CFA1
   ^extra data 1
   #CFA
   ^CFA0
-> (DATA)
   ... more data
   ... and maybe more data

CFA1:
   assembler here
   ...


    
Variant 2
==========

+ All states are equal
+ Name can be retrieved as a state
+ Extra data (flags?) can be retrieved as a state
+ State 0 can also have extra data
- None ?

Word PFA:

   ...
   ^CFA2
   ^extra data 2
   ^CFA1
   ^extra data 1
   ^CFA0
   ^ extra data 0
   #CFA
-> (DATA)
   

