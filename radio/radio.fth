\ RADIO-86RK emulator 

vocabulary radio
radio definitions

hex
: binary 2 base ! ;

: cell! ! ;
: cell@ @ ;

: @ ( a -- byte ) c@ ;
: ! ( byte a -- ) c! ;
: @@ ( a -- word ) dup @ swap 1+ @ 8 lshift or ;
: !! ( word a -- ) 2dup  swap swap !  swap 8 rshift swap 1+ ! ;

\ ----

: reg variable ;
reg a reg f reg b reg c reg d reg e reg h reg l reg pc reg sp

: a! a ! ; : a@ a @ ;
: b! b ! ; : b@ b @ ;
: c! c ! ; : c@ c @ ;
: d! d ! ; : d@ d @ ;
: e! e ! ; : e@ e @ ;
: h! h ! ; : h@ h @ ;
: l! l ! ; : l@ l @ ;
binary
: f! 11010111 and 00000010 or f ! ; 0 f!
: f@ f @ ;
hex


: hl!! dup l! 8 rshift h! ; : hl@@ h@ 8 lshift l@ or ;
: bc!! dup c! 8 rshift b! ; : bc@@ b@ 8 lshift c@ or ;
: de!! dup e! 8 rshift d! ; : de@@ d@ 8 lshift e@ or ;
: af!! dup f! 8 rshift a! ; : af@@ a@ 8 lshift f@ or ;

: pc!! pc !! ; : pc@@ pc @@ ;
: sp!! sp !! ; : sp@@ sp @@ ;

: pc+! ( n -- ) pc@@ + pc!! ; 
: sp+! ( n -- ) sp@@ + sp!! ; 

: pc++ 1 pc+! ;

\ Flag register (F) bits:
\ 7	6	5	4	3	2	1	0
\ S	Z	0	A	0	P	1	C

\ S - Sign Flag
\ Z - Zero Flag
\ 0 - Not used, always zero
\ A - also called AC, Auxiliary Carry Flag
\ 0 - Not used, always zero
\ P - Parity Flag
\ 1 - Not used, always one
\ C - Carry Flag

binary

: ? ( v -- 0|1 ) 0= if 0 else 1 then ;
: fc@ f@ 00000001 and ? ; : +fc! f@ 00000001 or f! ; : -fc! f@ 00000001 invert and f! ;
: fp@ f@ 00000100 and ? ; : +fp! f@ 00000100 or f! ; : -fp! f@ 00000100 invert and f! ;
: fa@ f@ 00000000 and ? ; : +fa! f@ 00000000 or f! ; : -fa! f@ 00000000 invert and f! ;
: fz@ f@ 01000000 and ? ; : +fz! f@ 01000000 or f! ; : -fz! f@ 01000000 invert and f! ;
: fs@ f@ 10000000 and ? ; : +fs! f@ 10000000 or f! ; : -fs! f@ 10000000 invert and f! ;
hex

: ?fc! dup 100 >= if +fc! else -fc! then ;
: ??fc! dup 1000 >= if +fc! else -fc! then ;
: ?fp! ; \ TODO implement parity check
: ?fa! ; \ TODO trap on DAA, then decide how to implement
: ?fz! dup 0= if +fz! else -fz! then ;
: ?fs! dup 80 and if +fs! else -fs! then ;

: ?fszapc! ?fc! ?fp! ?fa! ?fz! ?fs! ;
: ?fszap! ?fc! ?fp! ?fa! ?fz! ?fs! ;

\ ---- 

: .## 0 <# # # #> type space ;
: .#### 0 <# # # # # #> type space ;

: .regs 
   base cell@ hex
   cr ." PC=" pc@@ .#### ." A=" a@ .## ." BC=" bc@@ .#### ." DE=" de@@ .#### ." HL=" hl@@ .#### ." SP=" sp@@ .####
      ." F=" f@ .## ." C=" fc@ . ." P=" fp@ . ." A=" fp@ . ." Z=" fz@ . ." S=" fs@ .  cr
   base cell! 
;

\ ----

create mem 10000 allot
mem 10000 0 fill

: adr ( ofs -- addr ) FFFF and mem + ;

: m@ ( ofs -- byte ) adr @ ; \ TODO traps on read/write/execute of certain addresses
: m! ( byte ofs -- ) adr ! ;
: m@@ ( ofs -- word ) adr @@ ;
: m!! ( word ofs -- ) adr !! ;

variable mhere

: mhere! ( ofs -- ) FFFF and mhere cell! ;
: m, ( byte -- ) mhere cell@ m!  mhere cell@ 1+ mhere! ; 
: m,, ( word -- ) mhere cell@ m!!  mhere cell@ 2 + mhere! ; 

\ --


: @pc ( -- v ) pc@@ m@ ;
: @@pc ( -- v ) pc@@ m@@ ;

: !!sp ( v -- ) sp@@ m!! ;
: @@sp ( -- v ) sp@@ m@@ ;
: push ( v -- ) -2 sp+!  !!sp ;
: pop ( -- v ) @@sp  2 sp+! ;

: !bc bc@@ m! ; : @bc bc@@ m@ ;
: !de de@@ m! ; : @de de@@ m@ ;
: !hl hl@@ m! ; : @hl hl@@ m@ ;

\ ----

: mdump-line ( ofs -- )
  dup .#### space
  dup 10 0 do dup m@ .## 1+ loop drop space space
  10 0 do 
    dup m@ dup 0c = if drop [char] . then emit space 1+ 
  loop drop ;

: mdump ( ofs lines -- )
  cr 6 spaces 10 0 do i space .  loop 
  0 do dup cr mdump-line 10 + loop drop cr ;

: mdump-page 10 mdump ;

\ ----

variable emulator

: emulate 
  emulator cell@ dup
  0 <> if execute
  else drop pc@@ 1+ pc!! then ;

variable stopped
: stopped? stopped cell@ 0 <> ;
: running? stopped cell@ 0 = ;
: stop 1 stopped cell! ;
: unstop 0 stopped cell! ;

: step 
  emulate 
;

: go 
  unstop
  begin
    running? while
    step
  repeat
;

\ ---- 

create instrs
100 cells allot
instrs 100 cells 0 fill

: instr ( n -- addr ) FF and cells instrs + ;
: instr@ ( n -- xt ) instr cell@ ;

: :< :noname ;
: >; ( instr ) postpone ;  swap instr cell! ; immediate

: i8080emulate
  pc@@ m@ instr@ dup
  0 <> if execute else drop stop then
;
latestxt emulator cell!

\ https://pastraiser.com/cpu/i8080/i8080_opcodes.html
\ https://altairclone.com/downloads/manuals/8080%20Programmers%20Manual.pdf
\ http://www.emulator101.com/reference/8080-by-opcode.html

00 ( NOP         ) :< pc++ >;
01 ( LXI B, D16  ) :< pc++  @@pc bc!!  pc++ pc++ >;
02 ( STAX B      ) :< pc++  a@ !bc >;
03 ( INX B       ) :< pc++  bc@@ 1+ bc!! >;
04 ( INR B	 ) :< pc++  b@ 1+  ?fszap!  b! >;
05 ( DCR B       ) :< pc++  b@ 1-  ?fszap!  b! >;
06 ( MVI B, D8	 ) :< pc++  @pc b!  pc++ >;
07 ( RLC        1) :< cr ." RLC 07" cr stop >; (	CY	A = A << 1; bit 0 = prev bit 7; CY = prev bit 7 )

08 ( -           ) :< cr ." NOP 08" cr stop >;
09 ( DAD B	 ) :< hl@@ bc@@ + ??fc! hl!! >;
0A ( LDAX B	 ) :< pc++  @bc a! >;
0B ( DCX B       ) :< pc++  bc@@ 1- bc!! >;
0C ( INR C	 ) :< pc++  c@ 1+  ?fszap!  c! >;
0D ( DCR C       ) :< pc++  c@ 1-  ?fszap!  c! >;
0E ( MVI C, D8	 ) :< pc++  @pc c!  pc++ >;
0F ( RRC        1) :< cr ." RRC 0F" cr stop >; (	CY	A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0 )

10 ( -           ) :< cr ." NOP 10" cr stop >;
11 ( LXI D, D16  ) :< pc++  @@pc de!!  pc++ pc++ >;
12 ( STAX D      ) :< pc++  a@ !de >;
13 ( INX D       ) :< pc++  de@@ 1+ de!! >;
14 ( INR D	 ) :< pc++  d@ 1+  ?fszap!  d! >;
15 ( DCR D       ) :< pc++  d@ 1-  ?fszap!  d! >;
16 ( MVI D, D8	 ) :< pc++  @pc d!  pc++ >;
17 ( RAL	1) :< cr ." RAL 17" cr stop >; (	CY	A = A << 1; bit 0 = prev CY; CY = prev bit 7 )

18 ( -           ) :< cr ." NOP 18" cr stop >;
19 ( DAD D	 ) :< hl@@ de@@ + ??fc! hl!! >;
1A ( LDAX D	 ) :< pc++  @de a! >;
1B ( DCX D       ) :< pc++  de@@ 1- de!! >;
1C ( INR E	 ) :< pc++  e@ 1+  ?fszap!  e! >;
1D ( DCR E	 ) :< pc++  e@ 1-  ?fszap!  e! >;
1E ( MVI E, D8	 ) :< pc++  @pc e!  pc++ >;
1F ( RAR	1) :< cr ." RAR 1F" cr stop >; ( CY	A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0 )

20 ( -           ) :< cr ." NOP 20" cr stop >;
21 ( LXI H, D16  ) :< pc++  @@pc hl!!  pc++ pc++ >;
22 ( SHLD adr	 ) :< pc++  hl@@ @@pc m!!  pc++ pc++  >;
23 ( INX H       ) :< pc++  hl@@ 1+ hl!! >;
24 ( INR H	 ) :< pc++  h@ 1+  ?fszap!  h! >;
25 ( DCR H       ) :< pc++  h@ 1-  ?fszap!  h! >;
26 ( MVI H, D8	 ) :< pc++  @pc h!  pc++ >;
27 ( DAA	1) :< cr ." DAA 27" cr stop >;

28 ( -           ) :< cr ." NOP 28" cr stop >;
29 ( DAD H	 ) :< hl@@ hl@@ + ??fc! hl!! >;
2A ( LHLD adr	 ) :< pc++  @@pc hl!!  pc++ pc++ >;
2B ( DCX H       ) :< pc++  hl@@ 1- hl!! >;
2C ( INR L	 ) :< pc++  l@ 1+  ?fszap!  l! >;
2D ( DCR L	 ) :< pc++  l@ 1-  ?fszap!  l! >;
2E ( MVI L, D8	 ) :< pc++  @pc l!  pc++ >;
2F ( CMA	 ) :< pc++  fa@ if -fa! else +fa! then >; 

30 ( -           ) :< cr ." NOP 30" cr stop >;
31 ( LXI SP, D16 ) :< pc++  @@pc sp!!  pc++ pc++ >;
32 ( STA adr	 ) :< pc++  a@ @@pc m!  pc++ pc++ >;
23 ( INX SP      ) :< pc++  sp@@ 1+ sp!! >;
34 ( INR M	 ) :< pc++  @hl 1+ ?fszap! !hl >;
35 ( DCR M	 ) :< pc++  @hl 1- ?fszap! !hl >;
36 ( MVI M,D8	 ) :< pc++  @pc !hl  pc++ >;
37 ( STC	 ) :< pc++  +fc! >;

38 ( -           ) :< cr ." NOP 38" cr stop >;
39 ( DAD SP	 ) :< hl@@ sp@@ + ??fc! hl!! >;
3A ( LDA adr	 ) :< pc++  @pc a!  pc++ pc++ >;
3B ( DCX SP      ) :< pc++  sp@@ 1- sp!! >;
3C ( INR A	 ) :< pc++  a@ 1+  ?fszap!  a! >;
3D ( DCR A	 ) :< pc++  a@ 1-  ?fszap!  a! >;
3E ( MVI A, D8	 ) :< pc++  @pc a!  pc++ >;
3F ( CMC	 ) :< pc++  fc@ if -fc! else +fc! then >; 

: mov ( d s -- ) pc++  @ swap ! ;

40 ( MOV B,B	) :< b b mov >;
41 ( MOV B,C	) :< b c mov >;
42 ( MOV B,D	) :< b d mov >;
43 ( MOV B,E	) :< b e mov >;
44 ( MOV B,H	) :< b h mov >;
45 ( MOV B,L	) :< b l mov >;
46 ( MOV B,M	) :< pc++  @hl b! >; 
47 ( MOV B,A	) :< b a mov >;
48 ( MOV C,B	) :< c b mov >;    
49 ( MOV C,C	) :< c c mov >;    
4A ( MOV C,D	) :< c d mov >;    
4B ( MOV C,E	) :< c e mov >;    
4C ( MOV C,H	) :< c h mov >;    
4D ( MOV C,L	) :< c l mov >;    
4E ( MOV C,M	) :< pc++  @hl c! >;
4F ( MOV C,A	) :< c a mov >;    
50 ( MOV D,B	) :< d b mov >;    
51 ( MOV D,C	) :< d c mov >;    
52 ( MOV D,D	) :< d d mov >;    
53 ( MOV D,E	) :< d e mov >;    
54 ( MOV D,H	) :< d h mov >;    
55 ( MOV D,L	) :< d l mov >;    
56 ( MOV D,M	) :< pc++  @hl d! >;
57 ( MOV D,A	) :< d a mov >;    
58 ( MOV E,B	) :< e b mov >;    
59 ( MOV E,C	) :< e c mov >;    
5A ( MOV E,D	) :< e d mov >;    
5B ( MOV E,E	) :< e e mov >;    
5C ( MOV E,H	) :< e h mov >;    
5D ( MOV E,L	) :< e l mov >;    
5E ( MOV E,M	) :< pc++  @hl e! >;
5F ( MOV E,A	) :< e a mov >;    
60 ( MOV H,B	) :< h b mov >;    
61 ( MOV H,C	) :< h c mov >;    
62 ( MOV H,D	) :< h d mov >;    
63 ( MOV H,E	) :< h e mov >;    
64 ( MOV H,H	) :< h h mov >;    
65 ( MOV H,L	) :< h l mov >;    
66 ( MOV H,M	) :< pc++  @hl h! >;
67 ( MOV H,A	) :< h a mov >;    
68 ( MOV L,B	) :< l b mov >;    
69 ( MOV L,C	) :< l c mov >;    
6A ( MOV L,D	) :< l d mov >;    
6B ( MOV L,E	) :< l e mov >;    
6C ( MOV L,H	) :< l h mov >;    
6D ( MOV L,L	) :< l l mov >;    
6E ( MOV L,M	) :< pc++  @hl l! >;
6F ( MOV L,A	) :< c a mov >;    
70 ( MOV M,B	) :< pc++  b@ !hl >;
71 ( MOV M,C	) :< pc++  c@ !hl >;
72 ( MOV M,D	) :< pc++  d@ !hl >;
73 ( MOV M,E	) :< pc++  e@ !hl >;
74 ( MOV M,H	) :< pc++  h@ !hl >;
75 ( MOV M,L	) :< pc++  l@ !hl >;
76 ( HLT        ) :< cr ." HALT" cr >;
77 ( MOV M,A	) :< pc++  a@ !hl >;
78 ( MOV A,B	) :< a b mov >;    
79 ( MOV A,C	) :< a c mov >;    
7A ( MOV A,D	) :< a d mov >;    
7B ( MOV A,E	) :< a e mov >;    
7C ( MOV A,H	) :< a h mov >;    
7D ( MOV A,L	) :< a l mov >;    
7E ( MOV A,M	) :< pc++  @hl a! >;
7F ( MOV A,A	) :< a a mov >;    

80 ( ADD B	) :< pc++  a@ b@ + ?fszapc! a! >;
81 ( ADD C	) :< pc++  a@ c@ + ?fszapc! a! >;
82 ( ADD D	) :< pc++  a@ b@ + ?fszapc! a! >;
83 ( ADD E	) :< pc++  a@ e@ + ?fszapc! a! >;
84 ( ADD H	) :< pc++  a@ h@ + ?fszapc! a! >;
85 ( ADD L	) :< pc++  a@ l@ + ?fszapc! a! >;
86 ( ADD M	) :< pc++  a@ @hl + ?fszapc! a! >;
87 ( ADD A	) :< pc++  a@ a@ + ?fszapc! a! >;

88 ( ADC B	) :< pc++  a@ b@ + fc@ + ?fszapc! a! >;
89 ( ADC C	) :< pc++  a@ c@ + fc@ + ?fszapc! a! >;
8A ( ADC D	) :< pc++  a@ b@ + fc@ + ?fszapc! a! >;
8B ( ADC E	) :< pc++  a@ e@ + fc@ + ?fszapc! a! >;
8C ( ADC H	) :< pc++  a@ h@ + fc@ + ?fszapc! a! >;
8D ( ADC L	) :< pc++  a@ l@ + fc@ + ?fszapc! a! >;
8E ( ADC M	) :< pc++  a@ @hl + fc@ + ?fszapc! a! >;
8F ( ADC A	) :< pc++  a@ a@ + fc@ + ?fszapc! a! >;

90 ( SUB B	) :< pc++  a@ b@ - ?fszapc! a! >;
91 ( SUB C	) :< pc++  a@ c@ - ?fszapc! a! >;
92 ( SUB D	) :< pc++  a@ b@ - ?fszapc! a! >;
93 ( SUB E	) :< pc++  a@ e@ - ?fszapc! a! >;
94 ( SUB H	) :< pc++  a@ h@ - ?fszapc! a! >;
95 ( SUB L	) :< pc++  a@ l@ - ?fszapc! a! >;
96 ( SUB M	) :< pc++  a@ @hl - ?fszapc! a! >;
97 ( SUB A	) :< pc++  a@ a@ - ?fszapc! a! >;

98 ( SBB B	) :< pc++  a@ b@ - fc@ - ?fszapc! a! >;
99 ( SBB C	) :< pc++  a@ c@ - fc@ - ?fszapc! a! >;
9A ( SBB D	) :< pc++  a@ b@ - fc@ - ?fszapc! a! >;
9B ( SBB E	) :< pc++  a@ e@ - fc@ - ?fszapc! a! >;
9C ( SBB H	) :< pc++  a@ h@ - fc@ - ?fszapc! a! >;
9D ( SBB L	) :< pc++  a@ l@ - fc@ - ?fszapc! a! >;
9E ( SBB M	) :< pc++  a@ @hl - fc@ - ?fszapc! a! >;
9F ( SBB A	) :< pc++  a@ a@ - fc@ - ?fszapc! a! >;

A0 ( ANA B	) :< pc++  a@ b@ and ?fszapc! a! >;
A1 ( ANA C	) :< pc++  a@ c@ and ?fszapc! a! >;
A2 ( ANA D	) :< pc++  a@ b@ and ?fszapc! a! >;
A3 ( ANA E	) :< pc++  a@ e@ and ?fszapc! a! >;
A4 ( ANA H	) :< pc++  a@ h@ and ?fszapc! a! >;
A5 ( ANA L	) :< pc++  a@ l@ and ?fszapc! a! >;
A6 ( ANA M	) :< pc++  a@ @hl and ?fszapc! a! >;
A7 ( ANA A	) :< pc++  a@ a@ and ?fszapc! a! >;

A8 ( XRA B	) :< pc++  a@ b@ xor ?fszapc! a! >;
A9 ( XRA C	) :< pc++  a@ c@ xor ?fszapc! a! >;
AA ( XRA D	) :< pc++  a@ b@ xor ?fszapc! a! >;
AB ( XRA E	) :< pc++  a@ e@ xor ?fszapc! a! >;
AC ( XRA H	) :< pc++  a@ h@ xor ?fszapc! a! >;
AD ( XRA L	) :< pc++  a@ l@ xor ?fszapc! a! >;
AE ( XRA M	) :< pc++  a@ @hl xor ?fszapc! a! >;
AF ( XRA A	) :< pc++  a@ a@ xor ?fszapc! a! >;

B0 ( ORA B	) :< pc++  a@ b@ or ?fszapc! a! >;
B1 ( ORA C	) :< pc++  a@ c@ or ?fszapc! a! >;
B2 ( ORA D	) :< pc++  a@ b@ or ?fszapc! a! >;
B3 ( ORA E	) :< pc++  a@ e@ or ?fszapc! a! >;
B4 ( ORA H	) :< pc++  a@ h@ or ?fszapc! a! >;
B5 ( ORA L	) :< pc++  a@ l@ or ?fszapc! a! >;
B6 ( ORA M	) :< pc++  a@ @hl or ?fszapc! a! >;
B7 ( ORA A	) :< pc++  a@ a@ or ?fszapc! a! >;

B8 ( CMP B	) :< pc++  a@ b@ - ?fszapc! >;
B9 ( CMP C	) :< pc++  a@ c@ - ?fszapc! >;
BA ( CMP D	) :< pc++  a@ b@ - ?fszapc! >;
BB ( CMP E	) :< pc++  a@ e@ - ?fszapc! >;
BC ( CMP H	) :< pc++  a@ h@ - ?fszapc! >;
BD ( CMP L	) :< pc++  a@ l@ - ?fszapc! >;
BE ( CMP M	) :< pc++  a@ @hl - ?fszapc! >;
BF ( CMP A	) :< pc++  a@ a@ - ?fszapc! >;

: call ( ofs -- ) pc@@ push  pc!! ;
: jump ( ofs -- ) pc!! ;
: return ( -- )   pop  pc!! ;

C0 ( RNZ        ) :< pc++  fz@ if else return then >;
C1 ( POP B	) :< pc++  pop bc!! >;
C2 ( JNZ adr	) :< pc++  @@pc  pc++ pc++  fz@ if drop else jump then >;
C3 ( JMP adr	) :< pc++  @@pc  jump >;
C4 ( CNZ adr	) :< pc++  @@pc  pc++ pc++  fz@ if drop else call then >;
C5 ( PUSH B	) :< pc++  bc@@ push >;
C6 ( ADI D8     ) :< pc++  a@ @pc + ?fszapc! a!  pc++ >;                 
C7 ( RST 0	) :< pc++  0000 call >;

C8 ( RZ	        ) :< pc++  fz@ if return then >;
C9 ( RET        ) :< pc++  return >;	
CA ( JZ adr	) :< pc++  @@pc  pc++ pc++  fz@ if jump else drop then >;
CB ( -	        ) :< cr ." JMP CB" cr stop >; 
CC ( CZ adr	) :< pc++  @@pc  pc++ pc++  fz@ if call else drop then >;
CD ( CALL adr	) :< pc++  @@pc  pc++ pc++  call >;
CE ( ACI D8     ) :< pc++  a@ @pc + fc@ + ?fszapc! a!  pc++ >;
CF ( RST 1	) :< pc++  0008 call >;

D0 ( RNC	) :< pc++  fc@ if else return then >;
D1 ( POP D	) :< pc++  pop de!! >;
D2 ( JNC adr	) :< pc++  @@pc  pc++ pc++  fc@ if drop else jump then >;
D3 ( OUT D8	) :< pc++ >;
D4 ( CNC adr    ) :< pc++  @@pc  pc++ pc++  fc@ if drop else call then >;
D5 ( PUSH D	) :< pc++  de@@ push >;
D6 ( SUI D8	) :< pc++  a@ @pc - ?fszapc! a!  pc++ >;
D7 ( RST 2	) :< pc++  0010 call >;

D8 ( RC	        ) :< pc++  fc@ if return then >;
D9 ( -		) :< cr ." RET D9" cr stop >;
DA ( JC adr	) :< pc++  @@pc  pc++ pc++  fc@ if jump else drop then >;
DB ( IN D8	) :< pc++ >;
DC ( CC adr	) :< pc++  @@pc  pc++ pc++  fc@ if call else drop then >;
DD ( -	        ) :< cr ." CALL DD" cr stop >;
DE ( SBI D8     ) :< pc++  a@ @pc - fc@ - ?fszapc! a!  pc++ >;
DF ( RST 3      ) :< pc++  0018 call >;

E0 ( RPO	) :< pc++  fp@ if else return then >;
E1 ( POP H	) :< pc++  pop hl!! >;
E2 ( JPO adr	) :< pc++  @@pc  pc++ pc++  fp@ if drop else jump then >;
E3 ( XTHL	) :< pc++  @@sp hl@@ !!sp hl!! >;
E4 ( CPO adr	) :< pc++  @@pc  pc++ pc++  fp@ if drop else call then >;
E5 ( PUSH H	) :< pc++  hl@@ push >;
E6 ( ANI D8	) :< pc++  a@ @pc and ?fszapc! a!  pc++ >;
E7 ( RST 4      ) :< pc++  0020 call >;

E8 ( RPE	) :< pc++  fp@ if return then >;
E9 ( PCHL	) :< pc++  hl@@ pc!! >;
EA ( JPE adr	) :< pc++  @@pc  pc++ pc++  fp@ if jump else drop then >;
EB ( XCHG	) :< pc++  h@ d@ h! d! l@ e@ l! e! >;
EC ( CPE adr	) :< pc++  @@pc  pc++ pc++  fp@ if call else drop then >;
ED ( -	        ) :< cr ." CALL ED" cr stop >;
EE ( XRI D8     ) :< pc++  a@ @pc xor ?fszapc! a!  pc++ >;
EF ( RST 5	) :< pc++  0028 call >;

F0 ( RP	 	) :< pc++  fs@ if else return then >;
F1 ( POP PSW    ) :< pc++  pop af!! >;
F2 ( JP adr	) :< pc++  @@pc  pc++ pc++  fs@ if drop else jump then >;
F3 ( DI	        ) :< pc++ >;
F4 ( CP adr	) :< pc++  @@pc  pc++ pc++  fs@ if drop else call then >;
F5 ( PUSH PSW	) :< pc++  af@@ push >;
F6 ( ORI D8     ) :< pc++  a@ @pc or ?fszapc! a!  pc++ >;
F7 ( RST 6	) :< pc++  0030 call >;

F8 ( RM	        ) :< pc++  fs@ if return then >;
F9 ( SPHL	) :< pc++  hl@@ sp!! >;
FA ( JM adr	) :< pc++  @@pc  pc++ pc++  fs@ if jump else drop then >;
FB ( EI	        ) :< pc++ >;
FC ( CM adr	) :< pc++  @@pc  pc++ pc++  fs@ if call else drop then >;
FD ( -		) :< cr ." CALL FD" cr stop >;
FE ( CPI D8     ) :< pc++  a@ @pc - ?fszapc!  pc++ >;
FF ( RST 7	) :< pc++  0038 call >;

\ ---- 

create disinstrs
100 cells allot
disinstrs 100 cells 0 fill

: disinstr ( n -- addr ) FF and cells disinstrs + ;
: disinstr@ ( n -- xt ) disinstr cell@ ;

: :( :noname ;
: ); ( instr -- ) postpone ;  swap disinstr cell! ; immediate

variable dis
: dis@@ dis @ FFFF and ;
: dis!! FFFF and dis ! ;
: dis! dis!! ;
: dis++ dis@@ 1+ dis!! ;
: @dis dis@@ m@ ;
: @@dis dis@@ m@@ ;

: disasm ( ninstr -- )
  0 do
    cr
    dis@@ m@ disinstr@ dup
    0 <> if 
      dis@@ .#### space @dis .## 
      dis++
      execute
    else drop 
    then
  loop
;

: 6spaces 6 spaces ;
: .1" postpone 6spaces postpone ." ; immediate

\ TODO return string and argument length

00 :( .1" NOP" ); 
01 :( @@dis dup .#### space ." LXI B, " .#### dis++ dis++ ); 
02 :( .1" STAX B" ); 
03 :( .1" INX B" ); 
04 :( .1" INR B" ); 
05 :( .1" DCR B" ); 
06 :( @dis dup .## ."    " ." MVI B, " .## dis++ ); 
07 :( .1" RLC" ); 

08 :( .1" -" ); 
09 :( .1" DAD B" ); 
0A :( .1" LDAX B" ); 
0B :( .1" DCX B" ); 
0C :( .1" INR C" ); 
0D :( .1" DCR C" ); 
0E :( @dis dup .## ."    " ." MVI C, " .## dis++ ); 
0F :( .1" RRC" ); 

10 :( .1" -" ); 
11 :( @@dis dup .#### space ." LXI D, " .#### dis++ dis++ ); 
12 :( .1" STAX D" ); 
13 :( .1" INX D" ); 
14 :( .1" INR D" ); 
15 :( .1" DCR D" ); 
16 :( @dis dup .## ."    " ." MVI D, " .## dis++ ); 
17 :( .1" RAL" ); 

18 :( .1" -" ); 
19 :( .1" DAD D" ); 
1A :( .1" LDAX D" ); 
1B :( .1" DCX D" ); 
1C :( .1" INR E" ); 
1D :( .1" DCR E" ); 
1E :( @dis dup .## ."    " ." MVI E, " .## dis++ ); 
1F :( .1" RAR" ); 

20 :( .1" -" ); 
21 :( @@dis dup .#### space ." LXI H, " .#### dis++ dis++ ); 
21 :( @@dis dup .#### space ." SHLD " .#### dis++ dis++ ); 
23 :( .1" INX H" ); 
24 :( .1" INR H" ); 
25 :( .1" DCR H" ); 
26 :( @dis dup .## ."    " ." MVI H, " .## dis++ ); 
27 :( .1" DAA" ); 

28 :( .1" -" ); 
29 :( .1" DAD H" ); 
2A :( .1" LHLD adr" ); 
2B :( .1" DCX H" ); 
2C :( .1" INR L" ); 
2D :( .1" DCR L" ); 
26 :( @dis dup .## ."    " ." MVI L, " .## dis++ ); 
2F :( .1" CMA" ); 

30 :( .1" -" ); 
31 :( @@dis dup .#### space ." LXI SP, " .#### dis++ dis++ ); 
32 :( @@dis dup .#### space ." STA " .#### dis++ dis++ ); 
23 :( .1" INX SP" ); 
34 :( .1" INR M" ); 
35 :( .1" DCR M" ); 
36 :( @dis dup .## ."    " ." MVI M, " .## dis++ ); 
37 :( .1" STC" ); 

38 :( .1" -" ); 
39 :( .1" DAD SP" ); 
3A :( @@dis dup .#### space ." LDA " .#### dis++ dis++ ); 
3B :( .1" DCX SP" ); 
3C :( .1" INR A" ); 
3D :( .1" DCR A" ); 
3E :( @dis dup .## ."    " ." MVI A, " .## dis++ ); 
3F :( .1" CMC" ); 

40 :( .1" MOV B,B" ); 
41 :( .1" MOV B,C" ); 
42 :( .1" MOV B,D" ); 
43 :( .1" MOV B,E" ); 
44 :( .1" MOV B,H" ); 
45 :( .1" MOV B,L" ); 
46 :( .1" MOV B,M" ); 
47 :( .1" MOV B,A" ); 
48 :( .1" MOV C,B" ); 
49 :( .1" MOV C,C" ); 
4A :( .1" MOV C,D" ); 
4B :( .1" MOV C,E" ); 
4C :( .1" MOV C,H" ); 
4D :( .1" MOV C,L" ); 
4E :( .1" MOV C,M" ); 
4F :( .1" MOV C,A" ); 
50 :( .1" MOV D,B" ); 
51 :( .1" MOV D,C" ); 
52 :( .1" MOV D,D" ); 
53 :( .1" MOV D,E" ); 
54 :( .1" MOV D,H" ); 
55 :( .1" MOV D,L" ); 
56 :( .1" MOV D,M" ); 
57 :( .1" MOV D,A" ); 
58 :( .1" MOV E,B" ); 
59 :( .1" MOV E,C" ); 
5A :( .1" MOV E,D" ); 
5B :( .1" MOV E,E" ); 
5C :( .1" MOV E,H" ); 
5D :( .1" MOV E,L" ); 
5E :( .1" MOV E,M" ); 
5F :( .1" MOV E,A" ); 
60 :( .1" MOV H,B" ); 
61 :( .1" MOV H,C" ); 
62 :( .1" MOV H,D" ); 
63 :( .1" MOV H,E" ); 
64 :( .1" MOV H,H" ); 
65 :( .1" MOV H,L" ); 
66 :( .1" MOV H,M" ); 
67 :( .1" MOV H,A" ); 
68 :( .1" MOV L,B" ); 
69 :( .1" MOV L,C" ); 
6A :( .1" MOV L,D" ); 
6B :( .1" MOV L,E" ); 
6C :( .1" MOV L,H" ); 
6D :( .1" MOV L,L" ); 
6E :( .1" MOV L,M" ); 
6F :( .1" MOV L,A" ); 
70 :( .1" MOV M,B" ); 
71 :( .1" MOV M,C" ); 
72 :( .1" MOV M,D" ); 
73 :( .1" MOV M,E" ); 
74 :( .1" MOV M,H" ); 
75 :( .1" MOV M,L" ); 
76 :( .1" HLT" ); 
77 :( .1" MOV M,A" ); 
78 :( .1" MOV A,B" ); 
79 :( .1" MOV A,C" ); 
7A :( .1" MOV A,D" ); 
7B :( .1" MOV A,E" ); 
7C :( .1" MOV A,H" ); 
7D :( .1" MOV A,L" ); 
7E :( .1" MOV A,M" ); 
7F :( .1" MOV A,A" ); 

80 :( .1" ADD B" ); 
81 :( .1" ADD C" ); 
82 :( .1" ADD D" ); 
83 :( .1" ADD E" ); 
84 :( .1" ADD H" ); 
85 :( .1" ADD L" ); 
86 :( .1" ADD M" ); 
87 :( .1" ADD A" ); 

88 :( .1" ADC B" ); 
89 :( .1" ADC C" ); 
8A :( .1" ADC D" ); 
8B :( .1" ADC E" ); 
8C :( .1" ADC H" ); 
8D :( .1" ADC L" ); 
8E :( .1" ADC M" ); 
8F :( .1" ADC A" ); 

90 :( .1" SUB B" ); 
91 :( .1" SUB C" ); 
92 :( .1" SUB D" ); 
93 :( .1" SUB E" ); 
94 :( .1" SUB H" ); 
95 :( .1" SUB L" ); 
96 :( .1" SUB M" ); 
97 :( .1" SUB A" ); 

98 :( .1" SBB B" ); 
99 :( .1" SBB C" ); 
9A :( .1" SBB D" ); 
9B :( .1" SBB E" ); 
9C :( .1" SBB H" ); 
9D :( .1" SBB L" ); 
9E :( .1" SBB M" ); 
9F :( .1" SBB A" ); 

A0 :( .1" ANA B" ); 
A1 :( .1" ANA C" ); 
A2 :( .1" ANA D" ); 
A3 :( .1" ANA E" ); 
A4 :( .1" ANA H" ); 
A5 :( .1" ANA L" ); 
A6 :( .1" ANA M" ); 
A7 :( .1" ANA A" ); 

A8 :( .1" XRA B" ); 
A9 :( .1" XRA C" ); 
AA :( .1" XRA D" ); 
AB :( .1" XRA E" ); 
AC :( .1" XRA H" ); 
AD :( .1" XRA L" ); 
AE :( .1" XRA M" ); 
AF :( .1" XRA A" ); 

B0 :( .1" ORA B" ); 
B1 :( .1" ORA C" ); 
B2 :( .1" ORA D" ); 
B3 :( .1" ORA E" ); 
B4 :( .1" ORA H" ); 
B5 :( .1" ORA L" ); 
B6 :( .1" ORA M" ); 
B7 :( .1" ORA A" ); 

B8 :( .1" CMP B" ); 
B9 :( .1" CMP C" ); 
BA :( .1" CMP D" ); 
BB :( .1" CMP E" ); 
BC :( .1" CMP H" ); 
BD :( .1" CMP L" ); 
BE :( .1" CMP M" ); 
BF :( .1" CMP A" ); 

C0 :( .1" RNZ" ); 
C1 :( .1" POP B" ); 
C2 :( @@dis dup .#### space ." JNZ " .#### dis++ dis++ ); 
C2 :( @@dis dup .#### space ." JMP " .#### dis++ dis++ ); 
C2 :( @@dis dup .#### space ." CNZ " .#### dis++ dis++ ); 
C5 :( .1" PUSH B" ); 
C6 :( @dis dup .## ."    " ." ADI " .## dis++ ); 
C7 :( .1" RST 0" ); 

C8 :( .1" RZ" ); 
C9 :( .1" RET" ); 
CA :( @@dis dup .#### space ." JZ " .#### dis++ dis++ ); 
CB :( .1" -" ); 
CC :( @@dis dup .#### space ." CZ " .#### dis++ dis++ ); 
CD :( @@dis dup .#### space ." CALL " .#### dis++ dis++ ); 
CE :( @dis dup .## ."    " ." ACI " .## dis++ ); 
CF :( .1" RST 1" ); 

D0 :( .1" RNC" ); 
D1 :( .1" POP D" ); 
D2 :( @@dis dup .#### space ." JNC " .#### dis++ dis++ ); 
D3 :( @dis dup .## ."    " ." OUT " .## dis++ ); 
D4 :( @@dis dup .#### space ." CNC " .#### dis++ dis++ ); 
D5 :( .1" PUSH D" ); 
D6 :( @dis dup .## ."    " ." SUI " .## dis++ ); 
D7 :( .1" RST 2" ); 

D8 :( .1" RC" ); 
D9 :( .1" -" ); 
DA :( @@dis dup .#### space ." JC " .#### dis++ dis++ ); 
DB :( @dis dup .## ."    " ." IN " .## dis++ ); 
DC :( @@dis dup .#### space ." CC " .#### dis++ dis++ ); 
DD :( .1" -" ); 
DE :( @dis dup .## ."    " ." SUI " .## dis++ ); 
DF :( .1" RST 3" ); 

E0 :( .1" RPO" ); 
E1 :( .1" POP H" ); 
E2 :( @@dis dup .#### space ." JPO " .#### dis++ dis++ ); 
E3 :( .1" XTHL" ); 
E4 :( @@dis dup .#### space ." CPO " .#### dis++ dis++ ); 
E5 :( .1" PUSH H" ); 
E6 :( @dis dup .## ."    " ." ANI " .## dis++ ); 
E7 :( .1" RST 4" ); 

E8 :( .1" RPE" ); 
E9 :( .1" PCHL" ); 
EA :( @@dis dup .#### space ." JPE " .#### dis++ dis++ ); 
EB :( .1" XCHG" ); 
EC :( @@dis dup .#### space ." CPE " .#### dis++ dis++ ); 
ED :( .1" -" ); 
EE :( @dis dup .## ."    " ." XRI " .## dis++ ); 
EF :( .1" RST 5" ); 

F0 :( .1" RP1" ); 
F1 :( .1" POP PSW" ); 
F2 :( @@dis dup .#### space ." JP " .#### dis++ dis++ ); 
F3 :( .1" DI" ); 
F4 :( @@dis dup .#### space ." CP " .#### dis++ dis++ ); 
F5 :( .1" PUSH PSW" ); 
F6 :( @dis dup .## ."    " ." ORI " .## dis++ ); 
F7 :( .1" RST 6" ); 

F8 :( .1" RM" ); 
F9 :( .1" SPHL" ); 
FA :( @@dis dup .#### space ." JM " .#### dis++ dis++ ); 
FB :( .1" EI" ); 
FC :( @@dis dup .#### space ." CM " .#### dis++ dis++ ); 
FD :( .1" -" ); 
FE :( @dis dup .## ."    " ." CPI " .## dis++ ); 
FF :( .1" RST 7" ); 



\ -- Test programs
: test1 
33 a!
00 m,
01 m, 0020 m,,
02 m,
03 m,
02 m,
04 m,
02 m,
05 m,
02 m,
0c m, 0d m,

.regs
0 mdump-page
go
.regs
0 mdump-page
;

: test-mov
  11 a! 12 b! 13 c! 14 d! 15 e! 16 h! 17 l!
  40 m, 41 m, 42 m, 43 m, 44 m, 45 m, 46 m, 47 m, 
;

