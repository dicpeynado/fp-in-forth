\ SYNTAX: (op:)? num/vec
create source 200 allot
source value c
\ errors
: echar ." Expected '" emit ." ' " abort ;
: enum ." Expected num" abort ;
: edata ." Expected num or vec" abort ;
: read ( -- ) source 200 accept source swap ;
: nextc ( -- ) c 1+ to c ;
: c= ( n -- ) c c@ = ;
: c<> ( n -- ) c c@ <> ;
: num? ( -- t|f ) c c@ [char] 0 [char] 9 1+ within ;
: vec? ( -- t|f ) [char] < c= ;
: toall? ( -- t|f ) [char] @ c= ;
: comp? ( -- t|f ) [char] ( c= ; \ )
: cons? ( -- t|f ) [char] [ c= ;
: endcons? ( -- t|f ) [char] ] c= ;
: endcomp? ( -- t|f ) [char] ) c= ;
: separator? [char] , c= [char] : c= or endcomp? or endcons? or ;

: peek ( -- n ) c dup 1+ to c ;
: lookback ( n -- ) to c ;
: lookahead ( n -- ) drop ;
: expect dup c= if nextc drop else ECHAR then ;
: num num? if c c@ emit nextc else ENUM then ;
: startvec [char] < expect ." <| " ;
: endvec [char] > expect ."  |>" ;
: vec startvec
	begin
		vec? if recurse 
		else num? if num then
		then
		[char] , c= if space nextc 0 else -1 then
	until
	endvec ;
: binop? 
[char] + c= 
[char] - c= or
[char] * c= or
[char] / c= or ;


: fun 
space 
binop?
begin c c@ emit nextc separator? until
if [char] 2 emit then ;

: reverse ( n -- a n )
begin endcomp? not while
	nextc
	cons? if c swap 1+
		1 begin dup while 
			nextc cons? if 
				1+ 
			else endcons? if 1- then
			then 
		repeat
		drop
		nextc
	else comp? if recurse 
	else c swap 1+
	then then
	begin separator? not while
		nextc 
	repeat 
	( {separator?} )
repeat
( {endcomp?} )
nextc ;

defer op
: comp 
\ put ops on stack in reverse order
0 reverse
0 do to c op loop
 ;
 
: cons 
0
nextc endcons? not if 1+ ."  dup" op then
begin endcons? not while
	nextc 1+ ."  over" op
	begin separator? not while
		nextc 
	repeat 
	( {separator?} )
repeat
( {endcomp?} )
nextc 
."  nip " . ." vec"
;

\ OP
:noname 
comp? if comp
else cons? if cons
else toall? if 
	peek toall? if 
		lookahead nextc ."  '" fun ."  aall " \ TODO: find a way to support cons and comp
	else lookback nextc ."  '" fun ."  all "  \ TODO: find a way to support cons and comp
	then
else fun
then
then
then ; is op

: apply c 
begin nextc [char] : c= until nextc 
vec? if 
	vec 
else num? 
	if num 
	else EDATA 
	then 
then
to c
op
 ;
: program drop to c vec? if vec else num? if num else apply then then ;
: repl read program ;
\ TESTS
s" <1,2,3>" program cr
s" <1,2,<3,4>>"  program cr
s" +:<1,2>"  program cr
s" (double,+):<1,2>"  program cr
s" ((+,vdup),+):<1,2>"  program cr
s" [+,-]:<1,2>" program cr
s" ((+,[id,id]),+):<1,2>" program cr
s" @double:<1,2,3>" program cr
