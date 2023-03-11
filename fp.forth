6000 cells constant MEM_SIZE
0 value pmem	\ pointer to free memory cell
0 value vlen	\ REGISTER vector length	
0 value fun	\ REGISTER function
0 value lv	\ REGISTER (temp) last depth
0 value pvec	\ REGISTER pointer to vector
create mem MEM_SIZE allot 		\ create free memory pool
: reset mem MEM_SIZE + to pmem ;	\ deallocate all memory
: new pmem mem > 0= abort" out of mem" pmem swap cells - dup to pmem ;	\ memory allocation
: vec dup >r 0 do 1 new ! loop 1 new r> over ! ;			\ create vector of N on stack
: len @ ;				\ access vector length (stored in first cell of vector)
: pvec@+ pvec cell+ dup to pvec @ ;	\ auto increment and fetch pvec (C.H. Moore style)
					\  Friendly vector syntax: <| x y z |>
: <| lv depth to lv  ;			\  Save reg val on stack, record depth
: |> depth lv - vec swap to lv ;	\  Check change in depth and create vector, restore reg val
: addr? ( a -- a t|f ) dup pmem mem MEM_SIZE + within ; 

\ Printing Words ( a -- )
: .v	\ .v print as vector: <| 1 2 3 4 |> .v ==> 1 2 3 4 
	dup len 0 do 
		cell+ dup @ . 
	loop drop ;
: .vt	\ .v print as vector with tabs: <| 1 2 3 4 |> .v ==>	1	2	3	4 
	dup len 0 do 
		cell+ dup @ . 9 emit 
	loop drop ;
: .m 	\ .m print as matrix: <| <| 1 2 3 |> <| 4 5 6 |> |> .m ==>	1 2 3
	\								4 5 6
	dup len 0 do 
		cell+ dup @ .v cr 
	loop drop ;
: .l \ .l print as list (nested vectors) e.g. <| <| 1 2 <| 4 |> |> 5 |> .l ==> <| <| 1 2 <| 4 |> |> 5 |>
	addr? if 
		." <| " pvec swap		\ save last vector on the stack
		to pvec pvec len 
		begin dup while
			pvec@+ recurse 1-
		repeat drop
		to pvec ." |> " 		\ restore previous vector
	else .
	then ;
: .lt  \ .l print as list with tabs (helper word)
	addr? if 
		pvec swap 
		to pvec pvec len 
		begin dup while
			pvec@+ .l 9 emit 1- 
		repeat drop
		to pvec
	else . 9 emit 
	then  ;
: .t	\ .t print as table (combine tabs and lists) e.g. 
	dup len 0 do 
		cell+ dup @ .lt cr 
	loop drop ;

: s \ s ( a1 n1 -- n2|a2 ) select nth element of vector
	dup 0< if		\ if selection negative
		1+ over len +	\ take from the end of the vector
	then cells+ @  ;	\ else take from the start

\ define a few hardcoded selection words for ease
: s1 1 s ; : s2 2 s ; : s3 3 s ; : s4 4 s ; : s5 5 s ;

: vcopy \ ( a1 -- a2 ) copy vector a1 to vector a2
	dup len 1+ dup new swap cells move pmem ;
: cols \ ( a1 n1 -- a2|n2 )
	cells swap vcopy tuck \ ( a2 n2 a2 )
	dup len 0 do 
		cell+ 2dup @ + @ over !
	loop 2drop ;
: trans 
	dup s1 len dup >r
	1 + 1 do 
		dup i cols swap 
	loop drop 
	r> vec ;

\ Operations for two element vectors e.g. <| 1 2 |>
: v>2 \ ( a -- n1 n2 ) take two elements for words requiring two arguments
	dup s1 swap s2 ;
\ arithmetic
: +2 v>2 + ; : *2 v>2 * ; : -2 v>2 - ; : /2 v>2 / ; 
\ logic
: and2 v>2 and ; : or2 v>2 or ; 
\ comparison
: <2 v>2 < ; : >2 v>2 > ; : >=2 v>2 >= ; : <=2 v>2 <= ; : =2 v>2 = ; : <>2 v>2 <> ;

\ Vocabulary
: sqr dup * ;
: all \ all ( a1 xt -- a2 ) apply xt to all e.g. <| 1 2 3 4 |> ' sqr all ==> <| 1 4 9 16 |>
	swap vcopy tuck 
	dup len 0 do 	
		cell+ 2dup @ swap execute over ! 
	loop 2drop ;
: aall \ aall ( a1 xt -- a2 ) apply xt to all nested e.g. <| <| 1 2 |> <| 3 4 |> |>  ' sqr all ==> <| <| 1 4 |> <| 9 16 |> |>
	swap vcopy tuck 
	dup len 0 do
		cell+ 2dup @ swap all over ! 
	loop 2drop ;
: ints \ ints ( n -- a ) produce vector of integers from 1 to n e.g. 5 ints <| 1 2 4 5 |>
	dup 1+ 1 do 
		i swap	\ leave integers on the stack
	loop vec ;	\ take the count and put them in to a vector
: v>s \ v>s ( a -- n1 n2 n3... len ) put all elements of the vector on the stack ( inverse of vec )
	to pvec pvec len dup >r	\ point to vector and get length
	0 do
		pvec@+ 
	loop r> ;
: ins \ ins ( a xt -- n ) insert xt e.g. <| 1 2 3 4 |> ' +2 ins ==> <| <| 1 2 |> +2 3 |> +2 4 |> +2 ==> 10
	to fun 
	dup	to pvec pvec@+ 
	swap len 1 do 
		pvec@+ fun execute 
	loop ;
: apndl \ apndl ( a -- a ) append left e.g. <| 1 <| 2 3 4 |> |> ==> <| 1 2 3 4 |>
	dup s1 swap s2 v>s 1+ vec ;
: apndr \ apndr ( a -- a ) append right e.g. <| <| 1 2 3 |> 4 |> ==> <| 1 2 3 4 |>
	dup s2 >r s1 v>s r> swap 1+ vec ;
: distl \ distl ( a -- a ) distribute left e.g. <| 1 <| 2 3 4 |> |> ==> <| <| 1 2 |> <| 1 3 |> <| 1 4 |> |>
	dup s1 to vlen s2 to pvec 
	<| pvec len 0 do vlen pvec@+ 2 vec loop |> ;
: distr \ distr ( a -- a ) distribute left e.g. <| <| 1 2 3 |> 4 |> ==> <| <| 1 2 |> <| 1 3 |> <| 1 4 |> |>
	dup s2 to vlen s1 to pvec 
	<| pvec len 0 do pvec@+ vlen 2 vec loop |> ;
: filter	\ filter ( a xt -- a ) filter out false values when XT is applied
			\ e.g. <| <| 2 1 |> <| 2 2 |> <| 2 3 |> <| 2 4 |> |> ' <2 filter ==> <| <| 2 3 |> <| 2 4 |> |>
	to fun to pvec 
	<| pvec len 0 do pvec@+ dup fun execute 0= if drop then loop |> ; 
: fil<=2 ['] <=2 filter ;
: id ;

\ Example: calculating square root of 144
reset 144 dup ints dup ' sqr all swap 2 vec trans 2 vec distl ' apndl all dup ' s3 all swap ' =2 all 2 vec trans ' and2 all ' or ins . cr
\ Example: chinese multiplication table
reset <| 9 ints 9 ints |> distl ' distr all dup ' *2 aall 2 vec trans ' trans all ' apndr aall ' fil<=2 all .t

