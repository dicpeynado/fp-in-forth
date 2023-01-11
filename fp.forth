6000 cells constant MEM_SIZE
0 value pmem		 				\ pointer to free memory cell
0 value vlen						\ REGISTER vector length	
0 value fun							\ REGISTER function
0 value lv							\ REGISTER (temp) last depth
0 value pvec						\ REGISTER pointer to vector
create mem MEM_SIZE allot 			\ create free memory pool
: reset mem MEM_SIZE + to pmem ;	\ deallocate all memory
: new pmem mem > 0= abort" out of mem" pmem swap cells - dup to pmem ;	\ memory allocation
: vec dup >r 0 do 1 new ! loop 1 new r> over ! ;						\ create vector of N on stack
: len @ ;							\ access vector length (stored in first cell of vector)
: pvec@+ pvec cell+ dup to pvec @ ;	\ auto increment and fetch pvec (C.H. Moore style)
									\  Friendly vector syntax: <| x y z |>
: <| lv depth to lv  ;				\  Save reg val on stack, record depth
: |> depth lv - vec swap to lv ;	\  Check change in depth and create vector, restore reg val

: .v dup len 0 do cell+ dup @ . loop drop ;
: .vt dup len 0 do cell+ dup @ . 9 emit loop drop ;
: addr? 5000000 > ; 
: .m dup len 0 do cell+ dup @ .v cr loop drop ;
: .l dup addr? if ." <| " pvec swap to pvec pvec len begin dup while pvec@+ recurse 1- repeat drop to pvec ." |> " else . then ;
: .lt dup addr? if pvec swap to pvec pvec len begin dup while pvec@+ .l 9 emit 1- repeat drop to pvec else . 9 emit then  ;
: .t dup len 0 do cell+ dup @ .lt cr loop drop ;
: s dup 0< if 1+ over len + then cells+ @  ;
: s1 1 s ; : s2 2 s ; : s3 3 s ; : s4 4 s ; : s5 5 s ;
: vcopy dup len 1+ dup new swap cells move pmem ;
: cols cells swap vcopy tuck dup len 0 do cell+ 2dup @ + @ over ! loop 2drop ;
: trans dup 1 s len dup >r 1+ 1 do dup i cols swap loop drop r> vec ;
: +2 dup 1 s swap 2 s + ;
: *2 dup 1 s swap 2 s * ;
: -2 dup 1 s swap 2 s - ;
: /2 dup 1 s swap 2 s / ;
: and2 dup 1 s swap 2 s and ;
: or2 dup 1 s swap 2 s or ;
: <2 dup 1 s swap 2 s < ;
: =2 dup 1 s swap 2 s = ;
: all swap vcopy tuck dup len 0 do cell+ 2dup @ swap execute over ! loop 2drop ;
: aall swap vcopy tuck dup len 0 do cell+ 2dup @ swap all over ! loop 2drop ;
: sqr dup * ;
: ints dup 1+ 1 do i swap loop vec ;
: v>do dup dup len 1+ cells+ swap cell+ ;
: v>s to pvec pvec len dup >r 0 do pvec@+ loop r> ;
: ins to fun dup to pvec pvec@+ swap len 1 do pvec@+ fun execute loop ;
: apndl dup 1 s swap 2 s v>s 1+ vec ;
: apndr dup 2 s >r 1 s v>s r> swap 1+ vec ;
: distl dup 1 s to vlen 2 s to pvec <| pvec len 0 do vlen pvec@+ 2 vec loop |> ;
: distr dup 2 s to vlen 1 s to pvec <| pvec len 0 do pvec@+ vlen 2 vec loop |> ;
: filter to fun to pvec <| pvec len 0 do pvec@+ dup fun execute 0= if drop then loop |> ; 
: v* ['] *2 all ;
: iapndr ['] apndr all ;
: fil1 dup 1 s swap 2 s > not ;
: fil ['] fil1 filter ;


reset 144 dup ints dup ' sqr all swap 2 vec trans 2 vec distl ' apndl all dup ' s3 all swap ' =2 all 2 vec trans ' and2 all ' or ins 

reset <| 9 ints 9 ints |> distl ' distr all dup ' *2 aall 2 vec trans ' trans all ' apndr aall ' fil all .t