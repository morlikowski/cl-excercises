(* combinators are type-abstract or polymorphic *)

(* ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
let s x y z = x z (y z);;

(* 'a -> 'a *)
let i x = x;;

(* 'a -> 'b -> 'a *)
let k x y = x;;

(* Vorwaertsapplikation *)

(* ('a -> 'b) -> 'a -> 'b *)
let a x y = x y;;

(* Rueckwaertsapplikation *)

(* 'a -> ('a -> 'b) -> 'b *)
let r x y = y x;;

(* Excercise C1 *)

(* test values for x and y *)
let f x = x + 1;;
let n = 1;;

(* S(KI) = A *)
s (k i) f n = k i f (f n);;
k i f (f n) = i (f n);;
i (f n) = f n;;
f n = a f n;;

(* S(K(SI))K = R *)
s (k (s i)) k n f = (k (s i)) n (k n) f;;
(k (s i)) n (k n) f = (s i) (k n) f;;
(s i) (k n) f = i f ((k n) f);;
i f ((k n) f) = f ((k n) f);;
f ((k n) f) = f n;;
f n = r n f;;

(* Excercise C2 *)

(* SKII *)
s k i i n = k i (i i) n;;
k i (i i) n = i n;;

(* ASRIK *)

let t = s r;;
t i;;

(* s has type ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
(* r has type 'a -> ('a -> 'b) -> 'b *)
(* therefore 'a -> 'b -> 'c = 'a -> ('a -> 'b) -> 'b *)
(* so that t has type ('a -> 'a -> 'b) -> 'a -> 'b *)

(* t has type ('_a -> '_a -> '_b) -> '_a -> '_b *)
(* i has type 'a -> 'a *)
(* for the first application to work 'a -> 'a needs to equal ('a -> 'a -> 'b) and that cannot be true *)

(* Note: The underscore prefix in the type "'_a" indicates that this type is weakly polymorphic. In other words, "'_a" is not a polymorphic type variable but a temporary placeholder for an unknown at this time type. *)

(* SAIx *)

(* Excercise C3 *)

let o x = x x;;
