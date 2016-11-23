let john = "b";;
let karla = "bc";;
let first = 0;;
let human s = (s.[first] = 'b');;
let sees i j = if (String.length i) <= (String.length j) then ((String.sub j first (String.length i)) = i) else false;;
let every f = if (f "b") then "b" else "";;

(* D1 *)

(* In diesem Zusammenhang sind e und string sowie bool und t gleichbedeutend *)

(* Johann sieht Karla. | "Johann" :: e, "sieht Karla" :: e -> t *)
(* Karla sieht Johann. | "Karla" :: e, "sieht Johann" :: e -> t *)
(* Jeder Mensch sieht Karla | "Jeder Mensch" :: e, "sieht Karla" :: e -> t *)
(* Jeder Mensch sieht Johann. | "Jeder Mensch" :: e , "sieht Johann" :: e -> t *)

(* D2 *)

sees john karla;;
sees karla john;;

sees (every human) karla;;
sees (every human) john;;

(* D3 *)
(* In meiner Implementierung (e -> t) -> (e -> t) damit das Argument mit dem Typ von human zusammenpasst und der Rückgabewert mit dem erwarteten Argumenttyp von every *)

(* D4 *)

(* Johann singt morgen. | morgen :: (e -> t) -> (e -> t)  *)
(* Johann sieht morgen Karla. | morgen :: (e -> e -> t) -> (e -> e -> t)  *)
