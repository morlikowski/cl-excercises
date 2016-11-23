let print_line s = print_string(s ^ "\n");;

let rec l t = match t with
    | [] -> []
    | '0'::r -> '0'::'1'::l(r)
    | '1'::r -> '1'::'0'::l(r);;

let rec morse s n = match n with
  | 0 -> s
  | x -> morse (l s) (x-1);;

morse ['0'] 3;;

let rec sum l = match l with
  | [] -> 0
  | x::q -> x + sum q;;

sum [1;2;3;4];;

let rec flat l = match l with
  | [] -> []
  | a::q -> a @ flat q;;

flat [[0; 1]; [2]; [1; -1]];;
