#use "fstate.ml";;

let print_line s = print_string(s ^ "\n");;

let check_total fsa = if partial fsa then print_line("Der Automat ist partiell.") else print_line("Der Automat ist total.") ;;

let show_accepts fsa str = if accepts fsa str then print_line ("Akzeptiert " ^ str) else print_line ("Akzeptiert NICHT " ^ str) ;;

(* G1 *)

let a = new automaton ;;

a#add_transition 0 'a' 1 ;;
a#add_transition 0 'a' 2 ;;
a#add_transition 1 'b' 2 ;;
a#add_transition 1 'b' 1 ;;
a#add_transition 2 'a' 1 ;;
a#add_transition 2 'c' 0 ;;
a#add_astate 0 ;;
a#add_astate 2 ;;
a#add_alph 'd' ;;
show a;;

Format.print_newline() ;;

check_total a ;;

(* totalisieren *)
totalize a ;;

Format.print_newline() ;;

check_total a ;;

Format.print_newline() ;;

show a ;;

show_accepts a "aba" ;;

let b = exponentiate a ;;

show_accepts b "d" ;;
