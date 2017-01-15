(* Datei: fstate.ml
   Autor: Marcus Kracht
   Datum: 21.10.2008

   Dies ist ein Modul, das Automaten als Objekte definiert, und zwar
   nicht effizient, sondern möglichst Definitionsgetreu.

   Diese Datei ist denn auch eher Anschauungs- und Studienobjekt.

*)

(* Ich definiere in dem Modul StateSet Mengen von Zuständen.
   Zustände sind ganze Zahlen (also vom Typ <int>). *)

module StateSet = Set.Make(
   struct
      type t = int
      let compare x y = if x > y then 1
         else if x = y then 0
	 else -1
    end)

(* Ebenso definiere ich Mengen von Paaren von Zuständen
   (also Mengen von Paaren von ganzen Zahlen. *)

module StatePSet = Set.Make(
   struct
      type t = int * int
      let compare x y = if ((fst x) > (fst y)||
	    (fst x = fst y) && (snd x > snd y))
      then 1
         else if x = y then 0
	 else -1
    end)

(* Der record trans enthält einen Übergang.
   Die Felder sind:
   "ein": Anfangszustand,
   "symbol": eingelesenes Symbol,
   "aus": Endzustand. *)

type trans = {ein: int; symbol: char; aus: int}

module CharSet = Set.Make(
   struct
       type t = char
       let compare x y = if x > y then 1
	   else if x = y then 0
	   else -1
   end)

(* HashedTrans definiert Hashtabellen über Übergänge, damit der
   Automat schneller arbeitet. *)

module HTrans = Hashtbl.Make(
   struct
      type t = int * char
      let equal x y = (((fst x = fst y) && (snd x = snd y)))
      let hash x = ((256 * (fst x)) + (int_of_char (snd x)))
   end)

(* add_transition_entry h e fügt den Übergang e der Tabelle h hinzu. *)

let add_transition_entry htbl elt =
   HTrans.add htbl (elt.ein,elt.symbol) elt.aus;;

(* list_converts konvertiert eine Übergangs-Hashtabelle in eine
   List von Übergängen. *)

let list_convert htb =
   let conv c n =
      {ein = (fst c); symbol = (snd c); aus = n} in
   let conv_add c n tlist = if
      (List.mem n (HTrans.find_all htb c))
      then (conv c n)::tlist else tlist in
   HTrans.fold conv_add htb [];;

(* Hier ist die Definition der Klasse von "Automaten". Man schaue
   sich sorgsam all die Funktionen an, die jetzt zur Verfügung stehen.
   Achtung! Bei Beginn ist der Initialzustand 0, und die Zustandsmenge
   ist {0}. Alle anderen Mengen sind leer. Falls man einen Übergang
   hinzufügt, so werden das Symbol sowie der ein- und ausgehende
   Zustand gleich zu den Mengen hinzugegeben. Das spart Arbeit. Man
   muss also lediglich Symbole eingeben, die nicht in einem Übergang
   auftreten. Die akzeptierenden Zustände muss man natürlich immer
   angeben. *)

class automaton =
   object
      val mutable i = 0
      val mutable x = StateSet.singleton 0
      val mutable y = StateSet.empty
      val mutable z = CharSet.empty
      val mutable t = HTrans.create 0
      method get_initial = i
      method get_states = x
      method get_astates = y
      method get_alph = z
      method get_transitions = t
      method initialize_alph = z <- CharSet.empty
      method initialize_states = x <- StateSet.singleton 0
      method initialize_astates = y <- StateSet.empty
      method initialize_transitions n =
         t <- HTrans.create n
      method state_change s = i <- s
      method add_alph d = z <- CharSet.add d z
      method add_state d = x <- StateSet.add d x
      method add_astate d = y <- StateSet.add d y
      method add_transition u c v =
         (add_transition_entry t {ein = u; symbol = c; aus = v};
	  z <- CharSet.add c z; x <- StateSet.add u x;
	  x <- StateSet.add v x)
      method remove_alph d = z <- CharSet.remove d z
      method remove_state d = x <- StateSet.remove d x
      method remove_astate d = y <- StateSet.remove d y
      method remove_transition d = HTrans.remove t d
      method make_initial d = i <- d
      method make_alph d = z <- d
      method make_states d = x <- d
      method make_astates d = y <- d
      method make_transitions d = t <- d
      method iniset = StateSet.add i StateSet.empty
      method list_alph = CharSet.elements z
      method list_states = StateSet.elements x
      method list_astates = StateSet.elements y
      method list_transitions = list_convert t
   end

(* Hier ist eine Funktion, um einen Automaten zu kopieren.
   Dabei ist aut der Ausgangsautomat. Wird im Folgenden
   nicht benötigt. *)

let aut_copy aut =
   let c = new automaton in
   c#make_initial aut#get_initial;
   c#make_alph aut#get_alph;
   c#make_states aut#get_states;
   c#make_astates aut#get_astates;
   c#make_transitions aut#get_transitions;
   c

(* Die folgenden Funktionen übergehe ich kommentarlos.
   Sie definieren Hilfsfunktionen, mit denen wir und den
   Automaten schön darstellen lassen können. *)

let rec int_prt ls = match ls with
   [] -> Format.print_string("") |
   hd::[] -> (Format.print_int(hd)) |
   hd::tl -> (Format.print_int(hd);
      Format.print_string(", ");int_prt tl);;

let rec char_prt ls = match ls with
   [] -> Format.print_string("") |
   hd::[] -> (Format.print_char(hd)) |
   hd::tl -> (Format.print_char(hd);
      Format.print_string(", ");char_prt tl);;

let entry_print tr =
   Format.print_string("\123ein: ");
   Format.print_int(tr.ein);
   Format.print_string("; Symbol: ");
   Format.print_char(tr.symbol);
   Format.print_string("; aus: ");
   Format.print_int(tr.aus);
   Format.print_string("\125");;

let rec tr_prt ls =
   match ls with
   [] -> Format.print_string("") |
   hd::[] -> entry_print(hd) |
   hd::tl -> (entry_print(hd);
      Format.print_string(", ");
      Format.print_newline ();
      Format.print_string("   ");
      tr_prt tl);;

let list_print ls =
   Format.open_box 0;
   Format.print_char('\123');
   int_prt ls;
   Format.print_char('\125');
   Format.close_box ();
   Format.print_newline ();;

let alist_print ls =
   Format.open_box 0;
   Format.print_char('\123');
   char_prt ls;
   Format.print_char('\125');
   Format.close_box ();
   Format.print_newline ();;

let trlist_print ls =
   Format.open_box 0;
   Format.print_string("  \123");
   tr_prt ls;
   Format.print_char('\125');
   Format.close_box ();
   Format.print_newline ();;

(* show aut ist die Funktion, die uns den Automaten
   zeigt. *)

let show aut =
   print_string("Zustände: ");
      list_print aut#list_states;
   print_string("Akzeptierende Zustände: ");
      list_print aut#list_astates;
   print_string("Initialzustand: ");
      Format.open_box 0;
      Format.print_int(aut#get_initial);
      Format.close_box ();
      Format.print_newline ();
   print_string("Alphabet: ");
      alist_print aut#list_alph;
   print_string("Übergänge: ");
      Format.print_newline ();
      trlist_print aut#list_transitions;;

(* arrows aut erzeugt eine Liste von Paaren (m,n) derart, dass
   es einen Übergang von m nach n gibt.  *)

let arrows aut =
   let st = ref StatePSet.empty in
   let gc set (left,right) entry =
      if not (StatePSet.mem (left,entry) !set) then
	 set := StatePSet.add (left,entry) !set in
      HTrans.iter (gc st) aut#get_transitions;
   !st

(* closure s r berechnet die transitive Hülle  der Menge s
   unter der Relation r. *)

let rec flip set flag lrela1 lrela =
   match (lrela,flag) with
      ([],false) -> set |
      ([],true) -> flip set false [] lrela1 |
      ((h,d)::tl,_) when (StateSet.mem h set)
	 -> (flip (StateSet.add d set) true lrela1 tl) |
      (hd::tl,f) -> (flip set f (hd::lrela1) tl);;

let closure set rela =
   flip set false [] (StatePSet.elements rela);;

(* is_empty a prüft, ob der Automat a irgeneine Zeichenkette
   akzeptiert, dh ob L(a) leer ist. *)

let is_empty aut =
   let f = closure aut#iniset (arrows aut) in
   StateSet.is_empty (StateSet.inter f aut#get_astates)

(* assoc_make numbers the members of a list in ascending
   order *)

let rec assc slist =
   match slist with [] -> [] |
      hd::tl -> (hd,(List.length tl))::(assc tl)

let assoc_make slist = assc (List.rev slist)

(* totalize aut prüft, ob der Automat partiell ist;
   falls ja, so fügt es noch einen Zustand hinzu und
   totalisiert den Automaten. *)

let not_bound aut =
   let trs = aut#get_transitions in
   let lst = ref [] in
   let bad_list i chr = if HTrans.mem trs (i,chr)
      then lst := !lst
      else lst := (i,chr)::!lst in
   let state_check i = CharSet.iter (bad_list i)
	 aut#get_alph in
   StateSet.iter state_check aut#get_states;
   !lst

let partial aut =
   not ([] = not_bound aut)

let rec complete_trans trs elt lst =
   let add_entry t e pair =
      HTrans.add t pair e in
   List.iter (add_entry trs elt) lst

let totalize aut =
   let trs = aut#get_transitions in
   let lst = not_bound aut in
   if lst = [] then
      (Format.print_string("Der Automat ist schon total.");
      Format.print_newline())
   else
      begin
      Format.print_string("Totalisiere den Automaten.");
      let m = (StateSet.max_elt aut#get_states)+1 in
      complete_trans trs m lst;
      let trash x ch = HTrans.add trs (x,ch) x
      in CharSet.iter (trash m) aut#get_alph;
      aut#make_transitions trs
      end

(* compactify a komprimiert die Zustandsnamen. *)

let trsl tlist aslist =
   let remap aslist hd =
      {ein = (List.assoc hd.ein aslist);
       symbol = hd.symbol;
       aus = (List.assoc hd.aus aslist)}
   in List.map (remap aslist) tlist;;

let compactify aut =
   let tlist = aut#list_transitions
   and st = aut#get_states
   and ast = aut#get_astates in
   let ntbl = HTrans.create (List.length tlist)
   and slist = (assoc_make (StateSet.elements st)) in
   let ttlist = trsl tlist slist in
      List.iter (add_transition_entry ntbl) ttlist;
   aut#make_transitions ntbl;
   aut#make_initial (List.assoc (aut#get_initial) slist);
   aut#initialize_states;
   aut#initialize_astates;
   let state_add n =
      (aut#add_state (List.assoc n slist))
   in (StateSet.iter state_add st);
   let astate_add n =
      (aut#add_astate (List.assoc n slist))
   in (StateSet.iter astate_add ast);;

(* make_empty aut macht aus aut einen leeren Automaten.
   Man beachte, dass das Alphabet nicht verändert wird. *)

let make_empty aut =
      aut#initialize_transitions 0;
      aut#make_initial 0;
      aut#initialize_states;
      aut#add_state 0;
      aut#initialize_astates

(* restrict_to_accessible aut nimmt von aut unerreichbare
   Zustände weg. *)

let restrict_to_accessible aut =
(* erreichbar wird definiert; dafür benutzen wir die transitive
   Hülle ... *)
   let acc_states = (closure aut#iniset (arrows aut)) in
(* darauf schränken wir die Zustandsmenge ein. *)
      aut#make_states acc_states;
      aut#make_astates (StateSet.inter aut#get_astates
	 acc_states);
(* Unerreichbae Übergänge werden weggenommen. *)
   let htbl = aut#get_transitions
   and ctbl = HTrans.create ((StateSet.cardinal
	 (aut#get_states))*(StateSet.cardinal
	 (aut#get_states))*256) in
   let add_to_htbl c n = if (StateSet.mem n acc_states)
	 && (StateSet.mem (fst c) acc_states) then
	 HTrans.add ctbl c n
	 in
   HTrans.iter add_to_htbl htbl;
   aut#make_transitions ctbl;
(* falls es keinen erreichbaren akzeptierenden Zustand gibt,
   wird der Automat auf Null gesetzt. *)
   if ((StateSet.is_empty aut#get_astates)
      || not (StateSet.mem aut#get_initial aut#get_states))
      then
      (print_endline("Warnung: Dieser Automat akzeptiert keine Zeichenkette.");
   make_empty aut) else ();;

(* clean_fsa aut macht alle Schritte auf einmal. *)

let clean_fsa aut =
   restrict_to_accessible aut;
   compactify aut

(* runs_to htbl st x ist die Zahl y derart, dass x via st
   nach y geführt wird (mit Hashtabelle htbl). Es wird implizit
   angenommen, dass der Automat deterministisch ist, weil nur
   eine Lösung aufgesucht wird. *)

let rec runs_to g st x =
   if st = "" then x else
   let b = ref x in
   for i = 0 to (String.length st)-1 do
	b := HTrans.find g#get_transitions (!b,st.[i])
   done;
   !b

(* accepts g st schaut nach, ob wir vom Initialzustand mit st
   einen akzeptierenden Zustand erreichen können. Setzt voraus,
   dass der Automat deterministisch ist. *)

let accepts g st =
   StateSet.mem (runs_to g st g#get_initial) g#get_astates

(* exp_state aut set chr erzeugt in der deterministischen
   Maschine den Nachfolgerzustand von set unter Buchstabe
   chr. *)

let of_list l = List.fold_right (fun x s -> StateSet.add x s)
   l StateSet.empty

let next_exp_state htb set c =
   let nf x = of_list (HTrans.find_all htb (x,c))
   in
   StateSet.fold (fun x s -> (StateSet.union (nf x) s)) set StateSet.empty
   ;;

(* exponentiate aut berechnet den 'Exponentialautomaten' von aut.*)

let exponentiate aut =
   let d = new automaton in
   let d_current = ref d#get_initial
   and aut_current = ref (StateSet.singleton aut#get_initial)
   and d_max = ref d#get_initial
   and htb = aut#get_transitions
   and statefct = Queue.create ()
   and chars = Queue.create () in
   let assoc_list = ref [(!aut_current, !d_current)]
   in
   Queue.add (!aut_current, !d_current) statefct;
   while (not (Queue.is_empty statefct)
	 || not (Queue.is_empty chars)) do
      (* falls es noch Buchstaben gibt, werden die mit dem
	 aktuellen Zustand verarztet; dieser existiert, weil
	 statefct nicht leer ist  *)
   if not (Queue.is_empty chars) then
      begin
      let c = Queue.take chars in
      (* wir berechnen den Nachfolger ... *)
      let p = next_exp_state htb !aut_current c
      in
      (* und schauen, ob er schon konstruiert ist. Falls nein dann
	 mach erst einmal die Vorarbeiten: *)
      (if not (List.mem_assoc p !assoc_list) then
	 begin
	 (d_max := !d_max + 1;
	  Queue.add (p,!d_max) statefct;
	  assoc_list := (p,!d_max) :: !assoc_list;
        (* ist er akzeptierend? dann zu den akzeptierenden states
	   von d hinzufügen *)
	 if not (StateSet.is_empty (StateSet.inter aut#get_astates p))
	 then d#add_astate !d_max)
	 end);
      (* in jedem Fall den Übergang hinzufügen *)
      d#add_transition !d_current c !d_max
      end
      (* falls es keine Buchstaben mehr gibt, so wird ein
	 neuer Zustand aus statefct geholt und die Buchstaben-Qeue
	 aufgefüllt *)
   else
      begin
      let p = Queue.take statefct
      in
      d_current := snd p;
      aut_current := fst p;
      List.iter (fun x -> Queue.add x chars) aut#list_alph
      end;
   done;
   (* Und am Ende wird die Abbildung von Zustandsmengen von aut
      zu Zuständen von d angezeigt: *)
   print_endline ("Die Übersetzungsabbildung lautet:");
   List.iter (fun p ->
      begin
      match p with (s,i)
      -> (Format.open_box 0;
         Format.print_char('\123');
         int_prt (StateSet.elements s);
         Format.print_char('\125');
	 Format.print_string " :";
	 Format.print_int i;
	 Format.print_newline ();
	 Format.close_box ())
      end) !assoc_list;
   d ;;

(* let rec follow aut ast set i n alph lstr sts htb ntb =
   let next = (next_in_line set lstr) in
   if i=n
   then
      if (set = next)
      then ntb
      else (follow aut ast next 0 n alph lstr sts htb ntb)
   else
   let chr = (List.nth alph i) in
   let set2 = (exp_state htb sts set chr) in
   let lstr1 =
      if not (List.mem_assoc set2 lstr)
      then ((set2,(List.length lstr))::lstr)
      else lstr in
   if not (List.mem_assoc set2 lstr) &&
      not (StateSet.is_empty
	 (StateSet.inter set2 ast)) then
	 aut#add_astate (List.assoc set2 lstr1);
   let plus = {ein = (List.assoc set lstr1);
       symbol = chr;
       aus = (List.assoc set2 lstr1)} in
   (follow aut ast set (i+1) n alph lstr1 sts htb (plus::ntb)) *)

(* Die exponentiation von aut ist aut1.

let exponentiate aut =
   (* clean_fsa aut; *)
   let d = new automaton in
   let htb = aut#get_transitions
   and ast = aut#get_astates
   and i = aut#iniset in
   d#initialize_astates;
   d#make_initial 0;
   let ntb = HTrans.create
      (List.length (list_convert htb)) in
   let tlist =
      (follow aut ast i 0 (List.length aut#list_alph)
      aut#list_alph [(i,0)] aut#get_states htb [])
   in List.iter (add_transition_entry ntb) tlist;
   d#make_transitions ntb; *)

(* In diesem Modul definieren wir nun Mengen von Mengen. *)

module StateSetSet = Set.Make(
   struct
      type t = StateSet.t
      let compare x y = StateSet.compare x y
    end)

(* Wir definieren die Verfeinerung von einer Zustandsmenge set
   via einer Partition aset. Fügt keine leeren Mengen hinzu. *)

let set_refine aset set =
   let sct = StateSet.inter aset set in
   if ((StateSet.subset aset set)
      || (StateSet.is_empty sct))
   then (StateSetSet.singleton aset)
   else (StateSetSet.add
      sct (StateSetSet.singleton
      (StateSet.diff aset set)))

let part_refine pset set =
   if ((StateSetSet.is_empty pset) ||
      (StateSet.is_empty set))
   then pset
   else
      let add_refined set iset rset =
      StateSetSet.union rset (set_refine iset set) in
      StateSetSet.fold (add_refined set)
	 pset StateSetSet.empty;;

let char_refine states trans set ch pset =
   let goes_to set trans ch elt =
      ((HTrans.mem trans (elt,ch)) &&
      (StateSet.mem (HTrans.find trans (elt,ch)) set))
   in
   let preset = StateSet.filter (goes_to set trans ch)
      states in
   part_refine pset preset;;

let all_char_refine states alph trans set pset =
   CharSet.fold (char_refine states trans set) alph pset;;

let one_step_refine states alph trans pset =
   StateSetSet.fold (all_char_refine states alph trans)
      pset pset;;

let rec totally_refine states alph trans pset =
   let part = (one_step_refine states alph trans pset) in
   if (pset = part) then pset else
      (totally_refine states alph trans part);;

(* associatte_number e l findet der Index des ersten
   Vorkommens in der Umkehrung der Liste.
   *)

let rec associate_number elt lyst =
   if lyst = [] then 1 else
      if (List.hd lyst) = elt then 0
      else
      (associate_number elt (List.tl lyst))+1;;

(* compress aut part komprimiert jede Partitionsmenge von
   aut auf ein Element. Schaut nicht, ob der Automat
   verfeinert ist. *)

let compress aut part =
   let hl = StateSetSet.elements part in
      aut#initialize_states;
   let smb = (List.find (StateSet.mem aut#get_initial) hl)
      in
      aut#make_initial (associate_number smb (List.rev hl));
   let ast = aut#get_astates in
      aut#initialize_astates;
   let trs = aut#get_transitions in
      (aut#initialize_transitions
      ((List.length hl) * (List.length hl) * 256));
   let adt x y a =
      if
	 ((HTrans.mem trs
	 ((StateSet.min_elt (List.nth hl x)),a)))
	    &&
	 (StateSet.mem (HTrans.find trs
	 ((StateSet.min_elt (List.nth hl x)),a))
         (List.nth hl y))
         then (aut#add_transition
            {ein = x; symbol = a; aus = y})
	 else () in
   for i = 0 to ((List.length hl)-1) do
      aut#add_state i;
      if StateSet.subset (List.nth hl i) ast then
	 aut#add_astate i;
      for j = 0 to ((List.length hl)-1) do
	   CharSet.iter (adt i j) aut#get_alph
      done;
   done

(* automaton_refine aut verfeinert den Automaten falls nötig.
   Meldet sich, wenn der Automat keine akzeptierenden Zustände
   besitzt. Als erstes wird der Automat totalisiert. *)

let automaton_refine aut =
   let ast = aut#get_astates in
   let fst = aut#get_states in
   let trs = aut#get_transitions in
   if (StateSet.is_empty ast) then
      (Format.print_string
   ("Dieser Automat hat keine akzeptierenden Zustände!");
      Format.print_newline()) else
   let split =
      if (fst = ast) then
      (StateSetSet.singleton fst)
      else
      (StateSetSet.add ast
	 (StateSetSet.singleton
	    (StateSet.diff fst ast))) in
      (compress aut (totally_refine fst aut#get_alph
	 trs split))

(* Ende *)
