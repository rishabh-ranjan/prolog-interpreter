open Types

(*** printing ***)

let rec print_term = function
| Var v -> print_string "_"; print_int v
| Node (x, l) ->
    print_string x; ( match l with 
    | [] -> ()
    | h::t ->
        print_string "("; print_term h;
        List.iter (fun x -> print_string ", "; print_term x) t;
        print_string ")"
    )

let print_subst = IntMap.iter (fun k v ->
    print_string "_"; print_int k; print_string " -> ";
    print_term v; print_string "\n"
)

let rec print_goal = function
| Empty -> ()
| Term t -> print_term t
| And (l, r) ->
    let b = function
    | Or _ -> true
    | _ -> false
    in
    let p g =
        if b g then ( print_string "("; print_goal g; print_string ")" )
        else print_goal g
    in
    p l; print_string ", "; p r
| Or (l, r) ->
    print_goal l; print_string "; "; print_goal r

let print_clause (t, g) =
    print_term t; if g = Empty then () else print_string " :- "; print_goal g

let print_kb = List.iter print_clause

let rec print_soln_term_list inv = function
| [] -> ()
| h::[] -> print_soln_term inv h
| h::t -> print_soln_term inv h; print_string ", "

and print_soln_term inv = function
| Var v -> ( match IntMap.find_opt v inv with
    | None -> print_string "_"; print_int v
    | Some s -> print_string s
)
| Node (x, l) ->
    print_string x;
    if l = [] then () else (
        print_string "("; print_soln_term_list inv l; print_string ")"
    )

let print_soln inv = IntMap.iter (fun k v ->
    match IntMap.find_opt k inv with
    | None -> ()
    | Some s ->
        print_string s; print_string " = ";
        print_soln_term inv v; print_string "\n"
)

(*** term related ***)

(* sorted list of the variables in `t' *)
let vars t =
    let rec aux = function
    | Var v -> IntSet.singleton v
    | Node (_, l) -> List.map aux l |> List.fold_left IntSet.union IntSet.empty 
    in IntSet.elements (aux t)

let rec subst s = function
| Var v -> ( match IntMap.find_opt v s with | None -> Var v | Some t -> t)
| Node (x, l) -> Node (x, List.map (subst s) l)

(* subst mapping only the vars in vl *)
let clean_subst vl sub =
    let rec aux acc vl sl = match vl, sl with
    | [], _ | _, [] -> acc
    | vh::vt, (shk, shv)::st ->
        if vh < shk then aux acc vt sl
        else if vh > shk then aux acc vl st
        else aux (IntMap.add shk shv acc) vt st
    in aux IntMap.empty vl (IntMap.bindings sub)

(* subst equiv to applying `s1' then `s2' *)
let compose s1 s2 =
    IntMap.map (subst s2) s1 |> IntMap.union (fun k x y -> Some y) s2

(* does variable `v' occur in term *)
let rec occurs v = function
| Var x when x = v -> true | Var _ -> false
| Node (_, l) -> List.exists (occurs v) l

(* most general unifier of terms `t1' and `t2' *)
let rec mgu t1 t2 = match t1, t2 with
| Var x, Var y when x = y -> Some IntMap.empty
| Var x, t when (occurs x t) -> None
| t, Var x when (occurs x t) -> None
| Var x, t | t, Var x -> Some (IntMap.singleton x t)
| Node (f, _), Node (g, _) when f <> g -> None
| Node (_, xl), Node (_, yl) ->
    let rec aux acc xl yl = match xl, yl with
    | [], [] -> Some acc
    | [], _ | _, [] -> None
    | x::xs, y::ys -> ( match mgu (subst acc x) (subst acc y) with
        | None -> None
        | Some m -> aux (compose acc m) xs ys
    )
    in aux IntMap.empty xl yl

(*** string var to int var conversions ***)

let rec conv_term_list tab l =
    let tab, rl' = List.fold_left (fun (tab, acc) t ->
        let tab, t' = conv_term tab t in tab, (t'::acc)
    ) (tab, []) l in
    tab, List.rev rl'

and conv_term tab = function
| Var v ->
    (* anonymous variable *)
    if v = "_" then
        let n = StringMap.cardinal tab in
        let v = v ^ (string_of_int n) in
        StringMap.add v n tab, Var n
    else
        ( match StringMap.find_opt v tab with
        | None -> let n = StringMap.cardinal tab in StringMap.add v n tab, Var n
        | Some n -> tab, Var n
        ) 
| Node (x, l) -> let tab, l' = conv_term_list tab l in tab, Node (x, l')

let rec conv_goal tab = function
| Empty -> tab, Empty
| Term t ->
    let tab, t' = conv_term tab t in
    tab, Term t'
| And (l, r) ->
    let tab, l' = conv_goal tab l in
    let tab, r' = conv_goal tab r in
    tab, And (l', r')
| Or (l, r) ->
    let tab, l' = conv_goal tab l in
    let tab, r' = conv_goal tab r in
    tab, Or (l', r')

let conv_clause (t, g) =
    let tab, t' = conv_term StringMap.empty t in
    let tab, g' = conv_goal tab g in
    (t', g')

let conv_kb = List.map conv_clause

let rec conv_inv_term_list tabs l =
    let tabs, rl' = List.fold_left (fun (tabs, acc) t ->
        let tabs, t' = conv_inv_term tabs t in tabs, (t'::acc)
    ) (tabs, []) l in
    tabs, List.rev rl'

and conv_inv_term ((tab, inv) as tabs) = function
| Var v -> 
    (* anonymous variable *)
    if v = "_" then
        let n = StringMap.cardinal tab in
        let v = v ^ (string_of_int n) in
        (StringMap.add v n tab, inv), Var n
    else
        ( match StringMap.find_opt v tab with
        | None -> let n = StringMap.cardinal tab in
            (StringMap.add v n tab, IntMap.add n v inv), Var n
        | Some n -> tabs, Var n
        ) 
| Node (x, l) ->
    let tabs, l' = conv_inv_term_list tabs l in tabs, Node (x, l')

let rec conv_inv_goal ((tab, inv) as tabs) = function
| Empty -> tabs, Empty
| Term t ->
    let tabs, t' = conv_inv_term tabs t in
    tabs, Term t'
| And (l, r) ->
    let tabs, l' = conv_inv_goal tabs l in
    let tabs, r' = conv_inv_goal tabs r in
    tabs, And (l', r')
| Or (l, r) ->
    let tabs, l' = conv_inv_goal tabs l in
    let tabs, r' = conv_inv_goal tabs r in
    tabs, Or (l', r')

let conv_query = conv_inv_goal (StringMap.empty, IntMap.empty)

(*** variable renaming ***)

(* next min number not in `l' *)
let rec next_min (c, l) = match l with
| [] -> c+1, []
| h::t ->
    if h < c+1 then next_min (c, t)
    else if h > c+1 then c+1, l
    else next_min (c+1, t)

let rec rename_term_list nex tab l =
    let nex, tab, rl' = List.fold_left (fun (nex, tab, acc) t ->
        let nex, tab, t' = rename_term nex tab t in (nex, tab, t'::acc)
    ) (nex, tab, []) l in
    nex, tab, List.rev rl'

and rename_term nex tab = function
| Var v -> ( match IntMap.find_opt v tab with
    | None ->
        let (r, _) as nex = next_min nex in nex, IntMap.add v r tab, Var r
    | Some r -> nex, tab, Var r
)
| Node (x, l) ->
    let nex, tab, l' = rename_term_list nex tab l in nex, tab, Node (x, l')

let rec rename_goal nex tab = function
| Empty -> nex, tab, Empty
| Term t ->
    let nex, tab, t' = rename_term nex tab t in
    nex, tab, Term t'
| And (l, r) ->
    let nex, tab, l' = rename_goal nex tab l in
    let nex, tab, r' = rename_goal nex tab r in
    nex, tab, And (l', r')
| Or (l, r) ->
    let nex, tab, l' = rename_goal nex tab l in
    let nex, tab, r' = rename_goal nex tab r in
    nex, tab, Or (l', r')

let rename_clause vl (t, g) =
    let nex = (-1, vl) in
    let nex, tab, t' = rename_term nex IntMap.empty t in
    let _, _, g' = rename_goal nex tab g in
    (t', g')

(*** goal resolution ***)

let rec resolve kb sub = function
| End -> None, End
| New g -> ( match g with
    | Empty -> Some sub, End
    | Term t ->
        let sol, ts = resolve kb sub (Head (subst sub t, kb)) in
        ( match sol with
        | None -> None, End
        | Some sub' -> Some (compose sub sub'), ts
        )
    | And (l, r) -> resolve kb sub (And_goal (New l, New r, r))
    | Or (l, r) -> resolve kb sub (Or_goal (New l, New r))
)
| And_goal (ls, rs, r) ->
    let sol, ls' = resolve kb sub ls in
    ( match sol with
    | None -> None, End
    | Some sub' ->
        let sol, rs' = resolve kb sub' rs in
        ( match sol with
        | None -> resolve kb sub (And_goal (ls', New r, r))
        | _ -> sol, And_goal (ls, rs', r)
        )
    )
| Or_goal (ls, rs) ->
    let sol, ls' = resolve kb sub ls in
    ( match sol with
    | None ->
        let sol, rs' = resolve kb sub rs in
        sol, Or_goal (End, rs')
    | _ -> sol, Or_goal (ls', rs)
    )
| Head (t, kl) -> ( match kl with
    | [] -> None, End
    | kh::kt ->
        let vl = vars t in
        let (ct, cg) = rename_clause vl kh in
        ( match mgu t ct with
        | None -> resolve kb sub (Head (t, kt))
        | Some m -> resolve kb sub (Body (t, kt, vl, m, New cg))
        )
)
| Body (t, kt, vl, m, gs) ->
    let sol, gs' = resolve kb m gs in
    ( match sol with
    | None -> resolve kb sub (Head (t, kt))
    | Some sub' ->
        let r = clean_subst vl sub' |> compose sub in
        Some r, Body (t, kt, vl, m, gs')
    )

(*** user interface ***)

(* knowledge base of the prolog program in `file_name' *)
let reconsult file_name =
    let in_channel = open_in file_name in
    let lexbuf = Lexing.from_channel in_channel in
    conv_kb (Parser.prog Lexer.scan lexbuf)

(* exploration of resolution space *)
let rec explore kb inv gs =
    let sol, gs' = resolve kb IntMap.empty gs in
    match sol with
    | None -> print_string "\027[1;31mno.\027[0m\n"
    | Some s ->
        print_soln inv s;
        print_string "\027[1;34myes\027[0m";
        let inp = read_line () in
        print_string "\027[0m";
        ( match inp with
        | "" -> explore kb inv gs'
        | _ -> ()
        )

(* handle a query *)
let query kb =
    print_string "\n?- "; flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    let (_, inv), g = Parser.query Lexer.scan lexbuf |> conv_query in
    explore kb inv (New g)

(* accepts prolog file once as a commandline arg *)
let main () =
    if Array.length Sys.argv = 0 then
        print_endline "please provide file as a commandline argument"
    else
        let kb = reconsult Sys.argv.(1) in
        print_endline "\027[1mrpl\027[0m - a prolog subset by Rishabh Ranjan";
        print_endline "use `;' to explore the resolution space";
        print_endline "and `.' to start a fresh query";
        print_endline ("file consulted: " ^ Sys.argv.(1));
        while true do query kb done

let () = main ()

