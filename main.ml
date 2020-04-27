open Types

(*** printing ***)

let print_var (s, v) =
    print_string v; if s = 0 then () else (print_string "#"; print_int s)

let rec print_term = function
| Var v -> print_var v
| Node (x, l) ->
    print_string x; ( match l with 
    | [] -> ()
    | h::t ->
        print_string "("; print_term h;
        List.iter (fun x -> print_string ", "; print_term x) t;
        print_string ")"
    )

let print_subst = VarMap.iter (fun k v ->
    print_var k; print_string " -> ";
    print_term v; print_string "\n"
)

let print_soln = VarMap.iter (fun (s, k) v ->
    if s <> 0 then () else (
        print_var (s, k); print_string " = ";
        print_term v; print_string "\n"
    )
)

(*** print funcs for debugging ***)

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

(*** term related ***)

let rec subst s = function
| Var v -> ( match VarMap.find_opt v s with | None -> Var v | Some t -> t)
| Node (x, l) -> Node (x, List.map (subst s) l)

(* subst equiv to applying `s1' then `s2' *)
let compose s1 s2 =
    VarMap.map (subst s2) s1 |> VarMap.union (fun k x y -> Some y) s2

(*
 * most general unifier of terms `t1' and `t2'
 * omit occurs check as it is redundant here
 *)
let rec mgu t1 t2 = match t1, t2 with
| Var x, Var y when x = y -> Some VarMap.empty
| Var x, t | t, Var x -> Some (VarMap.singleton x t)
| Node (f, _), Node (g, _) when f <> g -> None
| Node (_, xl), Node (_, yl) ->
    let rec aux acc xl yl = match xl, yl with
    | [], [] -> Some acc
    | [], _ | _, [] -> None
    | x::xs, y::ys -> ( match mgu (subst acc x) (subst acc y) with
        | None -> None
        | Some m -> aux (compose acc m) xs ys
    )
    in aux VarMap.empty xl yl

(*** variable renaming ***)

let rec rename_term n = function
| Var (_, v) -> Var (n, v)
| Node (x, l) -> Node (x, List.map (rename_term n) l)

let rec rename_goal n = function
| Empty -> Empty
| Term t -> Term (rename_term n t)
| And (l, r) -> And (rename_goal n l, rename_goal n r)
| Or (l, r) -> Or (rename_goal n l, rename_goal n r)

let rename_clause n (t, g) = (rename_term n t, rename_goal n g)

(*** goal resolution ***)

let rec resolve kb sc sub = function
| End -> sc, None, End
| New g -> ( match g with
    | Empty -> sc, Some sub, End
    | Term t -> resolve kb sc sub (Head (subst sub t, kb))
    | And (l, r) -> resolve kb sc sub (And_goal (New l, New r, r))
    | Or (l, r) -> resolve kb sc sub (Or_goal (New l, r))
)
| And_goal (ls, rs, r) ->
    let lsc, lsol, ls' = resolve kb sc sub ls in
    ( match lsol with
    | None -> sc, None, End
    | Some lsub ->
        let rsc, rsol, rs' = resolve kb lsc lsub rs in
        ( match rsol with
        | None -> resolve kb sc sub (And_goal (ls', New r, r))
        | _ -> rsc, rsol, And_goal (ls, rs', r)
        )
    )
| Or_goal (ls, r) ->
    let lsc, lsol, ls' = resolve kb sc sub ls in
    ( match lsol with
    | None -> resolve kb sc sub (New r)
    | _ -> lsc, lsol, Or_goal (ls', r)
    )
| Head (t, ks) -> ( match ks with
    | [] -> sc, None, End
    | kh::kt ->
        let (ct, cg) = rename_clause sc kh in
        ( match mgu t ct with
        | None -> resolve kb sc sub (Head (t, kt))
        | Some m -> resolve kb sc sub (Body ((t, kt), m, New cg))
        )
)
| Body (nh, m, gs) ->
    let sc', sol, gs' = resolve kb (sc+1) m gs in
    ( match sol with
    | None -> resolve kb sc sub (Head nh)
    | Some sub' -> sc', Some (compose sub sub'), Body (nh, m, gs')
    )

(*** user interface ***)

(* knowledge base of the prolog program in `file_name' *)
let reconsult file_name =
    let in_channel = open_in file_name in
    let lexbuf = Lexing.from_channel in_channel in
    Parser.prog Lexer.scan lexbuf

(* exploration of resolution space *)
let rec explore kb gs =
    let _, sol, gs' = resolve kb 1 VarMap.empty gs in
    match sol with
    | None -> print_string "\027[1;31mno.\027[0m\n"
    | Some s ->
        print_soln s;
        print_string "\027[1;34myes\027[0m";
        let inp = read_line () in
        print_string "\027[0m";
        ( match inp with
        | "" -> explore kb gs'
        | _ -> ()
        )

(* accept a query *)
let query kb =
    print_string "\n?- "; flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    let g = Parser.query Lexer.scan lexbuf in
    explore kb (New g)

(* accepts prolog file once as a commandline arg *)
let main () =
    if Array.length Sys.argv < 2 then
        print_endline "please provide file as a commandline argument"
    else
        let kb = reconsult Sys.argv.(1) in
        print_endline "\027[1mrpl\027[0m - a prolog subset by Rishabh Ranjan";
        print_endline "- press enter to explore the resolution space";
        print_endline "- any other input starts a fresh query";
        print_endline "- Cmd-C, Ctrl-C or Ctrl-Z (system dependent) to exit";
        print_endline ("file consulted: " ^ Sys.argv.(1));
        while true do query kb done

let () = main ()

