open Types

let vars t =
    let rec aux = function
    | Var v -> IntSet.singleton v
    | Node (_, l) -> List.map aux l |> List.fold_left IntSet.union IntSet.empty 
    in IntSet.elements (aux t)

let rec subst s = function
| Var v -> (match StringMap.find_opt v s with | None -> Var v | Some t -> t)
| Node (x, l) -> Node (x, List.map (subst s) l)

let compose s1 s2 =
    StringMap.map (subst s2) s1 |> StringMap.union (fun k x y -> Some y) s2

let rec occurs v = function
| Var x when x = v -> true | Var _ -> false
| Node (_, l) -> List.exists (occurs v) l

exception Not_unifiable

let iden_subst = StringMap.empty

let rec mgu t1 t2 = match t1, t2 with
| Var x, Var y when x = y -> iden_subst
| Var x, t when (occurs x t) -> raise Not_unifiable
| t, Var x when (occurs x t) -> raise Not_unifiable
| Var x, t | t, Var x -> StringMap.singleton x t
| Node (f, _), Node (g, _) when f <> g -> raise Not_unifiable
| Node (_, xl), Node (_, yl) ->
    let rec aux acc xl yl = match xl, yl with
    | [], [] -> acc
    | [], _ | _, [] -> raise Not_unifiable
    | x::xs, y::ys ->
        let m = mgu (subst acc x) (subst acc y) in aux (compose acc m) xs ys
    in aux iden_subst xl yl

let rec conv_term_list tab l =
    let tab, rl' = List.fold_left (fun (tab, acc) t ->
        let tab, t' = conv_term tab t in tab, (t'::acc)
    ) (tab, []) l in
    tab, List.rev rl'

and conv_term tab = function
| Var v -> (match StringMap.find_opt v tab with
    | None -> let n = StringMap.cardinal tab in StringMap.add v n tab, Var n
    | Some n -> tab, Var n
) 
| Node (x, l) -> let tab, l' = conv_term_list tab l in tab, Node (x, l')

let conv_clause (t, l) =
    let tab, t' = conv_term StringMap.empty t in
    let tab, l' = conv_term_list tab l in
    (t', l')

let conv_kb = List.map conv_clause

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
| Var v -> (match IntMap.find_opt v tab with
    | None -> let (r, _) as nex = next_min nex in nex, IntMap.add v r tab, Var r
    | Some r -> nex, tab, Var r
)
| Node (x, l) ->
    let nex, tab, l' = rename_term_list nex tab l in nex, tab, Node (x, l')

let rename_clause ft (t, l) =
    let nex = (-1, vars ft) in
    let nex, tab, t' = rename_term nex IntMap.empty t in
    let _, _, l' = rename_term_list nex tab l in
    (t', l')

let reconsult file_name =
    let in_channel = open_in file_name in
    let lexbuf = Lexing.from_channel in_channel in
    conv_kb (Parser.prog Lexer.scan lexbuf)

let query () =
    print_string "?- "; flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    Parser.goal Lexer.scan lexbuf

let rec print_term = function
| Var v -> print_string "_"; print_int v
| Node (x, l) ->
    print_string x; (match l with 
    | [] -> ()
    | h::t ->
        print_string "(";
        print_term h;
        List.iter (fun x -> print_string ", "; print_term x) t;
        print_string ")"
    )

let rec print_goal = function
| [] -> print_string ".\n"
| h::[] -> print_term h; print_string ".\n"
| h::t -> print_term h; print_string ", "; print_goal t

let print_clause (t, g) =
    print_term t; if g = [] then () else print_string " :- "; print_goal g

let print_kb = List.iter print_clause

let parse_in parse_fn conv_fn =
    let lexbuf = Lexing.from_channel stdin in
    parse_fn Lexer.scan lexbuf |> conv_fn

let parse_prog () = parse_in Parser.prog conv_kb
let parse_clause () = parse_in Parser.clause conv_clause
let parse_term () = parse_in Parser.term (conv_term StringMap.empty)

let () =
    print_kb (reconsult "prog/succmath_simple.pl");
    flush stdout;
