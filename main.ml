open Types

let vars t =
    let rec aux = function
    | Var v -> IntSet.singleton v
    | Node (_, l) -> List.map aux l |> List.fold_left IntSet.union IntSet.empty 
    in IntSet.elements (aux t)

let rec subst s = function
| Var v -> (match IntMap.find_opt v s with | None -> Var v | Some t -> t)
| Node (x, l) -> Node (x, List.map (subst s) l)

let compose s1 s2 =
    IntMap.map (subst s2) s1 |> IntMap.union (fun k x y -> Some y) s2

let rec occurs v = function
| Var x when x = v -> true | Var _ -> false
| Node (_, l) -> List.exists (occurs v) l

let iden_subst = IntMap.empty

let rec mgu t1 t2 = match t1, t2 with
| Var x, Var y when x = y -> Some iden_subst
| Var x, t when (occurs x t) -> None
| t, Var x when (occurs x t) -> None
| Var x, t | t, Var x -> Some (IntMap.singleton x t)
| Node (f, _), Node (g, _) when f <> g -> None
| Node (_, xl), Node (_, yl) ->
    let rec aux acc xl yl = match xl, yl with
    | [], [] -> Some acc
    | [], _ | _, [] -> None
    | x::xs, y::ys -> (match mgu (subst acc x) (subst acc y) with
        | None -> None
        | Some m -> aux (compose acc m) xs ys
    )
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

let rec conv_goal_term_list tabs l =
    let tabs, rl' = List.fold_left (fun (tabs, acc) t ->
        let tabs, t' = conv_goal_term tabs t in tabs, (t'::acc)
    ) (tabs, []) l in
    tabs, List.rev rl'

and conv_goal_term ((tab, inv) as tabs) = function
| Var v -> (match StringMap.find_opt v tab with
    | None -> let n = StringMap.cardinal tab in
        (StringMap.add v n tab, IntMap.add n v inv), Var n
    | Some n -> tabs, Var n
) 
| Node (x, l) -> let tabs, l' = conv_goal_term_list tabs l in tabs, Node (x, l')

let conv_goal = conv_goal_term_list (StringMap.empty, IntMap.empty)

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

let rec resolve_term_list kb = List.fold_left (resolve_term kb)

and resolve_term kb sub_opt tm = match sub_opt with
| None -> None
| Some sub ->
    let tm = subst sub tm in
    let rec aux = function
    | [] -> None
    | kh::kt -> 
        let (t, l) = rename_clause tm kh in
        (match mgu t tm with
        | None -> aux kt
        | Some m ->
            (match resolve_term_list kb (Some m) l with
            | None -> aux kt
            | Some s -> Some (compose (compose sub m) s)
            )
        )
    in aux kb

let resolve_goal kb = resolve_term_list kb (Some iden_subst)

let rec print_soln_term_list inv = function
| [] -> ()
| h::[] -> print_soln_term inv h
| h::t -> print_soln_term inv h; print_string ", "

and print_soln_term inv = function
| Var v -> (match IntMap.find_opt v inv with
    | None -> print_string "_"; print_int v
    | Some s -> print_string s
)
| Node (x, l) ->
    print_string x; print_string "("; print_soln_term_list inv l; print_string ")"

let print_soln inv = IntMap.iter (fun k v ->
    (match IntMap.find_opt k inv with
    | None -> print_string "_"; print_int k
    | Some s -> print_string s
    ); print_string " = "; print_soln_term inv v; print_string ".\n"
)

let reconsult file_name =
    let in_channel = open_in file_name in
    let lexbuf = Lexing.from_channel in_channel in
    conv_kb (Parser.prog Lexer.scan lexbuf)

let query kb =
    print_string "?- "; flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    let (_, inv), g = Parser.goal Lexer.scan lexbuf |> conv_goal in
    (match resolve_goal kb g with
    | None -> print_string "\nfalse.\n"
    | Some s -> print_soln inv s; print_string "\ntrue.\n"
    ); flush stdout

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
let parse_term () = snd (parse_in Parser.term (conv_term StringMap.empty))

