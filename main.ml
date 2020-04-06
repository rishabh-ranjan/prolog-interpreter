open Types

let rec subst s = function
| Var v -> ( match IntMap.find_opt v s with | None -> Var v | Some t -> t )
| Node (x, l) -> Node (x, List.map (subst s) l)

let compose s1 s2 =
    IntMap.map (subst s2) s1 |> IntMap.union (fun k x y -> Some y) s2

let rec occurs v = function
| Var x when x = v -> true | Var _ -> false
| Node (_, l) -> List.exists (occurs v) l

exception Not_unifiable

let iden_subst = IntMap.empty

let rec mgu t1 t2 = match t1, t2 with
| Var x, Var y when x = y -> iden_subst
| Var x, t when (occurs x t) -> raise Not_unifiable
| t, Var x when (occurs x t) -> raise Not_unifiable
| Var x, t | t, Var x -> IntMap.singleton x t
| Node (f, _), Node (g, _) when f <> g -> raise Not_unifiable
| Node (_, xl), Node (_, yl) ->
    let rec aux acc xl yl = match xl, yl with
    | [], [] -> acc
    | [], _ | _, [] -> raise Not_unifiable
    | x::xs, y::ys ->
        let m = mgu (subst acc x) (subst acc y) in aux (compose acc m) xs ys
    in aux iden_subst xl yl

let rec str_to_int_term ((var_tab, var_inv, sym_tab, sym_inv) as tabs) = function
| Var x -> ( match StringMap.find_opt x var_tab with
    | Some n -> tabs, Var n
    | None ->
        let n = StringMap.cardinal var_tab in
        (StringMap.add x n var_tab, IntMap.add n x var_inv, sym_tab, sym_inv),
        Var n
)
| Node (x, l) ->
        let tabs', rl' = List.fold_left (fun (tabs, acc) t ->
            let tabs', t' = str_to_int_term tabs t in (tabs', t'::acc)
        ) (tabs, []) l in
        let l' = List.rev rl' in
        ( match StringMap.find_opt x sym_tab with
        | Some n -> tabs', Node (n, l')
        | None ->
            let n = StringMap.cardinal sym_tab in
            let var_tab', var_inv', sym_tab', sym_inv' = tabs' in
            (var_tab', var_inv', StringMap.add x n sym_tab', IntMap.add n x sym_inv'),
            Node (n, l')
        )

let str_to_int_ast tabs ast =
    let rec aux acc tabs = function
    | [] -> List.rev acc, tabs
    | h::t -> let tabs', h' = str_to_int_term tabs h in aux (h'::acc) tabs' t
    in aux [] tabs ast

