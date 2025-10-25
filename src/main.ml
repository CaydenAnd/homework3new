open Ast

let bop_err = "bop_err"
let if_guard_err = "if_guard_err"
let unbound_var_err = "unbound_var_err"

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let string_of_val e =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | _ -> failwith "precondition violated"

let is_value = function
  | Int _ | Bool _ -> true
  | Var _ | Let _ | Binop _ | If _ -> false

let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | Bool _ -> e
  | Int _ -> e
  | Binop (b, e1, e2) -> Binop (b, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y then Let (y, e1', e2)
    else Let (y, e1', subst e2 v x)
  | If (e1, e2, e3) ->
    If (subst e1 v x, subst e2 v x, subst e3 v x)

let rec step = function
  | Int _ | Bool _ -> failwith "Does not step"
  | Var _ -> failwith unbound_var_err
  | Binop (b, e1, e2) when is_value e1 && is_value e2 -> step_bop b e1 e2
  | Binop (b, e1, e2) when is_value e1 -> Binop (b, e1, step e2)
  | Binop (b, e1, e2) -> Binop (b, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith if_guard_err
  | If (e1, e2, e3) -> If (step e1, e2, e3)

and step_bop b e1 e2 = match b, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

let rec eval e =
  if is_value e then e else eval (step e)

let interp s =
  s |> parse |> eval
