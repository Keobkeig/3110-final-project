type t =
  | Const of float
  | Var of string
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Neg of t
  | Exp of t
  | Log of t
  | Sin of t
  | Cos of t
  | Tanh of t

type token =
  | Number of float
  | Ident of string
  | Plus
  | Minus
  | Star
  | Slash
  | LParen
  | RParen
  | End

type stream = { tokens : token array; mutable index : int }

let is_digit ch = ch >= '0' && ch <= '9'

let is_letter ch =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch = '_'

let is_alphanumeric ch = is_letter ch || is_digit ch

let token_name token =
  match token with
  | Number _ -> "number"
  | Ident name -> "identifier(" ^ name ^ ")"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | LParen -> "("
  | RParen -> ")"
  | End -> "end-of-input"

let rec skip_spaces text index =
  if index >= String.length text then index
  else
    let ch = text.[index] in
    if ch = ' ' || ch = '\n' || ch = '\t' || ch = '\r' then
      skip_spaces text (index + 1)
    else index

let rec consume_number_chars text index =
  if index >= String.length text then index
  else
    let ch = text.[index] in
    if is_digit ch || ch = '.' then consume_number_chars text (index + 1)
    else index

let rec consume_ident_chars text index =
  if index >= String.length text then index
  else
    let ch = text.[index] in
    if is_alphanumeric ch then consume_ident_chars text (index + 1) else index

let parse_float_literal literal =
  try Ok (float_of_string literal)
  with Failure _ -> Error ("invalid number literal: " ^ literal)

let tokenize text =
  let rec loop index acc =
    let index_no_space = skip_spaces text index in
    if index_no_space >= String.length text then Ok (List.rev (End :: acc))
    else
      let ch = text.[index_no_space] in
      match ch with
      | '+' -> loop (index_no_space + 1) (Plus :: acc)
      | '-' -> loop (index_no_space + 1) (Minus :: acc)
      | '*' -> loop (index_no_space + 1) (Star :: acc)
      | '/' -> loop (index_no_space + 1) (Slash :: acc)
      | '(' -> loop (index_no_space + 1) (LParen :: acc)
      | ')' -> loop (index_no_space + 1) (RParen :: acc)
      | _ ->
          if is_digit ch || ch = '.' then
            let next = consume_number_chars text index_no_space in
            let literal =
              String.sub text index_no_space (next - index_no_space)
            in
            match parse_float_literal literal with
            | Error message -> Error message
            | Ok number -> loop next (Number number :: acc)
          else if is_letter ch then
            let next = consume_ident_chars text index_no_space in
            let name = String.sub text index_no_space (next - index_no_space) in
            loop next (Ident name :: acc)
          else
            Error
              ("unexpected character '" ^ String.make 1 ch
             ^ "' while tokenizing expression")
  in
  loop 0 []

let current_token parser =
  if parser.index >= Array.length parser.tokens then End
  else parser.tokens.(parser.index)

let next_token parser =
  let next_index = parser.index + 1 in
  if next_index >= Array.length parser.tokens then End
  else parser.tokens.(next_index)

let advance parser =
  if parser.index < Array.length parser.tokens then
    parser.index <- parser.index + 1

let consume_expected parser expected =
  let actual = current_token parser in
  if actual = expected then (
    advance parser ;
    Ok ())
  else
    Error
      ("expected token " ^ token_name expected ^ " but found "
     ^ token_name actual)

let is_function_name name =
  name = "exp" || name = "log" || name = "sin" || name = "cos" || name = "tanh"

let build_function_call name argument =
  match name with
  | "exp" -> Ok (Exp argument)
  | "log" -> Ok (Log argument)
  | "sin" -> Ok (Sin argument)
  | "cos" -> Ok (Cos argument)
  | "tanh" -> Ok (Tanh argument)
  | _ -> Error ("unknown function: " ^ name)

let rec parse_expression parser =
  match parse_term parser with
  | Error message -> Error message
  | Ok first_term -> parse_expression_tail parser first_term

and parse_expression_tail parser left =
  match current_token parser with
  | Plus -> (
      advance parser ;
      match parse_term parser with
      | Error message -> Error message
      | Ok right -> parse_expression_tail parser (Add (left, right)))
  | Minus -> (
      advance parser ;
      match parse_term parser with
      | Error message -> Error message
      | Ok right -> parse_expression_tail parser (Sub (left, right)))
  | _ -> Ok left

and parse_term parser =
  match parse_unary parser with
  | Error message -> Error message
  | Ok first_factor -> parse_term_tail parser first_factor

and parse_term_tail parser left =
  match current_token parser with
  | Star -> (
      advance parser ;
      match parse_unary parser with
      | Error message -> Error message
      | Ok right -> parse_term_tail parser (Mul (left, right)))
  | Slash -> (
      advance parser ;
      match parse_unary parser with
      | Error message -> Error message
      | Ok right -> parse_term_tail parser (Div (left, right)))
  | _ -> Ok left

and parse_unary parser =
  match current_token parser with
  | Minus -> (
      advance parser ;
      match parse_unary parser with
      | Error message -> Error message
      | Ok expr -> Ok (Neg expr))
  | Ident name ->
      if is_function_name name && next_token parser = LParen then
        parse_function_call parser name
      else parse_primary parser
  | _ -> parse_primary parser

and parse_function_call parser name =
  advance parser ;
  match consume_expected parser LParen with
  | Error message -> Error message
  | Ok () -> (
      match parse_expression parser with
      | Error message -> Error message
      | Ok argument -> (
          match consume_expected parser RParen with
          | Error message -> Error message
          | Ok () -> build_function_call name argument))

and parse_primary parser =
  match current_token parser with
  | Number value ->
      advance parser ;
      Ok (Const value)
  | Ident name ->
      advance parser ;
      Ok (Var name)
  | LParen -> (
      advance parser ;
      match parse_expression parser with
      | Error message -> Error message
      | Ok expr -> (
          match consume_expected parser RParen with
          | Error message -> Error message
          | Ok () -> Ok expr))
  | token ->
      Error
        ("unexpected token " ^ token_name token
       ^ " while parsing primary expression")

let parse input =
  match tokenize input with
  | Error message -> Error message
  | Ok tokens -> (
      let parser = { tokens = Array.of_list tokens; index = 0 } in
      match parse_expression parser with
      | Error message -> Error message
      | Ok expr ->
          let trailing = current_token parser in
          if trailing = End then Ok expr
          else
            Error
              ("unexpected trailing token " ^ token_name trailing
             ^ " after expression"))

let rec to_string expr =
  match expr with
  | Const value -> string_of_float value
  | Var name -> name
  | Add (left, right) -> "(" ^ to_string left ^ " + " ^ to_string right ^ ")"
  | Sub (left, right) -> "(" ^ to_string left ^ " - " ^ to_string right ^ ")"
  | Mul (left, right) -> "(" ^ to_string left ^ " * " ^ to_string right ^ ")"
  | Div (left, right) -> "(" ^ to_string left ^ " / " ^ to_string right ^ ")"
  | Neg inner -> "(-" ^ to_string inner ^ ")"
  | Exp inner -> "exp(" ^ to_string inner ^ ")"
  | Log inner -> "log(" ^ to_string inner ^ ")"
  | Sin inner -> "sin(" ^ to_string inner ^ ")"
  | Cos inner -> "cos(" ^ to_string inner ^ ")"
  | Tanh inner -> "tanh(" ^ to_string inner ^ ")"

let vars expr =
  let seen = Hashtbl.create 16 in
  let rec collect expression acc =
    match expression with
    | Const _ -> acc
    | Var name ->
        if Hashtbl.mem seen name then acc
        else (
          Hashtbl.add seen name () ;
          name :: acc)
    | Add (left, right)
    | Sub (left, right)
    | Mul (left, right)
    | Div (left, right) ->
        let acc_after_left = collect left acc in
        collect right acc_after_left
    | Neg inner | Exp inner | Log inner | Sin inner | Cos inner | Tanh inner ->
        collect inner acc
  in
  List.rev (collect expr [])
