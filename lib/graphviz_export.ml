type dot_graph_builder = {
  mutable counter : int;
  mutable nodes : (int * string) list;
  mutable edges : (int * int) list;
  vars : (string, int) Hashtbl.t;
}

let make_graph () =
  { counter = 0; nodes = []; edges = []; vars = Hashtbl.create 16 }

let generate_id builder =
  let id = builder.counter in
  builder.counter <- builder.counter + 1 ;
  id

let rec to_graph builder expr =
  let open Expr in
  let label = to_string expr in
  match expr with
  | Const _ ->
      let id = generate_id builder in
      builder.nodes <- (id, label) :: builder.nodes ;
      id
  | Var s -> (
      match Hashtbl.find_opt builder.vars s with
      | Some id -> id
      | None ->
          let id = generate_id builder in
          builder.nodes <- (id, s) :: builder.nodes ;
          Hashtbl.add builder.vars s id ;
          id)
  (* binary ops *)
  | Add (l, r) | Sub (l, r) | Mul (l, r) | Div (l, r) ->
      let id = generate_id builder in
      builder.nodes <- (id, label) :: builder.nodes ;
      let l_id = to_graph builder l in
      let r_id = to_graph builder r in
      builder.edges <- (l_id, id) :: (r_id, id) :: builder.edges ;
      id
  (* unary ops *)
  | Neg e | Exp e | Log e | Sin e | Cos e | Tanh e ->
      let id = generate_id builder in
      builder.nodes <- (id, label) :: builder.nodes ;
      let e_id = to_graph builder e in
      builder.edges <- (e_id, id) :: builder.edges ;
      id

let expr_to_dot _expr =
  let builder = make_graph () in
  let _ = to_graph builder _expr in
  let buf = Buffer.create 64 in
  Buffer.add_string buf "digraph {\n rankdir=\"LR\"\n" ;
  List.iter
    (fun (id, label) ->
      Buffer.add_string buf (Printf.sprintf "  %d [label=\"%s\"]\n" id label))
    builder.nodes ;
  List.iter
    (fun (src, dst) ->
      Buffer.add_string buf (Printf.sprintf "  %d -> %d\n" src dst))
    builder.edges ;
  Buffer.add_string buf "}\n" ;
  Buffer.contents buf

let write_expr_dot path expr =
  let dot = expr_to_dot expr in
  try
    let channel = open_out path in
    output_string channel dot ;
    close_out channel ;
    Ok ()
  with Sys_error message -> Error message
