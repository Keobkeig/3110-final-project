type node = {
  value : float;
  mutable grad : float;
  parents : (node * float) list;
}

type build_state = {
  env : (string * float) list;
  var_nodes : (string, node) Hashtbl.t;
  mutable order : node list;
}

let make_node value parents = { value; grad = 0.0; parents }
let push_node state node = state.order <- node :: state.order
let make_leaf value = make_node value []

let lookup_env env name =
  try Ok (List.assoc name env)
  with Not_found -> Error ("missing variable assignment for " ^ name)

let get_or_create_var_node state name =
  match Hashtbl.find_opt state.var_nodes name with
  | Some existing -> Ok existing
  | None -> (
      match lookup_env state.env name with
      | Error message -> Error message
      | Ok value ->
          let node = make_leaf value in
          Hashtbl.add state.var_nodes name node ;
          Ok node)

let add_nodes state left right =
  let value = left.value +. right.value in
  let node = make_node value [ (left, 1.0); (right, 1.0) ] in
  push_node state node ;
  node

let sub_nodes state left right =
  let value = left.value -. right.value in
  let node = make_node value [ (left, 1.0); (right, -1.0) ] in
  push_node state node ;
  node

let mul_nodes state left right =
  let value = left.value *. right.value in
  let node = make_node value [ (left, right.value); (right, left.value) ] in
  push_node state node ;
  node

let div_nodes state left right =
  if right.value = 0.0 then Error "division by zero"
  else
    let value = left.value /. right.value in
    let dleft = 1.0 /. right.value in
    let dright = -.left.value /. (right.value *. right.value) in
    let node = make_node value [ (left, dleft); (right, dright) ] in
    push_node state node ;
    Ok node

let neg_node state inner =
  let value = -.inner.value in
  let node = make_node value [ (inner, -1.0) ] in
  push_node state node ;
  node

let exp_node state inner =
  let value = exp inner.value in
  let node = make_node value [ (inner, value) ] in
  push_node state node ;
  node

let log_node state inner =
  if inner.value <= 0.0 then Error "log undefined for non-positive input"
  else
    let value = log inner.value in
    let derivative = 1.0 /. inner.value in
    let node = make_node value [ (inner, derivative) ] in
    push_node state node ;
    Ok node

let sin_node state inner =
  let value = sin inner.value in
  let derivative = cos inner.value in
  let node = make_node value [ (inner, derivative) ] in
  push_node state node ;
  node

let cos_node state inner =
  let value = cos inner.value in
  let derivative = -.sin inner.value in
  let node = make_node value [ (inner, derivative) ] in
  push_node state node ;
  node

let tanh_node state inner =
  let value = tanh inner.value in
  let derivative = 1.0 -. (value *. value) in
  let node = make_node value [ (inner, derivative) ] in
  push_node state node ;
  node

let rec build_graph state expr =
  match expr with
  | Expr.Const value -> Ok (make_leaf value)
  | Expr.Var name -> get_or_create_var_node state name
  | Expr.Add (left, right) -> build_binary state left right add_nodes
  | Expr.Sub (left, right) -> build_binary state left right sub_nodes
  | Expr.Mul (left, right) -> build_binary state left right mul_nodes
  | Expr.Div (left, right) -> build_binary_result state left right div_nodes
  | Expr.Neg inner -> build_unary state inner neg_node
  | Expr.Exp inner -> build_unary state inner exp_node
  | Expr.Log inner -> build_unary_result state inner log_node
  | Expr.Sin inner -> build_unary state inner sin_node
  | Expr.Cos inner -> build_unary state inner cos_node
  | Expr.Tanh inner -> build_unary state inner tanh_node

and build_binary state left_expr right_expr make_op =
  match build_graph state left_expr with
  | Error message -> Error message
  | Ok left_node -> (
      match build_graph state right_expr with
      | Error message -> Error message
      | Ok right_node -> Ok (make_op state left_node right_node))

and build_binary_result state left_expr right_expr make_op =
  match build_graph state left_expr with
  | Error message -> Error message
  | Ok left_node -> (
      match build_graph state right_expr with
      | Error message -> Error message
      | Ok right_node -> make_op state left_node right_node)

and build_unary state inner_expr make_op =
  match build_graph state inner_expr with
  | Error message -> Error message
  | Ok inner_node -> Ok (make_op state inner_node)

and build_unary_result state inner_expr make_op =
  match build_graph state inner_expr with
  | Error message -> Error message
  | Ok inner_node -> make_op state inner_node

let rec propagate_parents parents upstream =
  match parents with
  | [] -> ()
  | (parent, local_derivative) :: rest ->
      parent.grad <- parent.grad +. (upstream *. local_derivative) ;
      propagate_parents rest upstream

let rec backward_pass nodes =
  match nodes with
  | [] -> ()
  | node :: rest ->
      propagate_parents node.parents node.grad ;
      backward_pass rest

let rec gradients_for_vars vars var_nodes acc =
  match vars with
  | [] -> List.rev acc
  | name :: rest ->
      let gradient_value =
        match Hashtbl.find_opt var_nodes name with
        | None -> 0.0
        | Some node -> node.grad
      in
      gradients_for_vars rest var_nodes ((name, gradient_value) :: acc)

let build_state env = { env; var_nodes = Hashtbl.create 16; order = [] }

let eval expr env =
  let state = build_state env in
  match build_graph state expr with
  | Error message -> Error message
  | Ok output -> Ok output.value

let gradient expr env =
  let state = build_state env in
  match build_graph state expr with
  | Error message -> Error message
  | Ok output ->
      output.grad <- 1.0 ;
      backward_pass state.order ;
      let var_names = Expr.vars expr in
      let gradients = gradients_for_vars var_names state.var_nodes [] in
      Ok (output.value, gradients)
