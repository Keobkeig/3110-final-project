type dual = { primal : float; tangent : float }

let make primal tangent = { primal; tangent }
let constant x = make x 0.0
let variable x = make x 1.0
let value d = d.primal
let derivative d = d.tangent

let add left right =
  make (left.primal +. right.primal) (left.tangent +. right.tangent)

let sub left right =
  make (left.primal -. right.primal) (left.tangent -. right.tangent)

let mul left right =
  let primal = left.primal *. right.primal in
  let tangent =
    (left.tangent *. right.primal) +. (left.primal *. right.tangent)
  in
  make primal tangent

let div left right =
  if right.primal = 0.0 then failwith "division by zero in forward_ad.div"
  else
    let primal = left.primal /. right.primal in
    let numerator =
      (left.tangent *. right.primal) -. (left.primal *. right.tangent)
    in
    let denominator = right.primal *. right.primal in
    make primal (numerator /. denominator)

let neg inner = make (-.inner.primal) (-.inner.tangent)

let exp inner =
  let primal = Stdlib.exp inner.primal in
  make primal (primal *. inner.tangent)

let log inner =
  if inner.primal <= 0.0 then failwith "log domain error in forward_ad.log"
  else make (Stdlib.log inner.primal) (inner.tangent /. inner.primal)

let sin inner =
  make (Stdlib.sin inner.primal) (Stdlib.cos inner.primal *. inner.tangent)

let cos inner =
  make (Stdlib.cos inner.primal) (-.Stdlib.sin inner.primal *. inner.tangent)

let tanh inner =
  let primal = Stdlib.tanh inner.primal in
  make primal ((1.0 -. (primal *. primal)) *. inner.tangent)

let derivative_univariate f x =
  let output = f (variable x) in
  output.tangent

let gradient_expr _expr _env =
  Error
    "forward-mode expression gradient is not implemented yet; see Sprint-B TODO"
