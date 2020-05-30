(* Question 1 *)

let mapTree_tests =
  [
    (((fun x -> x+1), Node(Empty, 1, Empty)), Node(Empty, 2, Empty));
    (((fun x -> x+1), Empty), Empty);
    (((fun x -> x+1), Node(Empty, 0, (Node(Empty, 1, Empty)))), Node(Empty, 1, (Node(Empty, 2, Empty))));
    (((fun x -> x+1), Node((Node(Empty, 1, Empty)), 2, Empty)), Node((Node(Empty, 2, Empty)), 3, Empty));
 
  ]

let rec mapTree (f, (t: 'a tree)) =
  match t with
  | Empty -> Empty
  | Node(l, v, r) -> Node(mapTree(f, l),(f v), mapTree(f, r))
;;

(* Question 2. *)

let halfint_tests =
  [
    (((fun x -> x+. 1.), 10.0, -10.0, 0.001), -1.0009765625); 
  ]

let rec halfint ((f: float -> float), (posValue : float), (negValue : float), (epsilon : float)) = 
  let mid = (posValue +. negValue) /. 2.0 in
  if abs_float(f mid) < epsilon then mid
  else if f mid > 0.0 then halfint(f, mid, negValue, epsilon)
  else halfint(f, posValue, mid, epsilon);
  
;;

(* Question 3. *)

let newton_tests =
  [ 
    ((sin, 5.0, 0.0001, 0.001), 9.42477);
  ]

let rec newton ((f: float -> float),  (guess:float), (epsilon:float), (dx:float)) =
  let close((x:float), (y:float), (epsilon:float)) = abs_float(x-.y) < epsilon in
  let improve((guess:float),f,(dx:float)) = raise NotImplemented in
  if close((f guess), 0.0, epsilon)
  then
    guess
  else
    let n = guess -. ((f guess) /. ((deriv (f, dx)) guess)) in
    if abs_float(n) < epsilon then n
    else newton(f, n, epsilon, dx)
;;

(* Question 4. *)

let indIntegral (f, (dx:float)) = 
  fun x -> integral(f, 0.0, x, dx)
;;
