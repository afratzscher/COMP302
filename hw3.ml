(* GRADE:  100% *)
(* Question 1. *)

let common_tests = [
  (([], []), []);
  (([1;3;2;4;1;5;6;3], [33;9;8;2;11;21;3]), [3;2]);
  (([1], [2]), []);
  (([1], [1]), [1]);

] 

let rec helper(l1,acc) = 
  match l1 with
  | [] -> acc
  | x::xs -> if (List.mem x acc) then helper(xs, acc)
      else helper(xs, x::acc)
;;

let rec common (l1, l2) = 
  match helper(l1,[]) with
  | [] -> []
  | x::xs -> if (not (List.mem x xs) && List.mem x l2) then x::common(xs, l2) 
      else common(xs, l2)
;; 

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let split_tests = [
  ([1;3;2;4;5;6;9;11;17;13;12], ([1;2;5;9;17;12], [3;4;6;11;13])); 
  ([1;3;2;4;5;6;9;11;17;13], ([1;2;5;9;17], [3;4;6;11;13])); 
  ([], ([], []));
]

let split l =
  let rec split_helper list l1 l2 = 
    match list with
    | [] -> (l1, l2)
    | [x] -> ((l1@[x]), l2)
    | x0::x1::xs -> split_helper xs (l1@[x0]) (l2@[x1]) 
  in split_helper l [] []
;;

(* Question 3 Here you implement merge. *)

let merge_tests = [
  (([], []), []);
  (([1;3;5;7;9], [2;4;6;8]), [1;2;3;4;5;6;7;8;9]);
  (([1;2;3], []), [1;2;3]);
]

let rec merge (l1,l2) = 
  match l1, l2 with
  | (x, []) -> x
  | ([], y) -> y
  | (x::xs, y::ys) -> if x<=y then x::merge(xs, y::ys)
      else y::merge(x::xs, ys)
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  ([], []);
  ([10;2;8;5;1;4;3;9;7;6], [1;2;3;4;5;6;7;8;9;10]);
  ([1;3;2;4;1;2;5], [1;1;2;2;3;4;5]);
  ([1;2;3], [1;2;3]);
  ([3], [3]);
]

let rec mergesort l =
  match l with
  | [] -> []
  | (x::[]) -> x::[]
  | _ -> let (l1,l2) = split l in merge (mergesort l1, mergesort l2)
;;

