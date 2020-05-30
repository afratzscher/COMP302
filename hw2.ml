(* GRADE:  100% *)
(* Q1a TODO: Write your own tests for the pairlists function.
         You should NOT test lists of different lengths.
*)
let pairlists_tests = [
  (* Your test cases go here. *)
  (([],[]),[]);
  (([1;2], [3;4]), [(1,3); (2,4)]);
]

(* Q1a TODO: Implement pairlists. *)
let rec pairlists (l1, l2) =
  match l1 with
  | [] -> []
  | hd::tl -> (hd, List.hd l2):: pairlists(tl, List.tl l2)
;;

(* Q1b TODO: Write your own tests for the w_mean function.
         You should NOT test lists of different lengths.
*)
let w_mean_tests = [
  (* Your test cases go here. *)
  (([1.0; 1.0], [1.0; 1.0]), 1.0);
  (([1.0; 1.0], [0.0; 1.0]), 0.5);
  (([1.0; 1.0], [-1.0; 1.0]), 0.0);
]

(* Q1b TODO: Implement w_mean. *)
let w_mean weights data =
  let x = pairlists(weights, data) in
  let y = List.map(fun n -> (fst n *. snd n)) x in
  (sumlist y) /. (sumlist weights)
;;

(* Q2 TODO: Write your own tests for the memberof function. *)
let memberof_tests = [
  (* Your test cases go here. *)
  ((3, [1;6;3;2]),true);
  ((0, [1;2]), false);
  ((0, []), false);
]

(* Q2 TODO: Implement memberof. *)
let rec memberof (item, lst) =
  match lst with
  | [] -> false
  | x::xs -> if x=item then true
      else memberof(item, xs)
;;

(* Q2 TODO: Write your own tests for the remove function. *)
let remove_tests = [
  (* Your test cases go here. *)
  ((1, [1;2;3]), [2;3]);
  ((1,[1;1;2]), [2]);
  ((0, [0]), []);
  ((0, []), []);
  ((2, [1;3]), [1;3]);
]

(* Q2 TODO: Implement remove. *)
let rec remove (item, lst) =
  match lst with
  | [] -> []
  | x::xs -> if x==item then remove(item, xs)
      else x::remove(item, xs)
;;

(* Q3 TODO: Write your own tests for the find_max function. *)
let find_max_tests = [
  (* Your test cases go here. *)
  ([1;6;3;2;6;1;7;2;3;5], 7);
  ([1;1], 1);
  ([1], 1);
  ([3;2;1], 3);
  ([1;2;3], 3);
  ([-1;-2;-3; 1], 1);
]

(* Q3 TODO: Implement find_max. *)
let find_max l =
  let rec helper(lst, max)=
    match lst with
    | [] -> max
    | x::xs -> if x > max then helper(xs, x)
        else helper(xs, max)
  in 
  helper(l, List.hd l)
;;

(* Q4 TODO: Write your own tests for the selsort function. *)
let selsort_tests = [
  (* Your test cases go here. *)
  ([1;2;3], [3;2;1]);
  ([], []);
  ([3;2;1], [3;2;1]);
  ([3;1;2], [3;2;1]);
  ([-1], [-1]);
]

(* Q4 TODO: Implement selsort. *)
let rec selsort l = 
  match l with
  | [] -> []
  | x::xs  -> find_max(x::xs)::selsort(remove(find_max(x::xs), x::xs));;

