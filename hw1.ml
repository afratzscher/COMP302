(* Q1 TODO: Correct these tests for the double function. *)
let double_tests = [
  (0, 0);
  (1, 2);
  (3, 6);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let double = fun n -> n*2


(* Q1 TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (* Your test cases go here.
     Remember that the outputs of fact should be *floating-point* numbers.
  *)
  (0,1.0);
  (1,1.0);
  (3, 6.0);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let fact n =
  let rec helper(n,m) = 
    if n = 0 then float_of_int(m)
    else helper(n-1, n*m)
  in helper(n, 1)
         
(* Q2 TODO: Write your own tests for the mysqrt function.
         You should NOT test cases for n < 0.
*)
let mysqrt_tests = [
  (* Your test cases go here. *)
  (0.0, 0.0);
  (1.0, 1.0);
  (4.0, 2.0);
]

(* Q2 TODO: Implement mysqrt. *)
let mysqrt (x:float) = 
  let rec helper (g) =
    if close(x, g *. g) then g
    else helper((g +. (x/.g))/. 2.0)
  in 
  helper(x)

(* Q3 TODO: Write your own tests for the cube_root function.
            You should NOT test cases for n < 0.
*)
let cube_root_tests = [
  (* Your test cases go here. *)
  (0.0, 0.0);
  (1.0, 1.0);
  (8.0, 2.0);
]

(* Q3 TODO: Implement cube_root. *)
let cube_root (x:float) = 
  let rec helper(g) = 
    if close(x, (g*.g*.g)) then g
    else helper(((2.0 *. g) +. (x /. (g *. g))) /. 3.0)
  in helper(x)

(* Q4 TODO: Write your own tests for the fast_exp function.
            You should NOT test cases for negative bases or powers.
*)
let fast_exp_tests = [
  (* Your test cases go here. *)
  ((1,2), 1);
  ((2, 0), 1);
  ((0, 2), 0);
  ((2, 2), 4);
  ((2, 3), 8);
  ((2, 5), 32);
  ((3, 3), 27);
]

(* Q4 TODO: Implement tail recursive helper fast_exp_aux. *)
let rec fast_exp_aux (base, power, acc) = 
  if power = 0 then acc
  else if (power mod 2 = 1) then fast_exp_aux(base, power-1, acc*base)
  else fast_exp_aux (base*base, (power/2), acc)
  

(* Q4 TODO: Implement fast_exp using fast_exp_aux. *)
let fast_exp (base, power) = 
  if base = 0 then 0
  else if power = 0 then 1
  else fast_exp_aux(base, power, 1)