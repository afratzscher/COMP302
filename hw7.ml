(* Question 1.1 *)
let rec occurCheck (v: char) (tau: typExp) : bool =
  match tau with
  | TypInt -> false
  | TypVar a -> 
      if v = a then true
      else false
  | Arrow(t1, t2) ->
      if occurCheck v t1 = false then
        (if occurCheck v t2 then true
         else false)
      else true
  | Lst t -> if occurCheck v t then true
      else false
;;

(* Question 1.2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with
  | TypInt -> TypInt
  | TypVar a -> if v = a then tau1 
      else TypVar a
  | Arrow(t1, t2) -> Arrow(substitute tau1 v t1, substitute tau1 v t2)
  | Lst t -> Lst (substitute tau1 v t)
        
;;

(* Question 1.3 *)
let applySubst (sigma: substitution) (tau: typExp) : typExp =
  List.fold_right(fun (v,exp) -> fun tau -> substitute exp v tau) sigma tau
;;

(* Question 2 *)
let rec unify (tau1: typExp) (tau2:typExp) : substitution = 
  match tau1, tau2 with
  | TypInt, TypInt -> []
  | TypVar c1, b -> if (TypVar c1 = b) then []
      else if occurCheck c1 b then failwith "Error"
      else[(c1, b)]
  | b, TypVar c2 -> if (TypVar c2 = b) then []
      else if occurCheck c2 b then failwith "Error"
      else [(c2, b)]
  |Arrow(ta1, tb1), Arrow(ta2, tb2) ->
      let b = (unify ta1 ta2) in
      let a1 = applySubst b tb1 in
      let a2 = applySubst b tb2 in
      (unify a1 a2) @ b
  | (Lst t1), (Lst t2) -> unify t1 t2
  | _ -> failwith "Error"
;;