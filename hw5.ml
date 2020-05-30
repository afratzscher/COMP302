(* Q1 Polynomials TODO: Implement the following four functions *)

let multiplyPolyByTerm (Term(c,e), Poly p) = 
  let rec multTermHelper((c, e), p) = 
    match p with
    | [] -> []
    | (coeff, exp)::xs -> if (c = 0.0) then [(0.0, 0)]
        else ((c *. coeff), (e + exp))::(multTermHelper((c, e), xs))
  in Poly (multTermHelper ((c, e), p))
;;

let addTermToPoly (Term(c,e), Poly p) =
  let rec addTermHelper((c, e), p) = 
    match p with 
    | [] -> if (c = 0.0 && e == 0) then p
        else p@[(c,e)]
    | (coeff, exp)::(coeff2, exp2)::xs -> 
        if (c = 0.0 && e == 0) then p
        else if(e > exp) then (c, e)::p
        else if (exp == e) then ((c +. coeff), e)::(coeff2, exp2)::xs
        else if (e < exp && e > exp2) then (coeff, exp)::(c,e)::(coeff2,exp2)::xs 
        else if (exp2 == e) then (coeff, exp)::((c +. coeff2), e)::xs 
        else (coeff, exp)::(coeff2, exp2)::(addTermHelper((c,e), xs)) 
    | (coeff, exp)::[] -> 
        if (c = 0.0 && e == 0) then [(coeff, exp)]
        else if(exp == e) then [((c +. coeff), e)]
        else if ( e > exp) then (c,e)::[(coeff, exp)]
        else (coeff, exp)::[(c,e)]
  in Poly (addTermHelper((c, e), p))
;;

let rec addPolys (Poly p1, Poly p2) = 
  match p1,p2 with
  | ([],[]) -> Poly []
  | (p1, []) -> Poly p1
  | ([], p2) -> Poly p2
  | (x::xs, _) -> addPolys(Poly xs, (addTermToPoly(Term (fst x, snd x), Poly p2))) 
;;

let rec multPolys (Poly p1, Poly p2) =
  match p1, p2 with
  | ([], []) -> Poly []
  | (p1, []) -> Poly []
  | ([], p2) -> Poly []
  | (x::xs, _) -> addPolys(multiplyPolyByTerm(Term(fst x, snd x), Poly p2), multPolys(Poly xs, Poly p2))
;;

(* Q2 References TODO: implement the `insert` function *)

let rec insert comp (item: int) (list: rlist) = 
  match !list with
  | None -> list:= Some {data = item; next = ref None} (* if nothing, make new cell *)
  | Some x -> if comp(item, x.data) then list := Some {data = item; next = ref (Some x)}
      else insert comp item x.next
;;
