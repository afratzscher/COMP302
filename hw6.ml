(* Question 1 *)

let makeProtectedAccount ((openingBalance: int), (password: string)) =
  let pwd = ref password in 
  let balance = ref openingBalance in 
  let account_closed = ref false in
  fun((checkpassword:string), (t:transaction))->
    if !account_closed = true then Printf.printf "Account closed."
    else match checkpassword with
      | password -> if password <> !pwd then print_string "Incorrect password."
          else match t with 
            | Withdraw m -> if(!balance >= m) then 
                  ((balance := !balance-m);
                   (Printf.printf "The new balance is: %i." !balance))
                else (Printf.printf ("Insufficient funds."))
            | Deposit m -> 
                ((balance := !balance + m);
                 (Printf.printf "The new balance is: %i." !balance))
            | CheckBalance -> (Printf.printf "The balance is: %i." !balance)
            | Close -> ((account_closed := true);
                        (Printf.printf "Account successfully closed."))
            | ChangePassword pass -> 
                ((pwd := pass);
                 Printf.printf "Password changed.")
;;