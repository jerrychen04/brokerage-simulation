type t = Account.transaction list

let create = []
let add tran prev_log = tran :: prev_log

let to_string my_log =
  if List.length my_log = 0 then "[]"
  else
    let rec to_string_helper my_log_1 =
      match my_log_1 with
      | [] -> ""
      | h :: t -> (
          match Account.transaction_to_string_quint h with
          | a, b, c, d, e ->
              "Date: " ^ a ^ "; Action: " ^ b ^ "; Shares: " ^ c ^ "; Stock: "
              ^ d ^ "; Price: " ^ e ^ " \n " ^ to_string_helper t)
    in

    let cur_str = to_string_helper my_log in

    "[" ^ String.sub cur_str 0 (String.length cur_str - 3) ^ "]"
