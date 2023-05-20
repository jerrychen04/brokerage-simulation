open Unix
open Account

exception NoSuchStock of string
exception NotOwned of string
exception Broke

let update_avg (a1, f1) (a2, f2) =
  ((a1 *. f1) +. (a2 *. f2)) /. Float.abs (f1 -. f2)

let order lst =
  let sorted_lst =
    List.sort
      (fun ({ Account.ticker = s1; Account.price = a1 }, f1)
           ({ Account.ticker = s2; Account.price = a2 }, f2) ->
        String.compare s1 s2)
      lst
  in
  let rec combine_entries acc lst =
    match lst with
    | [] -> List.rev acc
    | [ ({ Account.ticker = s1; Account.price = a1 }, f1) ] ->
        List.rev (({ Account.ticker = s1; Account.price = a1 }, f1) :: acc)
    | ({ Account.ticker = s1; Account.price = a1 }, f1)
      :: ({ Account.ticker = s2; Account.price = a2 }, f2)
      :: tl ->
        if s1 = s2 then
          combine_entries
            (( {
                 Account.ticker = s1;
                 Account.price = update_avg (a1, f1) (a2, f2);
               },
               if f2 -. f1 < 0. then raise Broke else f2 -. f1 )
            :: acc)
            tl
        else
          combine_entries
            (({ Account.ticker = s1; Account.price = a1 }, f1) :: acc)
            (({ Account.ticker = s2; Account.price = a2 }, f2) :: tl)
  in
  combine_entries [] sorted_lst

(** Private helper function. [contains_ticker lst s] checks if s is in lst*)
let rec contains_ticker lst s =
  match lst with
  | [] -> false
  | h :: tl -> if h = s then true else contains_ticker tl s

(** Private helper function. Converts the time into an easy-to-read
    month/day/year string format *)
let convert_unix_time (t : float) : string =
  let tm = localtime t in
  let day = string_of_int tm.tm_mday in
  let month = string_of_int (tm.tm_mon + 1) in
  let year = string_of_int (tm.tm_year + 1900) in
  month ^ "/" ^ day ^ "/" ^ year

(** Private helper function. Finds the original price of [stock] in portfolio
    [port]*)
let rec orig_price stock port =
  match port with
  | [] -> raise (NotOwned stock)
  | (tick, price) :: t ->
      if String.uppercase_ascii tick.ticker = String.uppercase_ascii stock then
        tick.price
      else orig_price stock t

let sell shares ticker acc =
  try
    let price = Stocks.get_ticker_price ticker in
    if contains_ticker (Account.only_stocks acc) ticker then
      let liquid = (Account.deposit (shares *. price) acc).cash_balance in
      ( {
          Account.stock_balance = acc.stock_balance -. (shares *. price);
          Account.cash_balance = liquid;
          Account.portfolio =
            order
              (({ Account.ticker; Account.price = 0. -. price }, shares)
              :: acc.portfolio);
          Account.transaction_log =
            Log.add
              {
                time = convert_unix_time (time ());
                type_of_transaction = "Sell";
                share = shares;
                stock = { ticker; price };
              }
              acc.transaction_log;
          Account.watchlist = acc.watchlist;
          dep_with_log = acc.dep_with_log;
        },
        shares *. (price -. orig_price ticker acc.portfolio) )
    else raise (NotOwned ticker)
  with
  | Broke -> raise Broke
  | NotOwned ticker -> raise (NotOwned ticker)
  | exc -> raise (NoSuchStock ticker)
