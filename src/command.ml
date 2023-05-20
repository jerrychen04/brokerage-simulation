type command =
  | Bal
  | Cash
  | Equity
  | Portfolio
  | Dep of float
  | Withdraw of float
  | Buy of (string * float)
  | Sell of (string * float)
  | View of string
  | ViewOption of string
  | Watchlist
  | WatchlistAdd of string
  | WatchlistRemove of string
  | Cashflow
  | Quit
  | History
  | Help
  | OptionsTickerHelp

exception Invalid
exception Empty

(** Private helper function to see if a string is a float. *)
let is_float s =
  try
    ignore (float_of_string s);
    true
  with Failure _ -> false

(** Private helper function to see if a string is not empty. *)
let is_not_empty_string s = String.length s > 0

let parse str =
  let split_str =
    List.filter is_not_empty_string
      (List.map String.trim (String.split_on_char ' ' str))
  in
  match split_str with
  | [ "-bal" ] -> Bal
  | [ "-cash" ] -> Cash
  | [ "-equity" ] -> Equity
  | [ "-portfolio" ] -> Portfolio
  | "-dep" :: [ amt ] ->
      if is_float amt then Dep (float_of_string amt) else raise Invalid
  | "-withdraw" :: [ amt ] ->
      if is_float amt then Withdraw (float_of_string amt) else raise Invalid
  | "-buy" :: ticker :: [ num_shares ] ->
      if is_float num_shares then
        Buy (String.uppercase_ascii ticker, float_of_string num_shares)
      else raise Invalid
  | "-sell" :: ticker :: [ num_shares ] ->
      if is_float num_shares then
        Sell (String.uppercase_ascii ticker, float_of_string num_shares)
      else raise Invalid
  | [ "-view"; ticker ] -> View ticker
  | [ "-view_option"; ticker ] -> ViewOption ticker
  | [ "-watchlist" ] -> Watchlist
  | [ "-watchlist"; "add"; ticker ] -> WatchlistAdd ticker
  | [ "-watchlist"; "remove"; ticker ] -> WatchlistRemove ticker
  | [ "-cashflow" ] -> Cashflow
  | [ "-quit" ] -> Quit
  | [ "-history" ] -> History
  | [ "-help" ] -> Help
  | [ "-options_ticker_help" ] -> OptionsTickerHelp
  | [] -> raise Empty
  | _ -> raise Invalid
