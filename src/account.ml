open Unix
open Stocks

exception Broke
exception OverMemory
exception StockNotFound

type stock = {
  ticker : string;
  price : float;
}

type account = {
  stock_balance : float;
  cash_balance : float;
  portfolio : (stock * float) list;
  transaction_log : transaction list;
  watchlist : (stock * float) list;
  dep_with_log : dep_with list;
}

and transaction = {
  time : string;
  type_of_transaction : string;
  share : float;
  stock : stock;
}

and dep_with = {
  ctime : string;
  type_of : string;
  amount : float;
  prev_balance : float;
}

let create = []

let cash_to_string_quad cash =
  ( cash.ctime,
    cash.type_of,
    string_of_float cash.amount,
    string_of_float cash.prev_balance )

let dep_with_string log =
  if List.length log = 0 then "[]"
  else
    let rec to_string_helper my_log_1 =
      match my_log_1 with
      | [] -> ""
      | h :: t -> (
          match cash_to_string_quad h with
          | a, b, c, d ->
              "Date: " ^ a ^ "; Type: " ^ b ^ "; Amount: " ^ c
              ^ "; Previous balance: " ^ d ^ " \n " ^ to_string_helper t)
    in

    let cur_str = to_string_helper log in

    "[" ^ String.sub cur_str 0 (String.length cur_str - 3) ^ "]"

(** Private helper function. Converts the time into an easy-to-read
    month/day/year string format*)
let convert_unix_time (t : float) : string =
  let tm = localtime t in
  let day = string_of_int tm.tm_mday in
  let month = string_of_int (tm.tm_mon + 1) in
  let year = string_of_int (tm.tm_year + 1900) in
  month ^ "/" ^ day ^ "/" ^ year

let stock_to_string_pair stk = (stk.ticker, string_of_float stk.price)

let transaction_to_string_quint trans =
  let pp = stock_to_string_pair trans.stock in
  match pp with
  | a, b ->
      let tick = a in
      let pr = b in
      ( trans.time,
        trans.type_of_transaction,
        string_of_float trans.share,
        tick,
        pr )

let withdraw (amt : float) (acc : account) =
  let b1 = acc.stock_balance in
  let b2 = acc.cash_balance -. amt in
  let b2' = acc.cash_balance in
  if amt > acc.cash_balance then raise Broke
  else
    {
      stock_balance = b1;
      cash_balance = b2;
      portfolio = acc.portfolio;
      transaction_log = acc.transaction_log;
      watchlist = acc.watchlist;
      dep_with_log =
        (let dw1 =
           {
             ctime = convert_unix_time (time ());
             type_of = "Withdrawal";
             amount = amt;
             prev_balance = b1 +. b2';
           }
         in
         dw1 :: acc.dep_with_log);
    }

let deposit (amt : float) (acc : account) =
  let b1 = acc.stock_balance in
  let b2 = acc.cash_balance +. amt in
  let b2' = acc.cash_balance in
  {
    stock_balance = b1;
    cash_balance = b2;
    portfolio = acc.portfolio;
    transaction_log = acc.transaction_log;
    watchlist = acc.watchlist;
    dep_with_log =
      (let dw1 =
         {
           ctime = convert_unix_time (time ());
           type_of = "Deposit";
           amount = amt;
           prev_balance = b1 +. b2';
         }
       in
       dw1 :: acc.dep_with_log);
  }

(** Private helper function. Calculates the current value of the portfolio
    [port]. *)
let rec find_stock_balance port =
  match port with
  | [] -> 0.
  | (stk, num) :: t ->
      (Stocks.get_ticker_price stk.ticker *. num) +. find_stock_balance t

let balance acc = acc.stock_balance +. acc.cash_balance
let stock_balance acc = find_stock_balance acc.portfolio
let cash_balance acc = acc.cash_balance

let rec ret_portfolio (port : (stock * float) list) =
  match port with
  | [] -> []
  | (h, q) :: t -> (h.ticker, h.price, q) :: ret_portfolio t

let port_to_string port =
  let full_port = ret_portfolio port in
  let rec to_string = function
    | [] -> ""
    | (ticker, price, q) :: t ->
        "(ticker = " ^ ticker ^ ", " ^ "average purchase price = "
        ^ string_of_float price ^ ", " ^ "number of shares = "
        ^ string_of_float q ^ ") " ^ "\n" ^ to_string t
  in
  let list_text = to_string full_port in
  "{ " ^ list_text ^ "}"

let only_stocks (acc : account) =
  List.map (fun ({ ticker; price }, _) -> ticker) acc.portfolio

let add_watchlist ticker acc =
  try
    let price2 = Stocks.get_ticker_price ticker in
    let rec check watchlist tic =
      match watchlist with
      | [] -> [ ({ ticker = tic; price = price2 }, price2) ]
      | ({ ticker = t1; price = p1 }, p2) :: t ->
          if t1 = tic then
            let new_stock = { ticker = t1; price = price2 } in
            (new_stock, price2) :: t
          else ({ ticker = t1; price = p1 }, p2) :: check t tic
    in

    let new_watch = check acc.watchlist ticker in
    {
      stock_balance = acc.stock_balance;
      cash_balance = acc.cash_balance;
      portfolio = acc.portfolio;
      transaction_log = acc.transaction_log;
      watchlist = new_watch;
      dep_with_log = acc.dep_with_log;
    }
  with exc -> raise (NoSuchStock ticker)

let remove_watchlist ticker acc =
  try
    let _ = Stocks.get_ticker_price ticker in
    let old_length = List.length acc.watchlist in
    let rec check watchlist tic =
      match watchlist with
      | [] -> []
      | ({ ticker = t1; price = p1 }, p2) :: t ->
          if t1 = tic then t
          else ({ ticker = t1; price = p1 }, p2) :: check t tic
    in

    let new_watch = check acc.watchlist ticker in
    if List.length new_watch = old_length then raise StockNotFound
    else
      {
        stock_balance = acc.stock_balance;
        cash_balance = acc.cash_balance;
        portfolio = acc.portfolio;
        transaction_log = acc.transaction_log;
        watchlist = new_watch;
        dep_with_log = acc.dep_with_log;
      }
  with NoSuchStock ticker -> raise (NoSuchStock ticker)

let watch_to_string list =
  let rec to_string = function
    | [] -> ""
    | ({ ticker = t; price = p }, p2) :: t2 ->
        let new_price = Stocks.get_ticker_price t in
        "(ticker = " ^ t ^ ", " ^ "price added at = " ^ string_of_float p ^ ", "
        ^ "current price = " ^ string_of_float new_price ^ ") " ^ "\n"
        ^ to_string t2
  in
  let list_text = to_string list in
  "{ " ^ list_text ^ "}"

let rec find_portfolio_stock ticker (port : (stock * float) list) =
  match port with
  | [] -> None
  | ({ ticker = t; price = _ }, num) :: _ when t = ticker -> Some num
  | _ :: tl -> find_portfolio_stock ticker tl

let rec update_portfolio_stock ticker num (port : (stock * float) list) =
  match port with
  | [] -> []
  | ({ ticker = t; price = p }, _) :: tl when t = ticker ->
      ({ ticker = t; price = p }, num) :: tl
  | head :: tl -> head :: update_portfolio_stock ticker num tl

let buy (shares : float) (ticker : string) (acc : account) =
  let price = Stocks.get_ticker_price ticker in
  let cost = price *. shares in
  if cost > acc.cash_balance then raise Broke
  else
    let new_cash_balance = acc.cash_balance -. cost in
    let stock_in_portfolio = find_portfolio_stock ticker acc.portfolio in
    let new_portfolio =
      match stock_in_portfolio with
      | None -> ({ ticker; price }, shares) :: acc.portfolio
      | Some num -> update_portfolio_stock ticker (num +. shares) acc.portfolio
    in
    {
      stock_balance = find_stock_balance new_portfolio;
      cash_balance = new_cash_balance;
      portfolio = new_portfolio;
      transaction_log =
        {
          time = convert_unix_time (time ());
          type_of_transaction = "Buy";
          share = shares;
          stock = { ticker; price };
        }
        :: acc.transaction_log;
      watchlist = acc.watchlist;
      dep_with_log = acc.dep_with_log;
    }

let sell (shares : float) (ticker : string) (acc : account) =
  let stock_in_portfolio = find_portfolio_stock ticker acc.portfolio in
  match stock_in_portfolio with
  | None -> raise StockNotFound
  | Some num when num < shares -> raise Broke
  | Some num ->
      let price = Stocks.get_ticker_price ticker in
      let new_cash_balance = acc.cash_balance +. (price *. shares) in
      let new_portfolio =
        if num = shares then
          List.filter
            (fun ({ ticker = t; price = _ }, _) -> t <> ticker)
            acc.portfolio
        else update_portfolio_stock ticker (num -. shares) acc.portfolio
      in
      {
        stock_balance = find_stock_balance new_portfolio;
        cash_balance = new_cash_balance;
        portfolio = new_portfolio;
        transaction_log =
          {
            time = convert_unix_time (time ());
            type_of_transaction = "Sell";
            share = shares;
            stock = { ticker; price };
          }
          :: acc.transaction_log;
        watchlist = acc.watchlist;
        dep_with_log = acc.dep_with_log;
      }

let portfolio_value acc =
  let rec helper = function
    | [] -> 0.
    | ({ ticker = t; price = _ }, num) :: tl ->
        (num *. Stocks.get_ticker_price t) +. helper tl
  in
  helper acc.portfolio

let is_stock_in_portfolio ticker acc =
  match find_portfolio_stock ticker acc.portfolio with
  | None -> false
  | Some _ -> true

let check_watchlist (acc : account) =
  let rec helper = function
    | [] -> ()
    | ({ ticker = t; price = p }, _) :: tl ->
        let new_price = Stocks.get_ticker_price t in
        if new_price < p then
          print_endline
            ("Stock " ^ t
           ^ " has fallen below your watch price. Current price: "
           ^ string_of_float new_price);
        helper tl
  in
  helper acc.watchlist

let transaction_log_to_string (log : transaction list) =
  let rec to_string = function
    | [] -> ""
    | {
        time = t;
        type_of_transaction = tt;
        share = s;
        stock = { ticker = tick; price = p };
      }
      :: tl ->
        "(" ^ t ^ ", " ^ tt ^ ", " ^ string_of_float s ^ ", (" ^ tick ^ ", "
        ^ string_of_float p ^ "))\n" ^ to_string tl
  in
  let list_text = to_string log in
  "{ " ^ list_text ^ "}"

let print_account_details (acc : account) =
  let port_str = port_to_string acc.portfolio in
  let watch_str = watch_to_string acc.watchlist in
  let log_str = transaction_log_to_string acc.transaction_log in
  let cash_str = dep_with_string acc.dep_with_log in
  print_endline
    ("Account Details:\n\nCash Balance: "
    ^ string_of_float acc.cash_balance
    ^ "\nStock Balance: "
    ^ string_of_float acc.stock_balance
    ^ "\nTotal Balance: "
    ^ string_of_float (balance acc)
    ^ "\n\nPortfolio:\n" ^ port_str ^ "\nWatchlist:\n" ^ watch_str
    ^ "\nTransaction Log:\n" ^ log_str ^ "\nCash Transaction Log:\n" ^ cash_str
    ^ "\n")
