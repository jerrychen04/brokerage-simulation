open Brokerage
open Stocks
open Options
open Command
open Account
open Buy
open Sell

let print_tuple_list tuple_list =
  List.iter (fun (s, f1, f2) -> Printf.printf "%s: %f, %f\n" s f1 f2) tuple_list

let invalid_msg () =
  print_endline
    "Please enter in a valid prompt! Type -help to view all commands"
(* terms lets the user decide on whether or not they agree to the terms and
   conditions in order to keep using our services*)

(** Gets the first element of a pair. *)
let fst (a, b) = a

(** Gets the second element of a pair. *)
let snd (a, b) = b

let rec prompt_command (curr_acc : account) =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nPlease enter a command. Type -help to view all valid commands\n";
  print_string "> ";
  try
    match parse (read_line ()) with
    | Buy (ticker, num_shares) ->
        if num_shares > 0.0 then (
          try
            ANSITerminal.print_string [ ANSITerminal.yellow ]
              ("Buying " ^ string_of_float num_shares ^ " of " ^ ticker
             ^ " into your portfolio... \n      Type -bal to see new balances\n"
              );
            let new_acc =
              buy num_shares (String.uppercase_ascii ticker) curr_acc
            in
            ANSITerminal.print_string [ ANSITerminal.green ]
              ("You have successfully bought " ^ string_of_float num_shares
             ^ " shares of " ^ ticker);
            print_endline "\n Here is your updated portfolio";
            print_endline (port_to_string new_acc.portfolio);
            prompt_command new_acc
          with
          | Buy.Broke ->
              ANSITerminal.print_string [ ANSITerminal.magenta ]
                ("Purchasing " ^ string_of_float num_shares ^ " of " ^ ticker
               ^ " is more than what is in your account. \n\
                 \      Try again with a valid purchase.");
              prompt_command curr_acc
          | Buy.NoSuchStock _ ->
              print_endline
                "This stock does not exist. Try again. Note our API (free \
                 version) can only handle 5 calls per minute, so this can also \
                 be an API call overload. ";
              prompt_command curr_acc
          | _ ->
              print_endline
                " Please enter a valid number of shares to purchase.";
              prompt_command curr_acc)
        else
          ANSITerminal.print_string [ ANSITerminal.yellow ]
            "Error, the number of shares you want to buy must be greater than 0";
        prompt_command curr_acc
    | Sell (ticker, num_shares) ->
        if num_shares > 0.0 then (
          try
            ANSITerminal.print_string [ ANSITerminal.yellow ]
              ("Attemping to sell " ^ string_of_float num_shares ^ " of "
             ^ ticker
             ^ " from your portfolio... \n\
               \       Type -bal to see new balances\n");
            let new_acc =
              sell num_shares (String.uppercase_ascii ticker) curr_acc
            in
            ANSITerminal.print_string [ ANSITerminal.green ]
              ("You have successfully sold " ^ string_of_float num_shares
             ^ " shares of " ^ ticker);
            print_endline "\n Here is your updated portfolio";
            print_endline (port_to_string (fst new_acc).portfolio);
            if snd new_acc > 0. then
              print_endline
                ("\n Your profit from the sale: "
                ^ string_of_float (snd new_acc))
            else if snd new_acc < 0. then
              print_endline
                ("\n Your loss from the sale: "
                ^ string_of_float (0. -. snd new_acc))
            else if snd new_acc = 0. then print_endline "\n You broke even.";
            prompt_command (fst new_acc)
          with
          | Broke ->
              ANSITerminal.print_string [ ANSITerminal.magenta ]
                ("Selling " ^ string_of_float num_shares ^ " of " ^ ticker
               ^ " is more than the number of shares that you own in your\n\
                  account. \n\
                 \    Try again with a valid sale.");
              prompt_command curr_acc
          | Sell.NoSuchStock _ ->
              print_endline
                "This stock does not exist. Try again. Note our API (free \
                 version) can only handle 5 calls per minute, so this can also \
                 be an API call overload. ";
              prompt_command curr_acc
          | Sell.NotOwned _ ->
              print_endline "You do not own any shares of this stock.";
              prompt_command curr_acc
          | _ ->
              print_endline "Please enter a valid number of shares to sell.";
              prompt_command curr_acc)
        else
          ANSITerminal.print_string [ ANSITerminal.yellow ]
            "Error, the number of shares you want to sell must be greater than \
             0";
        prompt_command curr_acc
    | View ticker -> (
        try
          print_endline
            ("Price for " ^ ticker ^ ": "
            ^ string_of_float (get_ticker_price (String.uppercase_ascii ticker))
            );
          prompt_command curr_acc
        with NoSuchStock _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "This stock does not exist. Try again. Note our API (free version) \
             can only handle 5 calls per minute, so this can also be an API \
             call overload. ";
          prompt_command curr_acc)
    | ViewOption ticker -> (
        try
          ANSITerminal.print_string [ ANSITerminal.green ]
            ("\nPrice: "
            ^ string_of_float
                (let interest_rate = 0.02 in
                 let volatility = 0.25 in
                 let symbol, expiration_date, strike_price =
                   get_option_contract ticker
                 in
                 let underlying_price = get_ticker_price symbol in
                 let time_to_expiration = time_to_expiration_years ticker in
                 let delta, gamma, vega, theta, rho =
                   compute_greeks underlying_price strike_price
                     time_to_expiration interest_rate volatility
                 in

                 ANSITerminal.print_string [ ANSITerminal.yellow ]
                   ("\nOption Contract: " ^ symbol);
                 ANSITerminal.print_string [ ANSITerminal.red ]
                   ("\n\nExpiration Date: " ^ expiration_date);
                 ANSITerminal.print_string [ ANSITerminal.red ]
                   ("\nStrike price: " ^ string_of_float strike_price);
                 ANSITerminal.print_string [ ANSITerminal.green ]
                   "\n\nGreeks:\n";
                 Printf.printf "  Delta: %f\n" delta;
                 Printf.printf "  Gamma: %f\n" gamma;
                 Printf.printf "  Vega: %f\n" vega;
                 Printf.printf "  Theta: %f\n" theta;
                 Printf.printf "  Rho: %f\n\n" rho;
                 if String.lowercase_ascii (String.sub ticker 12 1) = "c" then
                   let call_price =
                     black_scholes_call underlying_price strike_price
                       time_to_expiration interest_rate volatility
                   in

                   call_price
                 else
                   let put_price =
                     black_scholes_put underlying_price strike_price
                       time_to_expiration interest_rate volatility
                   in

                   put_price));
          prompt_command curr_acc
        with Stocks.NoSuchStock _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "This option does not exist. Try again running \
             -options_ticker_help to see how to input an options ticker";
          prompt_command curr_acc)
    | OptionsTickerHelp ->
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\n\
           How To Call An Options Ticker\n\
          \          \n\
           To help illustrate how to read an options ticker, take a look at \
           the following example (O:EVRI240119C00002500). This is how you \
           properly call an option \n\
          \          \n\
          \          A January 19th, 2024 Call Option for EVRI with a $2.50 \
           Strike Price\n\
          \          \n\
          \          O:EVRI240119C00002500 = EVRI + 240119 + C + 00002500\n\
          \          \n\
          \          Underlying Stock - EVRI\n\
          \          Expiration Date - January 19th, 2024 or ‘240119’ (YYMMDD)\n\
          \          Option Type - Call or ‘C’\n\
          \          Strike Price - 00002500 (2500/1000) or $2.50\n\
          \          \n\
          \      ";
        prompt_command curr_acc
    | Portfolio ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Here are all the stocks you own, their price, and your shares: \n";
        print_string (port_to_string curr_acc.portfolio);
        prompt_command curr_acc
    | Watchlist ->
        ANSITerminal.print_string [ ANSITerminal.green ]
          "Here is your current watchlist: \n";
        print_string (watch_to_string curr_acc.watchlist);
        prompt_command curr_acc
    | WatchlistAdd ticker -> (
        try
          let new_acc =
            add_watchlist (String.uppercase_ascii ticker) curr_acc
          in
          ANSITerminal.print_string [ ANSITerminal.green ]
            (ticker
           ^ " was successfully added to your watchlist. \n\
             \    Here is your updated watchlist: \n");
          print_string (watch_to_string new_acc.watchlist);
          prompt_command new_acc
        with Stocks.NoSuchStock _ ->
          ANSITerminal.print_string [ ANSITerminal.yellow ]
            "This stock does not exist. Try again. Note our API (free version) \
             can only handle 5 calls per minute, so this can also be an API \
             call overload. ";
          prompt_command curr_acc)
    | WatchlistRemove ticker -> (
        try
          let new_acc =
            remove_watchlist (String.uppercase_ascii ticker) curr_acc
          in
          ANSITerminal.print_string [ ANSITerminal.green ]
            (ticker
           ^ " was successfully removed from your watchlist. \n\
             \    Here is your updated watchlist: \n");
          print_string (watch_to_string new_acc.watchlist);
          prompt_command new_acc
        with
        | Account.StockNotFound ->
            ANSITerminal.print_string [ ANSITerminal.green ]
              "This stock is not in your watchlist so it cannot be removed. \n\
              \   Here is your watchlist: \n\
              \ ";
            print_string (watch_to_string curr_acc.watchlist);
            prompt_command curr_acc
        | Stocks.NoSuchStock _ ->
            ANSITerminal.print_string [ ANSITerminal.yellow ]
              "This stock does not exist. Try again. Note our API (free \
               version) can only handle 5 calls per minute, so this can also \
               be an API call overload. ";
            prompt_command curr_acc)
    | Dep amt -> (
        try
          if amt > 0.0 then (
            ANSITerminal.print_string [ ANSITerminal.green ]
              ("Successfully deposited : " ^ string_of_float amt ^ "\n");
            let new_acc = deposit amt curr_acc in
            prompt_command new_acc)
          else
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Error, you cannot deposit an amount less than $0.0 ";
          prompt_command curr_acc
        with exc ->
          print_endline "Please enter a valid amount to deposit.";
          prompt_command curr_acc)
    | Bal ->
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          ("Your current balance (cash and stock worth combined) is: "
          ^ string_of_float (Account.balance curr_acc)
          ^ "\n");
        prompt_command curr_acc
    | Cash ->
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          ("Your current cash balance is: "
          ^ string_of_float (Account.cash_balance curr_acc)
          ^ "\n");
        prompt_command curr_acc
    | Equity ->
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          ("Your current stock balance is: "
          ^ string_of_float (Account.stock_balance curr_acc)
          ^ "\n");
        prompt_command curr_acc
    | Cashflow ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Here is your cashflow history: \n";
        print_string (Account.dep_with_string curr_acc.dep_with_log);
        prompt_command curr_acc
    (*Removed \n-view [ticker] feature from UI. Run separately in ./operate *)
    | Help ->
        ANSITerminal.print_string [ ANSITerminal.green ]
          "\n\
           Available commands\n\
          \      \n\
           -bal\n\
          \      \n\
           -cash\n\
          \      \n\
           -equity\n\
          \                \n\
           -cashflow\n\
          \      \n\
           -portfolio\n\
          \      \n\
           -dep [amt]\n\
          \      \n\
           -withdraw [amt]\n\
          \      \n\
           -view [ticker]\n\
          \      \n\
           -view_option [ticker]\n\
          \      \n\
           -options_ticker_help\n\
          \                          \n\
           -buy [ticker] [number of shares]\n\
          \      \n\
           -sell [ticker] [number of shares]\n\
          \                                     \n\
           -history\n\
          \                                    \n\
           -watchlist\n\
          \ \n\
           -watchlist add [ticker]\n\
          \      \n\
           -watchlist remove [ticker]\n\
          \ \n\
           -help \n\n\
           -quit\n";
        prompt_command curr_acc
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.green ]
          "Terminating brokerage simulation. Have a wonderful day! \n\
          \      If you want to run this program again, please type [make play]\n";
        exit 0
    | History ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Here is your transaction history: \n";
        print_string (Log.to_string curr_acc.transaction_log);
        prompt_command curr_acc
    | Withdraw amt ->
        if amt > 0.0 then (
          try
            ANSITerminal.print_string [ ANSITerminal.yellow ]
              ("Withdrawing $" ^ string_of_float amt
             ^ "from your account... \n      Type -bal to see new balance\n");
            let new_acc = withdraw amt curr_acc in
            prompt_command new_acc
          with
          | Broke ->
              ANSITerminal.print_string [ ANSITerminal.magenta ]
                ("$" ^ string_of_float amt
               ^ " is more than is in your account (in cash). \n\
                 \      Try again with a valid withdrawal, or sell your \
                  portfolio.");
              prompt_command curr_acc
          | _ ->
              invalid_msg ();
              prompt_command curr_acc)
        else
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Error, you cannot withdraw an amount less than $0.0 ";
        prompt_command curr_acc
  with
  | Invalid ->
      invalid_msg ();
      prompt_command curr_acc
  | Empty ->
      invalid_msg ();
      prompt_command curr_acc
  | NoSuchStock _ ->
      print_endline
        "This stock does not exist. Try again. Note our API (free version) can \
         only handle 5 calls per minute, so this can also be an API call \
         overload. ";
      prompt_command curr_acc
  | _ ->
      invalid_msg ();
      prompt_command curr_acc

let aapl_stock = { ticker = "AAPL"; price = 135.0 }
let meta_stock = { ticker = "META"; price = 175.0 }

let fresh_acc_example_preloaded_stocks =
  {
    stock_balance = 0.;
    cash_balance = 0.;
    portfolio = [];
    transaction_log = Log.create;
    watchlist = [];
    dep_with_log = Account.create;
  }

(* new_user creates a new user within the UI and a gifted portfolio with some
   stocks and prints out an empty portfolio, giving the user the option to view
   stocks*)
let new_user () =
  print_endline
    "You're a new user with balance of $0. Type -help to view all our \
     brokerage feature commands";
  prompt_command fresh_acc_example_preloaded_stocks

let read_file filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []

let rec terms () =
  let terms_and_conditions = read_file "ui/terms.txt" in
  List.iter (fun line -> print_endline line) terms_and_conditions;

  ANSITerminal.print_string [ ANSITerminal.red ]
    "Do you agree to these terms and conditions? Please enter 'yes' or 'no' ";
  print_string "> ";
  match read_line () with
  | "yes" -> new_user ()
  | "no" ->
      print_string
        "You must agree to the terms and conditions to trade with Jame Street.\n"
  | _ ->
      print_string "Please enter a valid command.";
      print_string "> ";
      terms ()

(** age_check verifies that the user is atleast the legal age limit to engage in
    trading*)
let rec age_check ans =
  match ans with
  | "yes" -> terms ()
  | "no" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Unfortunately, our terms abide by federal law where users must be \
         atleast\n\
         eighteen years old to access trading tools. Have a great day!.\n"
  | _ ->
      print_endline "Please enter a valid command.";
      print_string "> ";
      age_check (read_line ())

(** Starts the user interface for Jame Street*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Jame Street.\n";
  print_endline
    "We are the fastest growing trading firm in the industry using\n\
     innovative technology with fast user services.";
  print_endline "";

  ANSITerminal.print_string [ ANSITerminal.red ]
    "To begin trading, please indicate if you are at least eighteen years old. \
     Please enter 'yes' or 'no' ";
  print_string "> ";
  match read_line () with
  | h -> age_check h

(* Execute the game engine. *)
let () = main ()
