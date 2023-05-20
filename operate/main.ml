open Brokerage
open Options
open Stocks

let rec main num =
  let test_options = false in
  if test_options = false then (
    try
      print_endline "\nEnter the ticker symbol of a stock: ";
      let ticker = String.uppercase_ascii (read_line ()) in
      let ticker_price = get_ticker_price ticker in
      print_endline "\nRemember, the API only has 5 calls per minute.";
      print_endline
        ("\nThe closing stock price for "
        ^ String.uppercase_ascii ticker
        ^ " yesterday was\n"
        ^ string_of_float ticker_price);
      main ()
    with
    | NoSuchStock _ ->
        print_endline
          "Invalid Stock Ticker. Please enter a valid ticker symbol.";
        main ()
    | _ ->
        print_endline
          "An error occurred while processing your request. Please try again.";
        main ())
  else
    let ticker = "O:EVRI240119C00002500" in
    let symbol, expiration_date, strike_price = get_option_contract ticker in
    let underlying_price = get_ticker_price symbol in
    let t = 30.0 /. 365.0 in
    (* Time to expiration in years, assuming 30 days *)
    let r = 0.02 in
    (* Risk-free interest rate, 2% *)
    let sigma = 0.25 in

    (* Volatility, e.g., 25% *)
    let price = black_scholes_call underlying_price strike_price t r sigma in
    Printf.printf "%s: %.2f\n" symbol price

let () = main ()
