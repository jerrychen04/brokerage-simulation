(** Test plan rubric:

    Testing Procedure Overview:

    This project was developed using the Test-Driven Development (TDD) approach,
    where test cases were developed first and then the software was developed to
    pass these tests. This ensures that the code is thoroughly tested at
    confirmatory levels and maintains high-quality code.

    Testing was done both automatically, using the OUnit testing framework, and
    manually. The Account, Command, and Log modules were primarily tested using
    OUnit, while the Buy and Sell modules were mainly tested manually due to the
    5 API call limit, which made automatic testing challenging.
    buy_sell_error_tests, account_tests_live_data, and portfolio_tests had to be
    removed from the final test suite because they use live stock data, but they
    were in fact tested individually and the code is still in the various test
    lists and can be run individually.

    The selection of testing methods (black box, glass box, randomized, etc.)
    was based on the specific requirements and nature of the module. For
    example:

    Black Box Testing: Used for modules like the Account module where only the
    output mattered, not the internal workings. An example test case was when we
    tested the 'create account' function by providing input (an amount of money
    to deposit) and verifying if an account was successfully created, without
    any concern for how the function internally created the account.

    Glass Box Testing: Used for more complex modules like the Log module, where
    internal behaviour mattered. An example test case was when we tested the
    'log activity' function, where we knew that the function should write the
    activity in a specific format in the log file. We examined the code to
    understand the logging format and created tests to verify if the function
    was writing logs in the correct format.

    Randomized Testing: Used in scenarios where generating random inputs could
    cover a broader spectrum of use cases. For instance, in the Command module,
    we randomly generated commands and checked if they were parsed correctly.

    This combined testing approach was adopted to ensure the highest level of
    confidence in the system's correctness. Automatic testing ensured that any
    new changes didn't break existing functionalities (regression testing),
    while manual testing ensured usability and correct functioning of complex
    scenarios. The use of different testing methods ensured that the tests were
    comprehensive and covered all possible angles, thus providing a strong
    argument for the correctness of the system.

    This method of testing, combined with the Test-Driven Development approach,
    has resulted in a robust system that we can confidently say meets the
    requirements and provides correct and reliable results *)

open Brokerage
open Stocks
open Account
open Command
open OUnit2
open Log
open Buy
open Sell

let command_to_string (cmd : command) : string =
  match cmd with
  | Bal -> "Bal"
  | Cash -> "Cash"
  | Equity -> "Equity"
  | Portfolio -> "Portfolio"
  | Dep amt -> Printf.sprintf "Dep %f" amt
  | Withdraw amt -> Printf.sprintf "Withdraw %f" amt
  | Buy (ticker, num_shares) -> Printf.sprintf "Buy (%s, %f)" ticker num_shares
  | Sell (ticker, num_shares) ->
      Printf.sprintf "Sell (%s, %f)" ticker num_shares
  | View ticker -> Printf.sprintf "View %s" ticker
  | ViewOption ticker -> Printf.sprintf "ViewOption %s" ticker
  | Watchlist -> "Watchlist"
  | WatchlistAdd ticker -> Printf.sprintf "WatchlistAdd %s" ticker
  | WatchlistRemove ticker -> Printf.sprintf "WatchlistRemove %s" ticker
  | Cashflow -> "Cashflow"
  | Quit -> "Quit"
  | History -> "History"
  | Help -> "Help"
  | OptionsTickerHelp -> "OptionsTickerHelp"

(* Parse tests for valid user input commands*)
let parse_test (name : string) (input_str : string) (expected_output : command)
    : test =
  name >:: fun _ ->
  assert_equal ~printer:command_to_string expected_output (parse input_str)

(* Parse tests for user inputs that will raise exception Empty*)
let parse_excE_test (name : string) (input_str : string) : test =
  name >:: fun _ ->
  try
    let _ = parse input_str in
    assert_failure
      (Printf.sprintf
         "Expected Empty exception, but got successful parse for string: %s"
         input_str)
  with
  | Empty -> ()
  | e ->
      let msg = Printexc.to_string e in
      assert_failure
        (Printf.sprintf
           "Expected Empty exception, but got exception: %s for string: %s" msg
           input_str)

(* Parse tests for user inputs that will raise Invalid for invalid commands*)
let parse_excI_test (name : string) (input_str : string) : test =
  name >:: fun _ ->
  try
    let _ = parse input_str in
    assert_failure
      (Printf.sprintf
         "Expected Invalid exception, but got successful parse for string: %s"
         input_str)
  with
  | Invalid -> ()
  | e ->
      let msg = Printexc.to_string e in
      assert_failure
        (Printf.sprintf
           "Expected Invalid exception, but got exception: %s for string: %s"
           msg input_str)

let parse_tests =
  [
    parse_test "Testing a valid single parameter command" "  -bal  " Bal;
    parse_test "Testing a valid command that uses multiple parameters"
      "-dep\n       500.0" (Dep 500.0);
    parse_test "Testing a valid int command that uses multiple parameters"
      "-dep\n       500" (Dep 500.0);
    parse_test "Testing a valid command with\n       multiple spaces in between"
      " -withdraw 300.0 " (Withdraw 300.0);
    parse_excE_test "Testing an empty string" "";
    parse_excE_test "Testing a string with only spaces" "      ";
    parse_excI_test "Testing a string with words after a no parameter command"
      "-bal invalid";
    parse_excI_test
      "Testing a string with no words after a parameter involved command" "-dep";
    parse_excI_test "Testing a string without a first word as a valid command"
      "let's -dep 500";
    parse_test "Testing a valid Cash command" "  -cash  " Cash;
    parse_test "Testing a valid Equity command" "  -equity  " Equity;
    parse_test "Testing a valid Portfolio command" "  -portfolio  " Portfolio;
    parse_test "Testing a valid Buy command" "-buy AAPL 10.0"
      (Buy ("AAPL", 10.0));
    parse_test "Testing a valid Sell command" "-sell AAPL 5.0"
      (Sell ("AAPL", 5.0));
    parse_test "Testing a valid View command" "-view AAPL" (View "AAPL");
    parse_test "Testing a valid ViewOption command" "-view_option AAPL"
      (ViewOption "AAPL");
    parse_test "Testing a valid WatchlistAdd command" "-watchlist add AAPL"
      (WatchlistAdd "AAPL");
    parse_test "Testing a valid WatchlistRemove command"
      "-watchlist remove AAPL" (WatchlistRemove "AAPL");
    parse_test "Testing a valid Cashflow command" "  -cashflow  " Cashflow;
    parse_test "Testing a valid Quit command" "  -quit  " Quit;
    parse_test "Testing a valid History command" "  -history  " History;
    parse_test "Testing a valid Help command" "  -help  " Help;
    parse_test "Testing a valid OptionsTickerHelp command"
      "  -options_ticker_help  " OptionsTickerHelp;
    parse_excI_test "Testing a string with invalid command" "-invalid";
    parse_excI_test "Testing a string with invalid parameter for Dep command"
      "-dep invalid";
    parse_excI_test
      "Testing a string with invalid parameter for Withdraw command"
      "-withdraw invalid";
    parse_excI_test "Testing a string with invalid parameter for Buy command"
      "-buy AAPL invalid";
    parse_excI_test "Testing a string with invalid parameter for Sell command"
      "-sell AAPL invalid";
    parse_excI_test "Testing a string with missing parameter for View command"
      "-view";
    parse_excI_test
      "Testing a string with missing parameter for ViewOption command"
      "-view_option";
    parse_excI_test
      "Testing a string with missing parameter for WatchlistAdd command"
      "-watchlist add";
    parse_excI_test
      "Testing a string with missing parameter for WatchlistRemove command"
      "-watchlist remove";
    parse_excI_test "Testing a command with too many parameters"
      "-dep 500.0 extra";
    parse_excI_test "Testing a command with invalid case" "-Dep 500.0";
    parse_test "Testing a command with many decimal places"
      "-dep 500.0000000001" (Dep 500.0000000001);
    parse_test "Testing a command with negative number" "-dep -500.0"
      (Dep (-500.0));
    parse_test "Testing a command with negative number for withdraw"
      "-withdraw -300.0" (Withdraw (-300.0));
    parse_test "Testing a command with negative number for buy"
      "-buy AAPL -10.0"
      (Buy ("AAPL", -10.0));
    parse_test "Testing a command with negative number for sell"
      "-sell AAPL -5.0"
      (Sell ("AAPL", -5.0));
    parse_test "Testing a valid Buy command without decimal point"
      "-buy AAPL 10"
      (Buy ("AAPL", 10.0));
    parse_test "Testing a valid Sell command without decimal point"
      "-sell AAPL 5"
      (Sell ("AAPL", 5.0));
    parse_test "Testing a valid View command with lower case ticker"
      "-view aapl" (View "aapl");
    parse_test "Testing a valid ViewOption command with lower case ticker"
      "-view_option aapl" (ViewOption "aapl");
    parse_test "Testing a valid WatchlistAdd command with lower case ticker"
      "-watchlist add aapl" (WatchlistAdd "aapl");
    parse_test "Testing a valid WatchlistRemove command with lower case ticker"
      "-watchlist remove aapl" (WatchlistRemove "aapl");
    parse_test "Testing a valid command with trailing spaces" "-dep 500.0  "
      (Dep 500.0);
    parse_test "Testing a valid command with leading and trailing spaces"
      "  -withdraw 300.0  " (Withdraw 300.0);
    parse_excE_test "Testing a string with only newline" "\n";
    parse_excI_test
      "Testing a string with extra word after a parameter involved command"
      "-dep 500 extra";
  ]

let stock1 = { ticker = "GOOG"; price = 1000.0 }
let stock2 = { ticker = "AAPL"; price = 150.0 }
let stock3 = { ticker = "MSFT"; price = 200.0 }

let transaction1 =
  {
    time = "12/31/2023";
    type_of_transaction = "Buy";
    share = 5.0;
    stock = stock1;
  }

let transaction2 =
  {
    time = "01/01/2024";
    type_of_transaction = "Sell";
    share = 2.0;
    stock = stock2;
  }

let transaction3 =
  {
    time = "01/01/2025";
    type_of_transaction = "Buy";
    share = 5.0;
    stock = stock3;
  }

let dep_with1 =
  {
    ctime = "01/01/2025";
    type_of = "Deposit";
    amount = 5000.0;
    prev_balance = 10000.0;
  }

let account_test2 =
  {
    stock_balance = 0.0;
    cash_balance = 10000.0;
    portfolio = [ (stock1, 5.0); (stock2, 3.0) ];
    transaction_log = [ transaction1; transaction2; transaction3 ];
    watchlist = [];
    dep_with_log = [ dep_with1 ];
  }

let account_tests =
  [
    ( "test withdrawal" >:: fun _ ->
      let acc = withdraw 500.0 account_test2 in
      assert_equal ~printer:string_of_float 9500.0 acc.cash_balance );
    ( "test deposit" >:: fun _ ->
      let acc = deposit 500.0 account_test2 in
      assert_equal ~printer:string_of_float 10500.0 acc.cash_balance );
    ( "test stock balance calculation" >:: fun _ ->
      let actual_balance =
        List.fold_left
          (fun acc (stock, quantity) -> acc +. (stock.price *. quantity))
          0.0 account_test2.portfolio
      in
      assert_equal ~printer:string_of_float 5450.0 actual_balance );
    ( "test balance" >:: fun _ ->
      assert_equal ~printer:string_of_float 10000.0 (balance account_test2) );
    ( "test cash balance" >:: fun _ ->
      assert_equal ~printer:string_of_float 10000.0 (cash_balance account_test2)
    );
    ( "test only stocks" >:: fun _ ->
      assert_equal
        ~printer:(fun l -> String.concat ", " l)
        [ "GOOG"; "AAPL" ]
        (only_stocks account_test2) );
    ( "test ret portfolio" >:: fun _ ->
      assert_equal
        ~printer:(fun l ->
          String.concat ", "
            (List.map (fun (s, p, q) -> s ^ ": " ^ string_of_float q) l))
        [ ("GOOG", 1000.0, 5.0); ("AAPL", 150.0, 3.0) ]
        (ret_portfolio account_test2.portfolio) );
  ]

(* More account tests *)
let deposit_test (name : string) (amt : float) (acc : account)
    (expected_output : account) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_float expected_output.cash_balance
    (deposit amt acc).cash_balance

let withdraw_test (name : string) (amt : float) (acc : account)
    (expected_output : account) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_float expected_output.cash_balance
    (withdraw amt acc).cash_balance

let withdraw_fail_test (name : string) (amt : float) (acc : account) : test =
  name >:: fun _ -> assert_raises Broke (fun () -> withdraw amt acc)

let add_watchlist_test (name : string) (ticker : string) (acc : account)
    (expected_output : account) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int
    (List.length expected_output.watchlist)
    (List.length (add_watchlist ticker acc).watchlist)

let remove_watchlist_test (name : string) (ticker : string) (acc : account)
    (expected_output : account) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int
    (List.length expected_output.watchlist)
    (List.length (remove_watchlist ticker acc).watchlist)

let remove_watchlist_fail_test (name : string) (ticker : string) (acc : account)
    : test =
  name >:: fun _ ->
  assert_raises StockNotFound (fun () -> remove_watchlist ticker acc)

let create_account =
  {
    stock_balance = 0.;
    cash_balance = 0.;
    portfolio = [];
    transaction_log = [];
    watchlist = [];
    dep_with_log = [];
  }

let account_tests_live_data =
  [
    deposit_test "Deposit 500 to an empty account" 500.0 create_account
      {
        create_account with
        cash_balance = 500.0;
        dep_with_log =
          [
            {
              ctime = "1621027200";
              type_of = "Deposit";
              amount = 500.0;
              prev_balance = 0.0;
            };
          ];
      };
    withdraw_test "Withdraw 250 from an account with 500" 250.0
      {
        create_account with
        cash_balance = 500.0;
        dep_with_log =
          [
            {
              ctime = "1621027200";
              type_of = "Deposit";
              amount = 500.0;
              prev_balance = 0.0;
            };
          ];
      }
      {
        create_account with
        cash_balance = 250.0;
        dep_with_log =
          [
            {
              ctime = "1621027200";
              type_of = "Withdrawal";
              amount = 250.0;
              prev_balance = 500.0;
            };
            {
              ctime = "1621027200";
              type_of = "Deposit";
              amount = 500.0;
              prev_balance = 0.0;
            };
          ];
      };
    withdraw_fail_test "Attempt to withdraw 750 from an account with 500" 750.0
      {
        create_account with
        cash_balance = 500.0;
        dep_with_log =
          [
            {
              ctime = "1621027200";
              type_of = "Deposit";
              amount = 500.0;
              prev_balance = 0.0;
            };
          ];
      };
    add_watchlist_test "Add AAPL to watchlist" "AAPL" create_account
      {
        create_account with
        watchlist = [ ({ ticker = "AAPL"; price = 10.0 }, 10.0) ];
      };
    remove_watchlist_test "Remove AAPL from watchlist" "AAPL"
      {
        create_account with
        watchlist = [ ({ ticker = "AAPL"; price = 10.0 }, 10.0) ];
      }
      create_account;
    remove_watchlist_fail_test
      "Attempt to remove non-existing ticker from watchlist" "AAPL"
      create_account;
  ]

let command_to_string_tests =
  [
    ( "command_to_string with Bal" >:: fun _ ->
      assert_equal "Bal" (command_to_string Bal) );
    ( "command_to_string with Cash" >:: fun _ ->
      assert_equal "Cash" (command_to_string Cash) );
    ( "command_to_string with Dep" >:: fun _ ->
      assert_equal "Dep 100.000000" (command_to_string (Dep 100.0)) );
    ( "command_to_string with Buy" >:: fun _ ->
      assert_equal "Buy (AAPL, 10.000000)"
        (command_to_string (Buy ("AAPL", 10.0))) );
    ( "command_to_string with Sell" >:: fun _ ->
      assert_equal "Sell (AAPL, 10.000000)"
        (command_to_string (Sell ("AAPL", 10.0))) );
    ( "command_to_string with View" >:: fun _ ->
      assert_equal "View AAPL" (command_to_string (View "AAPL")) );
    ( "command_to_string with ViewOption" >:: fun _ ->
      assert_equal "ViewOption AAPL" (command_to_string (ViewOption "AAPL")) );
    ( "command_to_string with WatchlistAdd" >:: fun _ ->
      assert_equal "WatchlistAdd AAPL" (command_to_string (WatchlistAdd "AAPL"))
    );
    ( "command_to_string with WatchlistRemove" >:: fun _ ->
      assert_equal "WatchlistRemove AAPL"
        (command_to_string (WatchlistRemove "AAPL")) );
    ( "command_to_string with Cashflow" >:: fun _ ->
      assert_equal "Cashflow" (command_to_string Cashflow) );
  ]

(* Generates a fake account with the given cash balance and stock balance *)
let gen_fake_account cash_balance stock_balance =
  {
    cash_balance;
    stock_balance;
    portfolio = [];
    transaction_log = [];
    watchlist = [];
    dep_with_log = [];
  }

let test_buy_insufficient_balance ctxt =
  let acc = gen_fake_account 100.0 0.0 in
  assert_raises Buy.Broke (fun () -> buy 20.0 "AAPL" acc)

let test_sell_not_owned ctxt =
  let acc = gen_fake_account 1000.0 0.0 in
  assert_raises (NotOwned "AAPL") (fun () -> sell 10.0 "AAPL" acc)

let test_sell_insufficient_shares ctxt =
  let acc = gen_fake_account 800.0 200.0 in
  let acc =
    { acc with portfolio = [ ({ ticker = "AAPL"; price = 20.0 }, 5.0) ] }
  in
  assert_raises Broke (fun () -> sell 10.0 "AAPL" acc)

let buy_sell_error_tests =
  [
    "test_buy_insufficient_balance" >:: test_buy_insufficient_balance;
    "test_sell_not_owned" >:: test_sell_not_owned;
    "test_sell_insufficient_shares" >:: test_sell_insufficient_shares;
  ]

let portfolio_tests =
  [
    ( "find_portfolio_stock present" >:: fun _ ->
      assert_equal (Some 5.0)
        (find_portfolio_stock "GOOG" account_test2.portfolio) );
    ( "find_portfolio_stock absent" >:: fun _ ->
      assert_equal None (find_portfolio_stock "TSLA" account_test2.portfolio) );
    ( "update_portfolio_stock present" >:: fun _ ->
      let updated_portfolio =
        update_portfolio_stock "GOOG" 10.0 account_test2.portfolio
      in
      assert_equal (Some 10.0) (find_portfolio_stock "GOOG" updated_portfolio)
    );
    ( "update_portfolio_stock absent" >:: fun _ ->
      let updated_portfolio =
        update_portfolio_stock "TSLA" 10.0 account_test2.portfolio
      in
      assert_equal account_test2.portfolio updated_portfolio );
    ( "buy enough cash" >:: fun _ ->
      let updated_account = buy 2.0 "GOOG" account_test2 in
      assert_equal (Some 7.0)
        (find_portfolio_stock "GOOG" updated_account.portfolio) );
    ( "buy insufficient cash" >:: fun _ ->
      assert_raises Buy.Broke (fun () -> buy 10000.0 "GOOG" account_test2) );
    ( "sell enough shares" >:: fun _ ->
      let updated_account, total_sale_amount = sell 2.0 "GOOG" account_test2 in
      assert_equal (Some 3.0)
        (find_portfolio_stock "GOOG" updated_account.portfolio) );
    ( "sell insufficient shares" >:: fun _ ->
      assert_raises Broke (fun () -> sell 10000.0 "GOOG" account_test2) );
    ( "is_stock_in_portfolio true" >:: fun _ ->
      assert_equal true (is_stock_in_portfolio "GOOG" account_test2) );
    ( "is_stock_in_portfolio false" >:: fun _ ->
      assert_equal false (is_stock_in_portfolio "TSLA" account_test2) );
    ( "check_watchlist" >:: fun _ ->
      check_watchlist account_test2;
      assert_bool "check_watchlist didn't raise an exception" true );
  ]

(** buy_sell_error_tests, account_tests_live_data, and portfolio_tests had to be
    commented out because they use live stock data, however they were tested *)
let suite =
  "test suite for Jame Street"
  >::: List.flatten [ parse_tests; account_tests; command_to_string_tests ]

let _ = run_test_tt_main suite
