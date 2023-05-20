(** An account to make transactions with.*)

exception Broke
(** Raised when attempting to withdraw more money than available.*)

exception OverMemory
(** Raised when a user attempts to deposit money into an account resulting in an
    integer the brokerage cannot handle: 1073741823 which leads to overmemory *)

exception StockNotFound
(** Raised when a user attempts to remove a ticker from their watchlist that is
    not in their watchlist *)

type stock = {
  ticker : string;
  price : float;
}
(** Type [stock] contains the ticker symbol [ticker] and current price [price]. *)

type account = {
  stock_balance : float;
  cash_balance : float;
  portfolio : (stock * float) list;
  transaction_log : transaction list;
  watchlist : (stock * float) list;
  dep_with_log : dep_with list;
}
(** Type [account] contains the floats [stock_balance], [cash_balance] and the
    set-like-list of stocks [portfolio] and how many shares owned. It
    additionally contains a list of transactions anf balances named
    [transaction_log] and [balance_log], respectively*)

and transaction = {
  time : string;
  type_of_transaction : string;
  share : float;
  stock : stock;
}
(**Type [transaction] any transaction has a [time], [type_of_transaction],
   [share], and [stock] field. *)

and dep_with = {
  ctime : string;
  type_of : string;
  amount : float;
  prev_balance : float;
}
(** Type [dep_with] keeps track of the time of a withdrawal or deposit, as well
    as the amount and the balance. Let's call this a cashflow log. *)

val create : dep_with list
(** Makes a new, empty dep_with. *)

val cash_to_string_quad : dep_with -> string * string * string * string
(** [cash_to_string_quad cash] returns the 4-tuple of string values of the field
    of a dep_with*)

val dep_with_string : dep_with list -> string
(** [to_string log] returns the string-like representation of a cashflow log.
    Useful in printing*)

val stock_to_string_pair : stock -> string * string
(** [stock_to_string_pair stk] returns the tuple of strings where the first
    element is the ticker and the second element is the price*)

val transaction_to_string_quint :
  transaction -> string * string * string * string * string
(** [transaction_to_string_quint trans] returns the 5-tuple of string values of
    the field of a transaction*)

val withdraw : float -> account -> account
(** [withdraw amt acc] is a record with updated [stock_balance] and
    [cash_balance] after withdrawing the specified amount [amt]. Raises [Broke]
    if the requested withdrawal amount is greater than the available
    cash_balance in the account. Example: given an account with [cash_balance] =
    1000.0, [stock_balance] = 500.0, and portfolio, withdrawing 200.0 will
    result in an account with cash_balance = 800.0 and stock_balance = 300.0.*)

val deposit : float -> account -> account
(** [deposit amt acc] is a new account record with an updated cash_balance after
    depositing the specified amount [amt]. Raises [OverMemory] if the total
    account stock_balance would be over 1073741823. Examples: Given an account
    with cash_balance = 1000.0, depositing 200.0 will result in an account with
    cash_balance = 1200.0. If the deposit amount is 0.0, the function will
    return an account with the same cash_balance.*)

val balance : account -> float
(** [balance acc] is the cash and stock balance of [acc] in float form. Example:
    balance \{stock_balance = 500.0; cash_balance = 500.0 ; portfolio = []\} is
    500.*)

val stock_balance : account -> float
(** [stock_balance acc] is the stock balance of the account as a float. *)

val cash_balance : account -> float
(** [cash_balance acc] is the cash balance of the account as a float. *)

val ret_portfolio : (stock * float) list -> (string * float * float) list
(** [ret_portfolio port] is a list of each ticker with its associated average
    price and quantity of shares from the account's portfolio. Example:
    portfolio \[\{ticker = AAPL; price = 125.0\} 3.0); (\{ticker = META; price =
    175.0\}, 2.0)\] is \[\["AAPL";"125.0";"3.0"\];\["META";"175.0";"2.0"\]\]*)

val port_to_string : (stock * float) list -> string
(** [port_to_string port] is a string representation of an account's portfolio.
    Example: portfolio
    [{ticker = AAPL; price = 125.0}, 3.0 ); {ticker = META; price = 175.0}, 2.0]
    is \{(AAPL,125.0,3.0), (META,175.0,2.0)\} *)

val only_stocks : account -> string list
(** Returns only the stocks in an account. *)

val add_watchlist : string -> account -> account
(** [add_watchlist ticker acc] adds the selected ticker and its current price
    into the account's watchlist or doesn't change the account's watchlist if
    the ticker isn't found. Example: adding MSFT into a watchlist of
    [({ticker = AAPL; price = 125.0}, 123.0); ({ticker = META; price = 175.0}, 163.0)]
    is
    [({ticker = AAPL; price = 125.0}, 123.0); ({ticker = META; price = 175.0}, 163.0); 
    ({ticker = MSFT; price = 175.0}, 123.0)] *)

val remove_watchlist : string -> account -> account
(** [remove_watchlist ticker acc] removes the selected ticker from the account's
    watchlist or raises StockNotFound if the selected ticker is not in the
    account's watchlist. Example: removing AAPL from a watchlist of
    [({ticker = AAPL; price = 125.0}, 126.0); ({ticker = META; price = 175.0}, 163.0)]
    is [({ticker = META; price = 175.0 }, 163.0)] *)

val watch_to_string : (stock * float) list -> string
(** [watch_to_string watchlist] is a string representation of an account's
    watchlist. Example: watchlist
    [({ticker = AAPL; price = 125.0}, 126.0); ({ticker = META; price = 175.0}, 163.0)]
    is \{(AAPL,125.0, 126.0), (META,175.0, 163.0)\}*)

val find_portfolio_stock : string -> (stock * float) list -> float option
(** [find_portfolio_stock ticker port] is the number of shares of the stock with
    ticker symbol [ticker] in the portfolio [port]. If no such stock is found,
    the function returns None. *)

val update_portfolio_stock :
  string -> float -> (stock * float) list -> (stock * float) list
(** [update_portfolio_stock ticker num port] updates the number of shares of the
    stock with ticker symbol [ticker] in the portfolio [port] to be [num]. If no
    such stock is found, the portfolio is unchanged. *)

val buy : float -> string -> account -> account
(** [buy shares ticker acc] buys [shares] amount of the stock with ticker
    [ticker] in the account [acc]. If the account does not have enough cash
    balance for the purchase, the function raises the [Broke] exception. *)

val sell : float -> string -> account -> account
(** [sell shares ticker acc] sells [shares] amount of the stock with ticker
    [ticker] from the account [acc]. If the account does not own enough shares
    of the stock, the function raises the [Broke] exception. *)

val portfolio_value : account -> float
(** [portfolio_value acc] is the total value of the portfolio in account [acc]. *)

val is_stock_in_portfolio : string -> account -> bool
(** [is_stock_in_portfolio ticker acc] is true if the stock with ticker symbol
    [ticker] is in the portfolio of account [acc], and false otherwise. *)

val check_watchlist : account -> unit
(** [check_watchlist acc] checks the watchlist of account [acc] and prints a
    message if a stock's current price has fallen below the price at which it
    was added to the watchlist. *)

val transaction_log_to_string : transaction list -> string
(** [transaction_log_to_string log] is a string representation of the
    transaction log [log]. *)

val print_account_details : account -> unit
(** [print_account_details acc] prints the details of account [acc], including
    cash balance, stock balance, total balance, portfolio, watchlist,
    transaction log, and cash transaction log. *)
