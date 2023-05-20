(** The command type represents the different types of commands a user can
    input. *)

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
(** Raised when a command is not valid*)

exception Empty
(** Raised when a command is empty*)

val parse : string -> command
(** [parse str] parses a user's input into a [command]. The first word that is
    not an empty string becomes the command type listed in -help and the rest of
    the words if any become the phrase. Raises \[Invalid\] when input is not in
    the format of -command \[number if required\]. Example:

    - parse " -dep 500." is \[Dep 500.0\]
    - parse "-bal" is \[Bal\] *)
