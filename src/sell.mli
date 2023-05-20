(** Sell module *)

exception NoSuchStock of string
(** Raised when this ticker symbol does not exist. *)

exception NotOwned of string
(** Raised when the portfolio does not contain this stock in the first place. *)

exception Broke
(** Raised when attempting to withdraw more money than available.*)

val update_avg : float * float -> float * float -> float
(** [update_avg (a1, f1) (a2, f2)] is the weighted average of [a1] and [a2],
    where [f1] and [f2] are the weights of [a1] and [a2] respectively. *)

val order : (Account.stock * float) list -> (Account.stock * float) list
(** [order lst] returns a list of portfolio entries sorted in ascending order of
    ticker symbol and with any entries for the same ticker symbol combined into
    a single entry. *)

val sell : float -> string -> Account.account -> Account.account * float
(** [sell shares ticker acc] sells [shares] shares of stock with the given
    [ticker] at the current market price and updates the given account [acc]
    with the resulting changes. It returns the float of the amount of money made
    (or lost) on the transaction. *)
