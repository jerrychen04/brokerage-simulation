(** Transaction log for system. *)

open Account

type t = Account.transaction list
(** A list of the transactions that were performed with an account *)

val create : t
(**[create] makes a new empty transaction list*)

val add : Account.transaction -> t -> t
(** [add tran prev_log] generates a new transaction with a new transaction
    [tran] performed on an account with previous transactions [prev_log]. This
    transaction is prepended to the head of the list.*)

val to_string : t -> string
(** [to_string my_log] creates a string representation of the transaction log
    [my_log] that will be useful for displaying to clients. *)
