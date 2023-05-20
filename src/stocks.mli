(** This module provides functions for retrieving stock prices from the Polygon
    API. *)

exception NoSuchStock of string
(** Raised when this ticker symbol does not exist. *)

val get_ticker_price : string -> float
(** [get_ticker_price ticker] grabs the symbol [ticker] of the stock and
    retrieves the closing price from yesterday. Raises NoSuchStock if ticker
    symbol does not exist. *)
