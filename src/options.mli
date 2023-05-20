(** This module provides functions for working with option contracts and pricing
    them using the Black-Scholes model. *)

open CalendarLib

exception NoSuchOption of string
(** Raised when an invalid option ticker is provided. *)

val get_option_contract : string -> string * string * float
(** [get_option_contract ticker] retrieves information for a specific option
    contract with the given [ticker] using the Polygon.io API. Returns a tuple
    containing the option ticker, expiration date (as a string), and strike
    price (as a float). Raises [NoSuchOption] if the ticker is not found. *)

val black_scholes_call : float -> float -> float -> float -> float -> float
(** [black_scholes_call s x t r sigma] calculates the price of a call option
    using the Black-Scholes model. [s] is the underlying asset price, [x] is the
    strike price, [t] is the time to expiration (in years), [r] is the risk-free
    interest rate, and [sigma] is the volatility of the underlying asset.
    Returns the calculated call option price. *)

val black_scholes_put : float -> float -> float -> float -> float -> float
(** [black_scholes_put s x t r sigma] calculates the price of a put option using
    the Black-Scholes model. [s] is the underlying asset price, [x] is the
    strike price, [t] is the time to expiration (in years), [r] is the risk-free
    interest rate, and [sigma] is the volatility of the underlying asset.
    Returns the calculated put option price. *)

val compute_greeks :
  float ->
  float ->
  float ->
  float ->
  float ->
  float * float * float * float * float
(** [compute_greeks s x t r sigma] computes the Greeks (Delta, Gamma, Vega,
    Theta, and Rho) for an option contract using the Black-Scholes model. [s] is
    the underlying asset price, [x] is the strike price, [t] is the time to
    expiration (in years), [r] is the risk-free interest rate, and [sigma] is
    the volatility of the underlying asset. Returns a tuple containing the
    Greeks (Delta, Gamma, Vega, Theta, and Rho). *)

val time_to_expiration_years : string -> float
(** [time_to_expiration_years ticker] returns the time to expiration in years
    for the given option ticker formatted as "O:AAPL230616C00150000". The
    function calculates the time difference between the current date and the
    expiration date of the option. *)
