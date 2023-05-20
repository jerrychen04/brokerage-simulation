open Yojson
open Yojson.Basic.Util
open Unix
open CalendarLib

exception NoSuchOption of string

(** Private URL.*)
let base_url = "https://api.polygon.io"
 (** Private API key.*)
let api_key = "jn_NAmtAD16hk6azpunzVK1TEvKiu5vy"

let get_option_contract ticker =
  try
    let endpoint =
      "/v3/reference/options/contracts/" ^ ticker ^ "?apiKey=" ^ api_key
    in
    let uri =
      Uri.with_query'
        (Uri.of_string (base_url ^ endpoint))
        [ ("apiKey", api_key) ]
    in
    let url = Uri.to_string uri in
    let cmd = "curl -s '" ^ url ^ "'" in
    let ic = open_process_in cmd in
    let body_str = input_line ic in
    let body_json = Yojson.Basic.from_string body_str in
    let contract = body_json |> member "results" in
    let symbol = contract |> member "ticker" |> to_string in
    let expiration_date = contract |> member "expiration_date" |> to_string in
    let strike_price = contract |> member "strike_price" |> to_float in
    ignore (close_process_in ic);
    (symbol, expiration_date, strike_price)
  with exc -> raise (NoSuchOption ticker)

(** Private helper function to make some greeks calculations. *)
let erf x =
  let a = 0.147 in
  let x_sq = x *. x in
  let p = 4. /. (Float.pi +. (a *. x_sq)) in
  let y = x_sq *. p in
  let erf = x *. exp (-.x_sq *. y) /. (1. +. (x_sq *. y)) in
  erf

(** Private helper function to calculate cdf. *)
let norm_cdf x =
  let sign = if x < 0.0 then -1.0 else 1.0 in
  let y = 1.0 +. erf (sign *. x /. sqrt 2.0) in
  0.5 *. y

let black_scholes_call s x t r sigma =
  let d1 =
    (log (s /. x) +. ((r +. ((sigma ** 2.0) /. 2.0)) *. t)) /. (sigma *. sqrt t)
  and d2 =
    (log (s /. x) +. ((r -. ((sigma ** 2.0) /. 2.0)) *. t)) /. (sigma *. sqrt t)
  in
  let nd1 = norm_cdf d1 and nd2 = norm_cdf d2 in
  (s *. nd1) -. (x *. exp (-.r *. t) *. nd2)

let black_scholes_put s x t r sigma =
  let call_price = black_scholes_call s x t r sigma in
  call_price -. s +. (x *. exp (-.r *. t))

let compute_greeks s x t r sigma =
  let d1 =
    (log (s /. x) +. ((r +. ((sigma ** 2.0) /. 2.0)) *. t)) /. (sigma *. sqrt t)
  and d2 =
    (log (s /. x) +. ((r -. ((sigma ** 2.0) /. 2.0)) *. t)) /. (sigma *. sqrt t)
  in

  let delta = norm_cdf d1
  and gamma =
    exp (-.(d1 ** 2.0) /. 2.0) /. (s *. sigma *. sqrt (2.0 *. Float.pi *. t))
  and vega = s *. sqrt t *. exp (-.(d1 ** 2.0) /. 2.0) /. sqrt (2.0 *. Float.pi)
  and theta =
    -.s *. sigma
    *. exp (-.(d1 ** 2.0) /. 2.0)
    /. (2.0 *. sqrt (2.0 *. Float.pi *. t))
    -. (r *. x *. exp (-.r *. t) *. norm_cdf d2)
  and rho = t *. x *. exp (-.r *. t) *. norm_cdf d2 in

  (delta, gamma, vega, theta, rho)

let time_to_expiration_years date_string =
  let current_time = time () in
  let current_tm = localtime current_time in
  let current_year = current_tm.tm_year + 1900 in
  let current_month = current_tm.tm_mon + 1 in
  let current_day = current_tm.tm_mday in

  let year = int_of_string (String.sub date_string 7 2) + 2000 in
  let month = int_of_string (String.sub date_string 9 2) in
  let day = int_of_string (String.sub date_string 10 2) in

  let total_days = 365.0 in
  (* assuming a non-leap year *)
  let elapsed_days = float_of_int (day - current_day) in
  let elapsed_months = float_of_int (month - current_month) in
  let elapsed_years = float_of_int (year - current_year) in

  let days_proportion = elapsed_days /. total_days in
  let months_proportion = elapsed_months /. 12.0 in
  let years_proportion = elapsed_years in

  let total_proportion =
    days_proportion +. months_proportion +. years_proportion
  in
  total_proportion
