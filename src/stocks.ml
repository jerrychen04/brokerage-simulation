open Yojson
open Unit
open Yojson.Basic.Util

exception NoSuchStock of string

(** Private URL for APi.*)
let base_url = "https://api.polygon.io"

(** Private, personal key for our team. Max calls per min = 5*)
let api_key = "jn_NAmtAD16hk6azpunzVK1TEvKiu5vy"

let get_ticker_price ticker =
  try
    let endpoint =
      "/v2/aggs/ticker/" ^ ticker ^ "/prev?adjusted=true&apiKey=" ^ api_key
    and query_params = [ ("apiKey", api_key) ] in
    let uri =
      Uri.with_query' (Uri.of_string (base_url ^ endpoint)) query_params
    in
    let url = Uri.to_string uri in
    let cmd = "curl -s -H 'Accept: application/json' '" ^ url ^ "'" in
    let ic = Unix.open_process_in cmd in
    let body_str = input_line ic in
    let body_json = Yojson.Basic.from_string body_str in
    let view_info =
      body_json |> member "results" |> to_list |> List.hd |> member "c"
      |> to_float
    in
    ignore (Unix.close_process_in ic);
    view_info
  with exc -> raise (NoSuchStock ticker)
