(* Payload → 14-dim normalized vector per DETECTION_RULES.md. *)

let max_amount = 10000.0
let max_installments = 12.0
let amount_vs_avg_ratio = 10.0
let max_minutes = 1440.0
let max_km = 1000.0
let max_tx_count_24h = 20.0
let max_merchant_avg_amount = 10000.0

(* mcc → risk. Default 0.5 for missing. *)
let mcc_risk = function
  | "5411" -> 0.15
  | "5812" -> 0.30
  | "5912" -> 0.20
  | "5944" -> 0.45
  | "7801" -> 0.80
  | "7802" -> 0.75
  | "7995" -> 0.85
  | "4511" -> 0.35
  | "5311" -> 0.25
  | "5999" -> 0.50
  | _ -> 0.5

let[@inline] clamp x = if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

(* Parse ISO8601 "YYYY-MM-DDTHH:MM:SSZ" → (y, mo, d, h, mi, s).
   Strict format; the spec guarantees this shape. *)
let parse_iso s =
  let i k = Char.code (String.unsafe_get s k) - Char.code '0' in
  let n2 a = i a * 10 + i (a+1) in
  let n4 a = i a * 1000 + i (a+1) * 100 + i (a+2) * 10 + i (a+3) in
  let y = n4 0 in
  let mo = n2 5 in
  let d = n2 8 in
  let h = n2 11 in
  let mi = n2 14 in
  let se = n2 17 in
  (y, mo, d, h, mi, se)

(* Days since Unix epoch (1970-01-01) for a Gregorian date. *)
let days_since_epoch y mo d =
  let m = if mo <= 2 then mo + 12 else mo in
  let yy = if mo <= 2 then y - 1 else y in
  let era = (if yy >= 0 then yy else yy - 399) / 400 in
  let yoe = yy - era * 400 in
  let doy = (153 * (m - 3) + 2) / 5 + d - 1 in
  let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy in
  era * 146097 + doe - 719468

(* day_of_week: 0=Mon..6=Sun. *)
let day_of_week y mo d =
  (* Unix epoch 1970-01-01 was Thursday. Thursday = 3 in mon=0..sun=6 system. *)
  let de = days_since_epoch y mo d in
  ((de mod 7) + 3 + 7 * 2) mod 7

(* Total UTC seconds for an ISO timestamp (used for minutes_since_last_tx). *)
let unix_seconds (y, mo, d, h, mi, se) =
  let de = days_since_epoch y mo d in
  de * 86400 + h * 3600 + mi * 60 + se

(* Look up a key in a JSON object (`Assoc), or fail. *)
let field obj k =
  match obj with
  | `Assoc fs ->
    (try List.assoc k fs
     with Not_found -> failwith (Printf.sprintf "missing field %s" k))
  | _ -> failwith "expected object"

let to_float = function
  | `Float f -> f
  | `Int i -> float_of_int i
  | _ -> failwith "expected number"

let to_int = function
  | `Int i -> i
  | `Float f -> int_of_float f
  | _ -> failwith "expected int"

let to_string = function
  | `String s -> s
  | _ -> failwith "expected string"

let to_bool = function
  | `Bool b -> b
  | _ -> failwith "expected bool"

let to_list = function
  | `List xs -> xs
  | _ -> failwith "expected list"

(* Vectorize a parsed JSON payload into a fresh 14-element float array. *)
let vectorize (j : Yojson.Safe.t) : float array =
  let v = Array.make 14 0.0 in
  let tx = field j "transaction" in
  let cu = field j "customer" in
  let me = field j "merchant" in
  let te = field j "terminal" in

  let amount = to_float (field tx "amount") in
  let installments = float_of_int (to_int (field tx "installments")) in
  let req_at = to_string (field tx "requested_at") in

  let cu_avg = to_float (field cu "avg_amount") in
  let cu_n24 = float_of_int (to_int (field cu "tx_count_24h")) in
  let cu_known = to_list (field cu "known_merchants") in

  let me_id = to_string (field me "id") in
  let me_mcc = to_string (field me "mcc") in
  let me_avg = to_float (field me "avg_amount") in

  let te_online = to_bool (field te "is_online") in
  let te_card = to_bool (field te "card_present") in
  let te_kmh = to_float (field te "km_from_home") in

  let parsed = parse_iso req_at in
  let (_, _, _, hr, _, _) = parsed in
  let (y, mo, d, _, _, _) = parsed in
  let dow = day_of_week y mo d in

  v.(0) <- clamp (amount /. max_amount);
  v.(1) <- clamp (installments /. max_installments);
  v.(2) <- clamp ((amount /. cu_avg) /. amount_vs_avg_ratio);
  v.(3) <- float_of_int hr /. 23.0;
  v.(4) <- float_of_int dow /. 6.0;

  (match field j "last_transaction" with
   | `Null ->
     v.(5) <- -1.0;
     v.(6) <- -1.0
   | last ->
     let last_ts = to_string (field last "timestamp") in
     let kfc = to_float (field last "km_from_current") in
     let now_s = unix_seconds parsed in
     let last_s = unix_seconds (parse_iso last_ts) in
     let mins = float_of_int (now_s - last_s) /. 60.0 in
     v.(5) <- clamp (mins /. max_minutes);
     v.(6) <- clamp (kfc /. max_km));

  v.(7) <- clamp (te_kmh /. max_km);
  v.(8) <- clamp (cu_n24 /. max_tx_count_24h);
  v.(9) <- if te_online then 1.0 else 0.0;
  v.(10) <- if te_card then 1.0 else 0.0;

  let known =
    List.exists (fun x -> match x with `String s -> s = me_id | _ -> false) cu_known
  in
  v.(11) <- if known then 0.0 else 1.0;
  v.(12) <- mcc_risk me_mcc;
  v.(13) <- clamp (me_avg /. max_merchant_avg_amount);
  v
