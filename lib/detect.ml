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

(* --- Custom byte-level JSON extractor for the rinha payload schema. --- *)
(* The payload shape is fixed (see DETECTION_RULES.md); we don't need a
   general-purpose parser, just key/value lookups. Yojson allocates an AST
   plus list-of-pairs per object, which is the dominant allocation cost
   per request. The functions below walk the bytes directly. *)

let[@inline] is_digit_c c = c >= '0' && c <= '9'

let[@inline] skip_ws (s : string) i =
  let len = String.length s in
  let j = ref i in
  while !j < len &&
        (let c = String.unsafe_get s !j in
         c = ' ' || c = '\t' || c = '\n' || c = '\r')
  do incr j done;
  !j

(* Find position right after `"key":` (and any whitespace), starting from
   [from]. Returns -1 on miss. *)
let find_key_pos (s : string) (key : string) (from : int) : int =
  let len = String.length s in
  let klen = String.length key in
  let stop_at = len - klen - 2 in
  let rec scan i =
    if i > stop_at then -1
    else if String.unsafe_get s i = '"'
            && String.unsafe_get s (i + klen + 1) = '"'
    then begin
      let rec match_key j =
        j = klen ||
        (String.unsafe_get s (i + 1 + j) = String.unsafe_get key j
         && match_key (j + 1))
      in
      if match_key 0 then
        let cp = i + klen + 2 in
        if cp < len && String.unsafe_get s cp = ':'
        then skip_ws s (cp + 1)
        else scan (i + 1)
      else scan (i + 1)
    end
    else scan (i + 1)
  in
  scan from

let[@inline] parse_float_at (s : string) i : float =
  let len = String.length s in
  let neg, i =
    if i < len && String.unsafe_get s i = '-' then true, i + 1
    else false, i
  in
  let int_part = ref 0.0 in
  let j = ref i in
  while !j < len && is_digit_c (String.unsafe_get s !j) do
    int_part := !int_part *. 10.0
      +. float_of_int (Char.code (String.unsafe_get s !j) - Char.code '0');
    incr j
  done;
  if !j < len && String.unsafe_get s !j = '.' then begin
    incr j;
    let factor = ref 0.1 in
    while !j < len && is_digit_c (String.unsafe_get s !j) do
      int_part := !int_part
        +. float_of_int (Char.code (String.unsafe_get s !j) - Char.code '0')
           *. !factor;
      factor := !factor *. 0.1;
      incr j
    done
  end;
  if neg then -. !int_part else !int_part

let[@inline] parse_int_at (s : string) i : int =
  let len = String.length s in
  let neg, i =
    if i < len && String.unsafe_get s i = '-' then true, i + 1
    else false, i
  in
  let acc = ref 0 in
  let j = ref i in
  while !j < len && is_digit_c (String.unsafe_get s !j) do
    acc := !acc * 10 + (Char.code (String.unsafe_get s !j) - Char.code '0');
    incr j
  done;
  if neg then - !acc else !acc

(* Returns (start, len) of the string contents (excluding quotes), assuming
   s.[i] = '"'. *)
let[@inline] parse_str_at (s : string) i : int * int =
  let start = i + 1 in
  let len = String.length s in
  let j = ref start in
  while !j < len && String.unsafe_get s !j <> '"' do incr j done;
  start, !j - start

let[@inline] is_null_at (s : string) i : bool =
  let len = String.length s in
  i + 4 <= len
  && String.unsafe_get s i = 'n'
  && String.unsafe_get s (i + 1) = 'u'
  && String.unsafe_get s (i + 2) = 'l'
  && String.unsafe_get s (i + 3) = 'l'

let[@inline] is_true_at (s : string) i : bool =
  i + 4 <= String.length s
  && String.unsafe_get s i = 't'

(* Lookup mcc_risk from a (start, len) slice of [s]. Hot path - mcc codes
   are 4 chars. *)
let mcc_risk_slice (s : string) start len =
  if len <> 4 then 0.5
  else
    let c0 = String.unsafe_get s start in
    let c1 = String.unsafe_get s (start + 1) in
    let c2 = String.unsafe_get s (start + 2) in
    let c3 = String.unsafe_get s (start + 3) in
    if c0 = '5' then begin
      if c1 = '4' && c2 = '1' && c3 = '1' then 0.15
      else if c1 = '8' && c2 = '1' && c3 = '2' then 0.30
      else if c1 = '9' && c2 = '1' && c3 = '2' then 0.20
      else if c1 = '9' && c2 = '4' && c3 = '4' then 0.45
      else if c1 = '3' && c2 = '1' && c3 = '1' then 0.25
      else if c1 = '9' && c2 = '9' && c3 = '9' then 0.50
      else 0.5
    end else if c0 = '7' then begin
      if c1 = '8' && c2 = '0' && c3 = '1' then 0.80
      else if c1 = '8' && c2 = '0' && c3 = '2' then 0.75
      else if c1 = '9' && c2 = '9' && c3 = '5' then 0.85
      else 0.5
    end else if c0 = '4' && c1 = '5' && c2 = '1' && c3 = '1' then 0.35
    else 0.5

(* Scan a JSON array of strings starting at s.[i] = '['. Returns true if
   any element equals the substring s[mid_start..mid_start+mid_len-1]. *)
let known_array_contains (s : string) i mid_start mid_len : bool =
  let len = String.length s in
  let j = ref (i + 1) in
  let found = ref false in
  while not !found && !j < len && String.unsafe_get s !j <> ']' do
    let c = String.unsafe_get s !j in
    if c = '"' then begin
      let str_start = !j + 1 in
      let k = ref str_start in
      while !k < len && String.unsafe_get s !k <> '"' do incr k done;
      let str_len = !k - str_start in
      if str_len = mid_len then begin
        let eq = ref true in
        let m = ref 0 in
        while !eq && !m < str_len do
          if String.unsafe_get s (str_start + !m)
             <> String.unsafe_get s (mid_start + !m)
          then eq := false;
          incr m
        done;
        if !eq then found := true
      end;
      j := !k + 1
    end else
      incr j
  done;
  !found

(* Vectorize directly from the raw request body. No Yojson AST. *)
let vectorize_str (s : string) : float array =
  let v = Array.make 14 0.0 in
  let amount        = parse_float_at s (find_key_pos s "amount" 0) in
  let installments  = parse_int_at   s (find_key_pos s "installments" 0) in
  let req_at_pos    = find_key_pos s "requested_at" 0 in
  let req_at_start, _ = parse_str_at s req_at_pos in

  let cu_avg_pos    = find_key_pos s "avg_amount" 0 in
  let cu_avg        = parse_float_at s cu_avg_pos in
  let cu_n24        = parse_int_at   s (find_key_pos s "tx_count_24h" 0) in
  let known_at      = find_key_pos s "known_merchants" 0 in

  let me_pos        = find_key_pos s "merchant" 0 in
  let me_id_pos     = find_key_pos s "id" me_pos in
  let me_id_start, me_id_len = parse_str_at s me_id_pos in
  let me_mcc_pos    = find_key_pos s "mcc" me_pos in
  let me_mcc_start, me_mcc_len = parse_str_at s me_mcc_pos in
  let me_avg        = parse_float_at s (find_key_pos s "avg_amount" me_pos) in

  let te_pos        = find_key_pos s "terminal" 0 in
  let te_online     = is_true_at s (find_key_pos s "is_online"     te_pos) in
  let te_card       = is_true_at s (find_key_pos s "card_present"  te_pos) in
  let te_kmh        = parse_float_at s (find_key_pos s "km_from_home" te_pos) in

  let _ = cu_avg_pos in

  (* parse_iso style on req_at_start *)
  let i k = Char.code (String.unsafe_get s k) - Char.code '0' in
  let n2 a = i a * 10 + i (a+1) in
  let n4 a = i a * 1000 + i (a+1) * 100 + i (a+2) * 10 + i (a+3) in
  let y  = n4 (req_at_start) in
  let mo = n2 (req_at_start + 5) in
  let d  = n2 (req_at_start + 8) in
  let hr = n2 (req_at_start + 11) in
  let mi = n2 (req_at_start + 14) in
  let se = n2 (req_at_start + 17) in
  let dow = day_of_week y mo d in
  let now_s = days_since_epoch y mo d * 86400 + hr * 3600 + mi * 60 + se in

  v.(0) <- clamp (amount /. max_amount);
  v.(1) <- clamp (float_of_int installments /. max_installments);
  v.(2) <- clamp ((amount /. cu_avg) /. amount_vs_avg_ratio);
  v.(3) <- float_of_int hr /. 23.0;
  v.(4) <- float_of_int dow /. 6.0;

  let lt_pos = find_key_pos s "last_transaction" 0 in
  if lt_pos >= 0 && is_null_at s lt_pos then begin
    v.(5) <- -1.0;
    v.(6) <- -1.0
  end else begin
    let ts_pos = find_key_pos s "timestamp" lt_pos in
    let ts_start, _ = parse_str_at s ts_pos in
    let ly  = n4 ts_start in
    let lmo = n2 (ts_start + 5) in
    let ld  = n2 (ts_start + 8) in
    let lh  = n2 (ts_start + 11) in
    let lmi = n2 (ts_start + 14) in
    let lse = n2 (ts_start + 17) in
    let last_s = days_since_epoch ly lmo ld * 86400 + lh * 3600 + lmi * 60 + lse in
    let mins = float_of_int (now_s - last_s) /. 60.0 in
    let kfc = parse_float_at s (find_key_pos s "km_from_current" lt_pos) in
    v.(5) <- clamp (mins /. max_minutes);
    v.(6) <- clamp (kfc /. max_km)
  end;

  v.(7) <- clamp (te_kmh /. max_km);
  v.(8) <- clamp (float_of_int cu_n24 /. max_tx_count_24h);
  v.(9)  <- if te_online then 1.0 else 0.0;
  v.(10) <- if te_card   then 1.0 else 0.0;
  let known = known_array_contains s known_at me_id_start me_id_len in
  v.(11) <- if known then 0.0 else 1.0;
  v.(12) <- mcc_risk_slice s me_mcc_start me_mcc_len;
  v.(13) <- clamp (me_avg /. max_merchant_avg_amount);
  v

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
