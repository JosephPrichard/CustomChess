module CharMap = Map.Make (Char)

let identity x = x

let add_pairs pairs map =
  List.fold_left
    (fun acc pair ->
      let k, v = pair in
      CharMap.add k v acc)
    map
    pairs

let option_to_result o e =
  match o with
  | Some x -> Ok x
  | None -> Error e

let is_numeric c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'a') || c >= 'A' || c <= 'Z'
let rec mcons elem n list = if n > 0 then elem :: mcons elem (n - 1) list else list

let inner_substr s b e =
  let len = String.length s in
  if len > 0 && b = String.get s 0 && e = String.get s (len - 1) then
    Some (String.sub s 1 (len - 1))
  else
    None

let fold_str_result f acc str =
  let rec loop acc i len =
    if i < len then
      let c = String.get str i in
      match acc with
      | Ok acc -> loop (f acc c) (i + 1) len
      | e -> e
    else
      acc
  in
  loop (Ok acc) 0 (String.length str)

let fold_result f acc list =
  let rec loop f acc list =
    match list with
    | [] -> acc
    | h :: list ->
      (match acc with
       | Ok acc -> loop f (f acc h) list
       | e -> e)
  in
  loop f (Ok acc) list

let ( let* ) = Result.bind

(* Find the integer sqrt for a small value of x - return None if no integer sqrt exists *)
let int_sqrt x =
  let rec loop i x =
    let sq = i * i in
    if x = 0 || x = 1 then
      Some x
    else if x < sq then
      loop (i + 1) x (* Keep looking *)
    else if x = sq then
      Some i (* Found the sqrt *)
    else
      None (* x does not have a sqrt *)
  in
  loop 0 x

let ok_or_raise result =
  match result with
  | Ok r -> r
  | Error exn -> raise exn

let assert_true ~(expected : 'a) ~(actual : 'a) ~show =
  if expected <> actual then
    Printf.printf
      "Assert_true failed: expected: %s actual %s\n\n"
      (show expected)
      (show actual)
  else
    Printf.printf "Assert_true passed %s\n\n" (show actual)

let assert_ok ~(expected : 'a) ~(actual : ('a, exn) result) ~show =
  let ok = ok_or_raise actual in
  assert_true ~expected ~actual:ok ~show

let show_list show list =
  List.fold_left (fun acc x -> acc ^ show x ^ "; ") "[ " list ^ "]"

let show_str_list list = show_list (fun x -> Printf.sprintf "\"%s\"" x) list
