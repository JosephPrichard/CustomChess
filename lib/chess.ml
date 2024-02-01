open Lib

type position = int * int

type vpiece =
  | Piece of char
  | King

type piece =
  | White of vpiece
  | Black of vpiece
  | Empty

type direction =
  | Single of position
  | Vector of position
  | Multiple of direction list

type rule = {
  allowed_pos : position list;
  direction : direction;
  hoppable : bool;
}

type board = {
  iswhite : bool;
  pieces : piece array;
  dim : int;
}

module CharMap = Map.Make (Char)

let add_pos (pos1 : position) (pos2 : position) : position =
  let row1, col1 = pos1 in
  let row2, col2 = pos2 in
  (row1 + row2, col1 + col2)

let pos_of_index index dim : position = (index / dim, index mod dim)
let index_of_pos ((row, col) : position) dim = (row * dim) + col

let pos_of_string str =
  let rowc = String.get str 0 in
  let colc = String.get str 1 in
  let row = int_of_char rowc - int_of_char 'a' in
  let col = int_of_char colc - int_of_char '0' in
  (row, col)

let ( .%() ) board pos = board.pieces.(index_of_pos pos board.dim)

exception Ruleset_error of string

let add_rules piece piece_rules (ruleset : rule list CharMap.t) =
  CharMap.add piece piece_rules ruleset

let get_rules piece (ruleset : rule list CharMap.t) =
  match CharMap.find_opt piece ruleset with
  | Some rules -> Ok rules
  | None -> Error (Ruleset_error "Ruleset doesn't contain any valid rule for the piece")

let is_piece_turn piece board =
  match piece with
  | Empty -> false
  | White _ -> board.iswhite
  | Black _ -> not board.iswhite

let map_board f board =
  Array.mapi
    (fun i piece ->
      let pos = pos_of_index i board.dim in
      f pos piece)
    board.pieces

let fold_board f acc board =
  let len = Array.length board.pieces in
  let rec loop acc i =
    if i < len then (
      let pos = pos_of_index i board.dim in
      let piece = board.pieces.(i) in
      let acc = f acc pos piece in
      loop acc (i + 1))
    else
      acc
  in
  loop acc 0

(* Make a move directly on the board - assumes this move was provided as valid by a find moves function *)
let make_move pos_from pos_to board =
  let pieces =
    map_board
      (fun pos piece ->
        if pos = pos_from then
          Empty
        else if pos = pos_to then
          board.%(pos_from)
        else
          piece)
      board
  in
  { board with pieces; iswhite = not board.iswhite }

let in_bounds pos board =
  let row, col = pos in
  row >= 0 && row < board.dim && col >= 0 && col < board.dim

(* Checks if the piece can move to the position on the board *)
let can_move_to piece pos board =
  if in_bounds pos board then (
    match (piece, board.%(pos)) with
    | _, Empty (* Any piece can move to an empty piece *)
    | White _, Black _ (* Pieces can take an opposite color *)
    | Black _, White _ -> true
    | _ -> false)
  else
    false

let rules_of_positions positions =
  List.fold_left
    (fun rules x ->
      let rule = { allowed_pos = []; direction = Single x; hoppable = false } in
      rule :: rules)
    []
    positions

let king_rules =
  rules_of_positions
    [ (0, 1); (1, 0); (-1, 0); (0, -1); (-1, -1); (1, 1); (-1, 1); (1, -1) ]

(* Find the moves for a given position on the board *)
let find_moves base_pos board ruleset =
  let piece = board.%(base_pos) in
  let rec walk_vector hoppable inc relative moves =
    let pos = add_pos base_pos relative in
    let next_pos = add_pos relative inc in
    if can_move_to piece pos board then (* Accumulate the position and continue walking *)
      walk_vector hoppable inc next_pos (pos :: moves)
    else if in_bounds pos board && hoppable then
      (* Continue walking about do not accumulate*)
      walk_vector hoppable inc next_pos moves
    else
      moves
  in
  let rec walk_direction hoppable moves direction =
    match direction with
    | Single relative ->
      let pos = add_pos base_pos relative in
      if can_move_to piece pos board then pos :: moves else moves
    | Vector relative -> walk_vector hoppable relative relative moves
    | Multiple directions -> walk_multiple hoppable moves directions
  and walk_multiple hoppable acc directions =
    (* Walking multiple directions is comprised of walking each direction *)
    match directions with
    | direction :: directions ->
      let acc = walk_direction hoppable acc direction in
      walk_multiple hoppable acc directions
    | [] -> acc
  in
  let rec apply_rules rules moves =
    match rules with
    | rule :: rules ->
      let moves =
        if List.mem base_pos rule.allowed_pos then
          (* If the rule allows the current position, then get all moves in that direction*)
          walk_direction rule.hoppable moves rule.direction
        else
          moves
      in
      apply_rules rules moves
    | [] -> moves
  in
  let apply_rules_with code ruleset =
    let* rules = get_rules code ruleset in
    let moves = apply_rules rules [] in
    Ok (piece, moves)
  in
  match piece with
  | White (Piece vpiece) | Black (Piece vpiece) -> apply_rules_with vpiece ruleset
  | White King | Black King ->
    let moves = apply_rules king_rules [] in
    Ok (piece, moves)
  | Empty -> Ok (piece, [])

(* Find all moves for all pieces that can move on the board *)
let find_all_moves board game_rules =
  fold_board
    (fun acc pos piece ->
      if is_piece_turn piece board then (
        (* A piece that matches the current color is allowed to move *)
        let moves = find_moves pos board game_rules in
        moves :: acc)
      else
        acc)
    []
    board

let count_piece target_piece board =
  fold_board (fun acc _ piece -> if piece = target_piece then acc + 1 else acc) 0 board

let contains_piece piece board = count_piece piece board >= 1

let has_valid_kings board =
  let wc = count_piece (White King) board in
  let bc = count_piece (Black King) board in
  wc != 1 && bc != 1
