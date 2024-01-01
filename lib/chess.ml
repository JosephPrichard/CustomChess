open Utils

type color = Clear | White | Black

type piece = {
  sym: char;
  color: color;
}

type position = (int * int)

type direction = 
  | Single of position
  | Vector of position
  | Multiple of direction list

type rule = {
  allowed: position list; (* Positions that allow this rule to be applied *)
  direction: direction; (* Direction this rule allows a piece to move in *)
  hoppable: bool; (* Allows the piece to hop from continous pieces in the vector *)
}

type board = {
  current_color: color;
  tiles: piece array;
}

type piece_ruleset = {
  rules: rule list;
  name: string;
}

type validity = Valid | Invalid

type piece_moves = (piece * position list)

module RulesMap = Map.Make(
  struct
    type t = piece

    (* White pieces will be in 0 to 255 while black pieces are in 256 to 511*)
    let key_of_piece piece = 
      int_of_char piece.sym + if piece.color = White then 0 else 256

    let compare piece1 piece2 = 
      key_of_piece piece1 - key_of_piece piece2
  end
)

type game_ruleset = {
  rules_map: piece_ruleset RulesMap.t;
  start: board;
}

let white_king = {sym = 'k'; color = White}
let black_king = {sym = 'k'; color = Black}
let empty = {sym = ' '; color = Clear}
let reserved = [white_king; black_king; empty]

let pos_of_index index len : position = (index / len, index mod len)

let index_of_pos ((row, col) : position) len = row * len + col

let get_piece tiles (pos : position) = tiles.(index_of_pos pos (Array.length tiles))

let add_pos ((row1, col1) : position) ((row2, col2) : position) : position = (row1 + row2, col1 + col2) 

let add_ruleset piece piece_rules game_rules = RulesMap.add piece piece_rules game_rules.rules_map

let get_ruleset piece game_rules = RulesMap.find piece game_rules.rules_map

let in_bounds pos board =
  let len = Array.length board.tiles in
  let i = index_of_pos pos len in
  i >= 0 && i < len 

let opposite_of_color color =
  match color with
  | Black -> White
  | White -> Black
  | Clear -> Clear

(* Make a move directly on the board - assumes this move was provided as valid by a find moves function *)
let make_move pos_from pos_to board =
  let len = Array.length board.tiles in
  let tiles = board.tiles in 

  let tiles = Array.mapi
    (fun i piece ->
      let pos = pos_of_index i len in
      if pos = pos_from then
        (* The position we moved from becomes empty*)
        empty
      else if pos = pos_to then
        (* The position we move to contains the piece that was at the from position *)
        get_piece tiles pos_from
      else piece)
    tiles
  in
  {tiles = tiles; current_color = opposite_of_color board.current_color}

(* Find the moves for a given position on the board *)
let find_moves pos board game_rules : piece_moves =
  let tiles = board.tiles in
  let piece = get_piece tiles pos in
  let piece_rules = get_ruleset piece game_rules in

  let can_move pos =
    in_bounds pos board 
      && board.current_color != (get_piece board.tiles pos).color
  in

  let can_hop pos hoppable = 
    in_bounds pos board && hoppable
  in  

  let rec walk_vector hoppable inc relative acc =
    let pos = add_pos pos relative in
    let next_pos = add_pos relative inc in
    if can_move pos then
      (* Accumulate the position and continue walking *)
      walk_vector hoppable inc next_pos (pos :: acc)
    else if can_hop pos hoppable then
      (* Continue walking about do not accumulate*)
      walk_vector hoppable inc next_pos acc
    else
      acc
  in

  let rec check_direction hoppable acc direction =
    match direction with
    | Single relative ->
      let pos = add_pos pos relative in
      if can_move pos then (pos :: acc) else acc
    | Vector relative -> 
      walk_vector hoppable relative relative acc
    | Multiple directions ->
      let rec check_directions hoppable acc directions =
        match directions with
        | direction :: directions ->
          let acc = check_direction hoppable acc direction in
          check_directions hoppable acc directions
        | [] -> acc
      in
      check_directions hoppable acc directions
  in
  
  let apply_rule rule acc =
    let is_allowed = Option.is_some (List.find_opt (fun a -> pos = a) rule.allowed) in
    if is_allowed then
      check_direction rule.hoppable acc rule.direction 
    else 
      acc
  in

  let rec apply_ruleset rules acc =
    match rules with
    | rule :: rules ->
      let acc = apply_rule rule acc in
      apply_ruleset rules acc
    | [] -> acc
  in
  (piece, apply_ruleset piece_rules.rules [])

(* Find all moves for all pieces that can move on the board *)
let find_all_moves board game_rules =
  let len = Array.length board.tiles in
  let tiles = board.tiles in

  fold_lefti
    (fun acc i -> 
      let pos = (pos_of_index i len) in
      let piece = tiles.(i) in
      if piece.color = board.current_color then 
        let moves = find_moves pos board game_rules in
        moves :: acc
      else 
        acc) 
    [] tiles

let count_piece target_piece board = 
  Array.fold_left 
    (fun acc piece -> if piece = target_piece then acc+1 else acc) 
    0 board.tiles 

let contains_piece piece board = count_piece piece board >= 1

(* Validates a given ruleset is valid *)
let validate_rulset ruleset = 
  (* It cannot have rules for empty pieces or kings *)
  let rec uses_reserved ruleset reserved = 
    match reserved with
    | piece :: reserved -> begin 
      match RulesMap.find_opt piece ruleset.rules_map with
      | Some _ -> true
      | None -> uses_reserved ruleset reserved end
    | [] -> false
  in

  (* The starting board must contain one and only one black and white king *)
  let invalid_kings ruleset =
    let wc = count_piece white_king ruleset.start in 
    let bc = count_piece black_king ruleset.start in
    wc != 1 && bc != 1
  in  

  if uses_reserved ruleset reserved then Invalid
  else if invalid_kings ruleset then Invalid
  else Valid
