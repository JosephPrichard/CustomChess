open Utils

type piece = {
  sym: char;
  is_white: bool;
}

type position = (int * int)

type direction = 
  | Single of position
  | Multiple of direction list

type conditions = position list

type rule = {
  conditions: conditions;
  direction: direction;
  hoppable: bool;
}

type board = {
  white_turn: bool;
  tiles: piece array;
}

type piece_ruleset = {
  rules: rule list;
  name: string;
}

type move = (piece * position)

type moves = move list

module RulesMap = Map.Make(
  struct
    type t = piece

    (* White pieces will be in 0 to 255 while black pieces are in 256 to 511*)
    let key_of_piece piece = 
      int_of_char piece.sym + if piece.is_white then 0 else 256

    let compare piece1 piece2 = 
      key_of_piece piece1 - key_of_piece piece2
  end
)

type game_ruleset = {
  rules_map: piece_ruleset RulesMap.t;
  start: board;
}

let white_king = {sym = 'k'; is_white = true}
let black_king = {sym = 'k'; is_white = false}
let empty = {sym = ' '; is_white = false}
let reserved = [white_king; black_king; empty]

module Game = struct
  let pos_of_index index len = (index / len, index mod len)

  let index_of_pos (row, col) len = row * len + col

  let get_piece tiles pos = tiles.(index_of_pos pos (Array.length tiles))

  let add_ruleset piece piece_rules game_rules = 
    RulesMap.add piece piece_rules game_rules.rules_map

  let get_ruleset piece game_rules = 
    RulesMap.find piece game_rules.rules_map

  (* Make a move directly on the board - assumes this move was provided as valid by a find moves function *)
  let make_move pos_from pos_to board =
    let len = Array.length board.tiles in
    let tiles = board.tiles in 

    let tiles = Array.mapi
      (fun i piece ->
        let pos = pos_of_index i len in
        match pos with
        (* The position we moved from becomes empty*)
        | _ when pos = pos_from -> empty
        (* The position we move to contains the piece that was at the from position *)
        | _ when pos = pos_to -> get_piece tiles pos_from
        | _ -> piece)
      tiles
    in
    {tiles = tiles; white_turn = not board.white_turn} 

  (* Find the moves for a given position on the board *)
  let find_moves pos board game_rules =
    let tiles = board.tiles in
    let piece = get_piece tiles pos in
    let piece_rules = get_ruleset piece game_rules in
    
    let rec apply_rule rule acc =
      []
    in

    let rec apply_ruleset rules acc =
      match rules with
      | rule :: rules ->
        let moves = apply_rule rule [] in
        apply_ruleset rules (moves @ acc)
      | [] -> acc
    in
    apply_ruleset piece_rules.rules []

  (* Find all moves for all pieces that can move on the board *)
  let find_all_moves board game_rules =
    let len = Array.length board.tiles in
    let tiles = board.tiles in

    fold_lefti
      (fun acc i -> 
        let pos = (pos_of_index i len) in
        let piece = tiles.(i) in
        if piece.is_white = board.white_turn then 
          let moves = find_moves pos board game_rules in
          moves @ acc
        else 
          acc) 
      [] tiles

  let count_piece target_piece board = 
    Array.fold_left 
      (fun acc piece -> if piece = target_piece then acc+1 else acc) 
      0 board.tiles 

  let contains_piece piece board = count_piece piece board >= 1

  (* Validates a given ruleset is valid *)
  let is_ruleset_valid ruleset = 
    (* It cannot have rules for empty pieces or kings *)
    let rec uses_reserved ruleset reserved = 
      match reserved with
      | piece :: reserved -> 
        (match RulesMap.find_opt piece ruleset.rules_map with
        | Some _ -> true
        | None -> uses_reserved ruleset reserved)
      | [] -> false
    in

    (* The starting board must contain one and only one black and white king *)
    let invalid_kings ruleset =
      let wc = count_piece white_king ruleset.start in 
      let bc = count_piece black_king ruleset.start in
      wc != 1 && bc != 1
    in

    match ruleset with
    | _ when uses_reserved ruleset reserved -> false
    | _ when invalid_kings ruleset -> false
    | _ -> true
end



