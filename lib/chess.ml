type piece = {
  sym: char;
  is_white: bool;
}

type position = (int * int)

type direction = 
  | Single of position
  | Multiple of direction list

type rule = {
  conditions: position list;
  directions: direction;
  hoppable: bool;
}

type board = {
  is_white: bool;
  tiles: piece array;
}

type piece_ruleset = {
  rules: rule list;
  name: string;
  is_white: bool;
}

type move = (piece * position)

let white_king = {sym = 'k'; is_white = true}
let black_king = {sym = 'k'; is_white = false}
let empty = {sym = ' '; is_white = false}
let reserved = [white_king; black_king; empty]

module GameRules = struct
  module RulesMap = Map.Make(
    struct
      type t = piece

      (* White pieces will be in 0 to 255 while black pieces are in 256 to 511*)
      let int_of_color is_white = if is_white then 0 else 256

      let compare piece1 piece2 = 
        int_of_char piece1.sym + int_of_color piece1.is_white - 
          int_of_char piece2.sym + int_of_color piece2.is_white
    end
  )

  type game_ruleset = {
    rules_map: piece RulesMap.t;
    start: board;
  }

  let rec ruleset_uses_reserved ruleset reserved = 
    match reserved with
    | piece :: reserved -> 
      (match RulesMap.find_opt piece ruleset with
      | Some _ -> true
      | None -> ruleset_uses_reserved ruleset reserved)
    | [] -> false

  (* Validates a given ruleset is valid *)
  let validate_ruleset ruleset = 
    match ruleset with
    (* It cannot have rules for empty pieces or kings *)
    | _ when ruleset_uses_reserved ruleset reserved -> false
    (* The starting board must contain one and only one black and white king *)
    | _ when true -> false
    (* Each *)
    | _ -> true

  let add_ruleset piece (ruleset : game_ruleset) game_rules = 
    RulesMap.add piece ruleset game_rules

  let get_ruleset piece ruleset : game_ruleset = 
    RulesMap.find piece ruleset
end

module Game = struct
  (* Make a move directly on the board - assumes this move was provided as valid by a find moves function *)
  let make_move position board = ()

  (* Find the moves for a given position on the board *)
  let find_moves position board game_rules = ()

  (* Find all moves for all pieces that can move on the board *)
  let find_all_moves board game_rules = ()

  let count_piece piece board = 
    Array.fold_left 
      (fun acc p -> if p = piece then acc+1 else acc) 
      0 
      board.tiles 

  let contains_piece piece board 
    = count_piece piece board >= 1 
end



