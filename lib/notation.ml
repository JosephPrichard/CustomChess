open Lib
open Chess

exception Parse_error of string

let char_of_turn iswhite = if iswhite then 'w' else 'b'

let turn_of_string s =
  match s with
  | "b" -> Ok true
  | "w" -> Ok false
  | _ -> Error (Parse_error (Printf.sprintf "Unknown color %s" s))

let vpiece_of_char c dmap =
  if c = 'K' then
    Ok King
  else
    let* vpiece =
      option_to_result
        (CharMap.find_opt c dmap)
        (Parse_error "Unable to parse character into a piece")
    in
    Ok vpiece

let piece_of_char c dmap =
  if c = 'K' then
    Ok (White King)
  else if c = 'k' then
    Ok (Black King)
  else
    let* piece =
      option_to_result
        (CharMap.find_opt c dmap)
        (Parse_error "Unable to parse character into a piece")
    in
    Ok piece

let char_of_piece piece smap =
  match piece with
  | White (Piece code) | Black (Piece code) ->
    let c = CharMap.find_opt code smap in
    Option.value c ~default:'?'
  | White King | Black King -> 'K'
  | Empty -> '_'

let pieces_of_fen pieces_fen dmap =
  (* Produces a list of pieces and the dimension of said pieces *)
  let* pieces_list =
    fold_str_result
      (fun acc c ->
        let pieces = acc in
        if c = '/' then (* A row seperator means the dimension is reset back to 0*)
          Ok pieces
        else if is_numeric c then
          (* A number indicates there is that number of empty pieces*)
          let count = int_of_char c in
          let pieces = mcons Empty count pieces in
          Ok pieces
        else
          let* piece = piece_of_char c dmap in
          Ok (piece :: pieces))
      []
      pieces_fen
  in
  let pieces = Array.of_list pieces_list in
  (* The dimention of the board is the sqrt of the length - if the sqrt is not an integer the board is invalid *)
  match int_sqrt (Array.length pieces) with
  | Some dim -> Ok (pieces, dim)
  | None -> Error (Parse_error "A board must be NxN, where N is a whole integer")

let board_of_fen fen dmap =
  let tokens = String.split_on_char ' ' fen in
  match tokens with
  | [ pieces_fen; color_fen ] ->
    (* Each space split token needs to be parsed into a seperate structure *)
    let* pieces, dim = pieces_of_fen pieces_fen dmap in
    let* iswhite = turn_of_string color_fen in
    Ok { pieces; dim; iswhite }
  | _ -> Error (Parse_error "Fen needs to have 2 tokens to be parsed")

let is_end_of_row board ((row, col) : position) =
  row != board.dim - 1 && col = board.dim - 1

let fen_of_board board smap =
  let pieces_fen, _ =
    fold_board
      (fun acc pos piece ->
        let fen, empty_count = acc in
        if piece = Empty then
          if is_end_of_row board pos then
            (* An empty piece at the end of the row means the empty count is added to the fen*)
            let fen = fen ^ string_of_int (empty_count + 1) ^ "/" in
            (fen, 0)
          else
            (fen, empty_count + 1)
        else (* A number count of empty pieces *)
          let empty_str = if empty_count > 0 then string_of_int empty_count else "" in
          (* Piece represented by a character *)
          let piece_str = String.make 1 (char_of_piece piece smap) in
          (* Slash at the end of a row *)
          let term_str = if is_end_of_row board pos then "/" else "" in
          let fen = fen ^ empty_str ^ piece_str ^ term_str in
          (fen, 0))
      ("", 0)
      board
  in
  let color_fen = char_of_turn board.iswhite in
  Printf.sprintf "%s %c" pieces_fen color_fen

type move = piece * position [@@deriving show]

type move_list = move list [@@deriving show]

let move_of_notation mn iswhite dmap : (move, exn) result =
  if String.length mn = 3 then
    let c = String.get mn 0 in
    let* vpiece = vpiece_of_char c dmap in
    let piece = if iswhite then White vpiece else Black vpiece in
    let pos = pos_of_string (String.sub mn 1 2) in
    Ok (piece, pos)
  else
    Error (Parse_error "A move must be 3 characters")

type ply = move * move option [@@deriving show]

let filter_emptystr = List.filter (( <> ) "")

let ply_of_notation pln dmap : (ply, exn) result =
  (* Split by space into tokens then strip out empty tokens *)
  let tokens = filter_emptystr (String.split_on_char ' ' pln) in
  match tokens with
  | [ wm; bm ] ->
    let* wm = move_of_notation wm true dmap in
    let* bm = move_of_notation bm false dmap in
    Ok (wm, Some bm)
  | [ wm ] ->
    let* m = move_of_notation wm true dmap in
    Ok (m, None)
  | _ -> Error (Parse_error "A ply pgn must have 1 or 2 tokens")

let split_on_number s = filter_emptystr (Str.split (Str.regexp "[0-9]+\\.") s)

let moves_of_notation pgn dmap : (move list, exn) result =
  let tokens = split_on_number pgn in
  let* acc =
    fold_result
      (fun acc pgn ->
        let* ply = ply_of_notation pgn dmap in
        match ply with
        | wm, Some bm -> Ok (wm :: bm :: acc)
        | wm, None -> Ok (wm :: acc))
      []
      tokens
  in
  Ok acc
