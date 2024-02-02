open Customchess
open Chess
open Lib
open Notation

let () =
  let dmap =
    add_pairs [ ('P', Piece 'a'); ('N', Piece 'b'); ('B', Piece 'c') ] CharMap.empty
  in
  (* Testing if a single move works *)
  let pgn = "Pd4" in
  assert_ok
    ~actual:(move_of_notation pgn true dmap)
    ~expected:(White (Piece 'a'), (3, 3))
    ~show:Notation.show_move;
  (* Testing if a ply works *)
  let pgn = "Pd4 Nf6" in
  assert_ok
    ~actual:(ply_of_notation pgn dmap)
    ~expected:((White (Piece 'a'), (3, 3)), Some (Black (Piece 'b'), (5, 5)))
    ~show:Notation.show_ply;
  (* Testing the split pgn function *)
  let s = "1. g8 2. Foo 3. Bar 4. Hello 5. World" in
  assert_true
    ~actual:(split_on_number s)
    ~expected:[ " g8 "; " Foo "; " Bar "; " Hello "; " World" ]
    ~show:show_str_list;
  (* Testing if a series of moves work *)
  let pgn = "1.Pd4 Nf6 2.Nf3 Pd5 3.Pe3" in
  assert_ok
    ~actual:(moves_of_notation pgn dmap)
    ~expected:
      [ (Chess.White (Chess.Piece 'a'), (4, 2))
      ; (Chess.White (Chess.Piece 'b'), (5, 2))
      ; (Chess.Black (Chess.Piece 'a'), (3, 4))
      ; (Chess.White (Chess.Piece 'a'), (3, 3))
      ; (Chess.Black (Chess.Piece 'b'), (5, 5))
      ]
    ~show:(show_list Notation.show_move)

let () =
  let pieces =
    [| Empty
     ; Empty
     ; Empty
     ; (* End of row *)
       White (Piece 'a')
     ; Black (Piece 'a')
     ; Empty
     ; (* End of row *)
       White King
     ; Empty
     ; Black King
    |]
  in
  let board = { iswhite = false; pieces; dim = 3 } in
  let smap = add_pairs [ ('a', 'A') ] CharMap.empty in
  assert_true ~actual:(fen_of_board board smap) ~expected:"3/Aa2/K1k" ~show:identity
