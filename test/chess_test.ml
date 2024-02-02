open Customchess
open Chess
open Lib
open Notation
open OUnit2

let () =
  let dmap =
    add_pairs
      [ ('P', Piece 'a'); ('N', Piece 'b'); ('B', Piece 'c'); ('Q', Piece 'd') ]
      CharMap.empty
  in
  let smap = add_pairs [ ('a', 'A') ] CharMap.empty in
  (*Tests and test suite*)
  let tests =
    [ ("move_of_notation"
       >:: fun _ ->
       let expected = (White (Piece 'a'), (3, 3)) in
       let actual = ok_or_raise (move_of_notation "Pd4" true dmap) in
       assert_equal expected actual ~printer:Notation.show_move)
    ; ("ply_of_notation"
       >:: fun _ ->
       let expected = ((White (Piece 'a'), (3, 3)), Some (Black (Piece 'b'), (5, 5))) in
       let actual = ok_or_raise (ply_of_notation "Pd4 Nf6" dmap) in
       assert_equal expected actual ~printer:Notation.show_ply)
    ; ("split_on_number"
       >:: fun _ ->
       let expected = [ " g8 "; " Foo "; " Bar "; " Hello "; " World" ] in
       let actual = split_on_number "1. g8 2. Foo 3. Bar 4. Hello 5. World" in
       assert_equal expected actual ~printer:Lib.show_string_list)
    ; ("moves_of_notation"
       >:: fun _ ->
       let e =
         [ (White (Piece 'a'), (4, 2))
         ; (White (Piece 'b'), (5, 2))
         ; (Black (Piece 'a'), (3, 4))
         ; (White (Piece 'a'), (3, 3))
         ; (Black (Piece 'b'), (5, 5))
         ]
       in
       let a = ok_or_raise (moves_of_notation "1.Pd4 Nf6 2.Nf3 Pd5 3.Pe3" dmap) in
       assert_equal e a ~printer:(Notation.show_move_list))
    ; ("fen_of_board"
       >:: fun _ ->
       let pieces =
         [| Empty
          ; Empty
          ; Empty
          ; White (Piece 'a')
          ; Black (Piece 'a')
          ; Empty
          ; White King
          ; Empty
          ; Black King
         |]
       in
       let board = { iswhite = false; pieces; dim = 3 } in
       let expected = "3/Aa1/K1k b" in
       let actual = fen_of_board board smap in
       assert_equal expected actual ~printer:identity)
    ]
  in
  let test_suite = "test suite for notation" >::: tests in
  run_test_tt_main test_suite
