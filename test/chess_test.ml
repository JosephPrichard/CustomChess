open Customchess

let () =
  let pgn = "1.Pd4 Nf6 2.Nf3 d5 3.e3 Bf5 4.c4 c6 5.Nc3 e6" in
  let expected_moves = [] in
  match Serialize.moves_of_notation pgn with
  | Ok actual_moves -> assert (3 > 2)
  | Error exn -> raise exn
