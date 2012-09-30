(*
http://cp1.nintendo.co.jp/{Code Puzzle 4の次}
*)
#load "nintendoCodePuzzle1.fsx"

open NintendoCodePuzzle1

let solve() =
    Seq.initInfinite id |> Seq.find (fun i -> lets_take_tea_break (bigint i) 1985I 33067I 84I)
