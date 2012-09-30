(*
http://cp1.nintendo.co.jp/{Code Puzzle 3の答え}
*)
#load "nintendoCodePuzzle3.fsx"

open NintendoCodePuzzle3

let solve() =
    let start = "ITT TI I T TIii"
    let barsArray =
        (makeBars "ITT TI I T TIii", true) |> Some |> Seq.unfold (Option.map (fun (bs, isFirst) ->
            let str = bs.ToString()
            str, if str = start && not isFirst then None else Some(bs.Next(), false)))
        |> Seq.toArray
    barsArray |> Array.iter (printfn "%s")
    barsArray.[barsArray.Length - 2] |> decode_morse |> printfn "answer: %s"
