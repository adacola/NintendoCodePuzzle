(*
http://cp1.nintendo.co.jp/{Code Puzzle 2の答え}
*)
#load "nintendoCodePuzzle2.fsx"

open NintendoCodePuzzle2

let makeBars = makeRotateReverseBars [|' ', (' ', 0); 'i', ('T', 1); 'I', ('I', 1); 'T', ('i', 0)|]

open System.Collections.Generic

let decode_morse =
    let morses =
        dict [
            "iI", "a"
            "Ii", "n"
            "Iiii", "b"
            "III", "o"
            "IiIi", "c"
            "iIIi", "p"
            "Iii", "d"
            "IIiI", "q"
            "i", "e"
            "iIi", "r"
            "iiIi", "f"
            "iii", "s"
            "IIi", "g"
            "I", "t"
            "iiii", "h"
            "iiI", "u"
            "ii", "i"
            "iiiI", "v"
            "iIII", "j"
            "iII", "w"
            "IiI", "k"
            "IiiI", "x"
            "iIii", "l"
            "IiII", "y"
            "II", "m"
            "IIii", "z"
        ]
    fun (str : string) ->
        str.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun word -> morses.[word]) |> String.concat ""

let solve() =
    let bs =
        (makeBars "I    IT ii  i I   I i   i   I  T", [1 .. 26]) ||> List.fold (fun bs _ -> printfn "%O" bs; bs.Next())
    printfn "%O" bs
    bs |> string |> decode_morse |> printfn "answer: %s"
