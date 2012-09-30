#load "nintendoCodePuzzle2.fsx"
#load "nintendoCodePuzzle3.fsx"

open NintendoCodePuzzle2
open NintendoCodePuzzle3

let should = (<|)
let be expected actual = if expected <> actual then failwithf "\nexpected : %A\nbut now : %A" expected actual

let ``actualのNext()をとるとexpectedになる`` (barsBaseConstructor : string -> BarsBase) actuals expecteds =
    List.zip actuals expecteds |> List.iter (fun (actual, expected) ->
        (barsBaseConstructor actual).Next().ToString() |> should be expected)
let ``actualをNext()で連続変換するとexpectedsの順に変化する`` (barsBaseConstructor : string -> BarsBase) actual expecteds =
    (actual, expecteds) ||> List.fold (fun actual expected ->
        let next = (barsBaseConstructor actual).Next().ToString()
        next |> should be expected
        next)
    |> ignore

let simpleBarsTest =
    ``actualのNext()をとるとexpectedになる`` makeSimpleBars
        ["     "; "  i  "; " i i "; "  T  "; " TiT "; " iTi "; " TTT "] ["     "; " iTi "; "iT Ti"; "  i  "; "  T  "; "iTiTi"; " iii "]
    ``actualをNext()で連続変換するとexpectedsの順に変化する`` makeSimpleBars
        "Ti  " [" Ti "; "  Ti"; "i  T"; "Ti  "]
    ``actualをNext()で連続変換するとexpectedsの順に変化する`` makeSimpleBars
        "  iT" [" iT "; "iT  "; "T  i"; "  iT"]

let barsTest =
    ``actualのNext()をとるとexpectedになる`` makeBars
        ["     "; "  i  "; " i i "; "  T  "; " TiT "; " iTi "; " TTT "] ["     "; " iTi "; "iT Ti"; "  i  "; "  T  "; "iTiTi"; " iii "]
    ``actualのNext()をとるとexpectedになる`` makeBars
        [" I  "; " ii "; " Ii "; " TI "; " II "] ["iIi "; "iIIi"; "iTIi"; "  Ii"; "iTTi"]
    ``actualをNext()で連続変換するとexpectedsの順に変化する`` makeBars
        "Ti  " [" Ti "; "  Ti"; "i  T"; "Ti  "]
    ``actualをNext()で連続変換するとexpectedsの順に変化する`` makeBars
        "  iT" [" iT "; "iT  "; "T  i"; "  iT"]
    ``actualをNext()で連続変換するとexpectedsの順に変化する`` makeBars
        "I    IT ii  i I   I i   i   I  T" ["Ii  iI iIIiiT Ii iI Ti iTi iIi  "; "TIiiIT IIITI iTI ITi T TiT IIIii"]
