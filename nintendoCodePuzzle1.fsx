(*
http://cp1.nintendo.co.jp/
*)

let lets_take_tea_break m e n c = bigint.ModPow(m, e, n) = c

let solve() =
    Seq.initInfinite id |> Seq.find (fun i -> lets_take_tea_break (bigint i) 17I 3569I 915I)
