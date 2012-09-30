(*
http://cp1.nintendo.co.jp/{Code Puzzle 1の答え}
*)
type BarsBase(next : int -> char[] -> char, str : string) =
    let arr = str.ToCharArray()
    override x.ToString() = arr |> Seq.map string |> String.concat ""
    member x.Length = str.Length
    member x.FixPosition(pos) = (pos + x.Length) % x.Length
    member x.Replace(startIndex, count, replacement) =
        let endIndex = startIndex + count
        if x.Length >= endIndex then
            let s = str |> String.mapi (fun i c -> if startIndex <= i && i < endIndex then replacement else c)
            BarsBase(next, s)
        else
            let over = endIndex - x.Length
            x.Replace(startIndex, count - over, replacement).Replace(0, over, replacement)
    member x.Next() = BarsBase(next, str |> String.mapi (fun i _ -> next i arr))

let makeRotateReverseBars chars str =
    let charMap = Map.ofArray chars
    let nexts = [|
        fun c -> charMap.[c] |> fst
        fun c -> (Array.findIndex (fst >> (=) c) chars + 1) % chars.Length |> Array.get chars |> fst
    |]
    let next i (arr : char[]) =
        let prev, next = arr.[(i - 1 + arr.Length) % arr.Length], arr.[(i + 1) % arr.Length]
        let f = (snd charMap.[prev] + snd charMap.[next]) % 2 |> Array.get nexts
        f arr.[i]
    BarsBase(next, str)

let makeSimpleBars = makeRotateReverseBars [|' ', (' ', 0); 'i', ('T', 1); 'T', ('i', 0)|]

open System.Text.RegularExpressions

let parseCommand length command =
    ((String.replicate length " " |> makeSimpleBars, 0, 1, 1, System.Text.StringBuilder()), command)
    ||> Seq.fold (fun (bs, pos, acc, accx, output) -> function
        | '1' -> bs, pos, 1, accx, output
        | '/' -> bs, pos, acc * 2, accx, output
        | ')' -> bs, pos + acc |> bs.FixPosition, acc, accx, output
        | '(' -> bs, pos - acc |> bs.FixPosition, acc, accx, output
        | ('i' | 'T' | ' ') as c -> bs.Replace(pos, acc, c), pos + acc |> bs.FixPosition, acc, accx, output
        | ']' ->
            let t = string bs in let s = t.[pos ..] + t.[.. pos]
            let m = Regex.Match(s, @"^ *[iT]* ")
            bs, pos, (if m.Success then m.Index + m.Length - 1 else 0), accx, output
        | '[' ->
            let t = string bs in let s = t.[pos - 1 .. pos - 1] + t.[pos ..] + t.[.. pos - 1]
            let m = Regex.Match(s, @" [iT]* *$")
            bs, pos, (if m.Success then s.Length - m.Index - 1 else 0), accx, output
        | 'l' -> bs, pos, accx, acc, output
        | 'L' -> bs, pos, accx - acc, accx + acc, output
        | '|' -> printfn "%O" bs; bs.Next(), pos, acc, accx, output
        | '!' -> bs, pos, acc, accx, output.Append((int '0' + acc) % 128 |> char)
        | _ -> bs, pos, acc, accx, output)
    |> fun (_, _, _, _, output) -> printfn "answer:%O" output

let solve() =
    parseCommand 78 "1(///(1iTiTiTi|||[(1 ])1( [L|[L|[L|[(] |1//)/)1i||1)///)1i||||1(///)1i(/////)1iTiTi[L!])|])[L!])])l|])1/( [(1/ ]L!l|[(1 ])1( //(1 ]L[L!|"
