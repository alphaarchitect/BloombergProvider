namespace AlphaArchitect.Common

open Microsoft.FSharp.Reflection
open System.Reflection

module Union =
    let fromString<'a> (string': string) =
        match
            FSharpType.GetUnionCases(typeof<'a>, BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.filter (fun case -> case.Name = string')
        with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None

module Result =
    let isOk result =
        match result with
        | Ok _ -> true
        | Error _ -> false

    let folder acc x =
        match acc, x with
        | Ok xs, Ok y -> Ok(y :: xs)
        | Error xs, Ok _ -> Error xs
        | Ok _, Error y -> Error [ y ]
        | Error xs, Error y -> Error(y :: xs)
