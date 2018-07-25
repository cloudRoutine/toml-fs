module TomlFSharp.Prelude
open System
(*| Helpers |*)

/// Compose predicates with `&&`
let inline (|&|) (pred1:'a->bool) (pred2:'a->bool) = fun x -> pred1 x && pred2 x

/// Compose predicates with `||`
let inline (|?|) (pred1:'a->bool) (pred2:'a->bool) = fun x -> pred1 x || pred2 x

let inline isNull v = match v with | null -> true | _ -> false


[<RequireQualifiedAccess>] 
module  List =
    let inline last ls = 
        let rec loop ls =
            match ls with
            | x::[] -> x | _::tl -> loop tl | [] -> failwith "empty list has no last member"
        loop ls

[<RequireQualifiedAccess>] 
module  Array =

    let inline private checkNonNull argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()


    let head (array: 'T []) =
         checkNonNull "array" array
         if array.Length = 0 then invalidArg "array" "cannot get the head of an empty array"
         array.[0]


    let inline last (array:'a[]) = array.[array.Length-1]


    /// Return an array of elements that preceded the first element that failed
    /// to satisfy the predicate
    let takeWhile predicate (array: 'T []) =
        checkNonNull "array" array
        if array.Length = 0 then [||] else
        let mutable count = 0
        while count < array.Length-1 && predicate array.[count] do
            count <- count + 1
        array.[0..count]


    /// Return an array of elements that begin at the first element that failed
    /// to satisfy the predicate
    let skipWhile predicate (array: 'T []) =
        checkNonNull "array" array
        if array.Length = 0 then [||] else
        let mutable count = 0
        while count < array.Length-1 && predicate array.[count] do
            count <- count + 1
        array.[count..array.Length-1]


type 'a ``[]`` with
    member array.Last = array.[array.Length-1]

[<RequireQualifiedAccess>] 
module String =
    open System.Text
    /// checks if the front and back of a string match the provided
    /// 'bookend' substrings
    let bookends (front:string) (back:string) (str:string) =
        let flen, blen, slen = front.Length, back.Length, str.Length
        if  flen > slen  || blen > slen || flen + blen > slen 
         || isNull front || isNull back || isNull str then false 
        elif String.Empty = front && String.Empty = back then true else
        str.Substring (0,front.Length) = front &&
        str.Substring (str.Length-back.Length,back.Length) = back

    /// indents every line in the provided string by `num` spaces
    let indent num (str:string) =
        let spc = String.replicate num " "
        str.Split '\n' |> Array.map (fun x -> spc + x) |> String.concat "\n"



(*---------------------------------------------*)
(* Helper functions for working with TOML keys *)
(*---------------------------------------------*)


let inline cleanEnds key =
    if   String.bookends "[[" "]]" key then key.[2..key.Length-3]
    elif String.bookends "["   "]" key then key.[1..key.Length-2]
    else key

let inline listToKey keys = String.concat "." keys
let inline keyToList (key:string) = (cleanEnds key).Split '.' |> Array.toList

let inline keyName (key:string) = 
    let tkey = cleanEnds key in let idx = tkey.LastIndexOf '.' 
    tkey.Substring(idx+1, tkey.Length-idx-1)

/// Retursn the fully qualified name of the parent
let inline parentKey (key:string)  = 
    let tkey = cleanEnds key
    let idx1 = tkey.LastIndexOf '.' 
    if idx1  = -1 then String.Empty else
    tkey.Substring(0,idx1) 

/// Returns the node one level up in the table heirarchy
let inline parentNode (key:string)  = 
    let sub = parentKey key in let idx2 = sub.LastIndexOf '.' 
    if idx2 = -1 then sub else sub.Substring(idx2+1, sub.Length-idx2-1)

let inline getRoot (key:string)  = 
    let tkey = cleanEnds key
    match tkey.IndexOf '.' with
    | -1 -> tkey | idx -> tkey.Substring(0,idx)

let inline removeRoot (key:string)  = 
    let tkey = cleanEnds key
    match tkey.IndexOf '.' with
    | -1 -> tkey | idx -> tkey.Substring(idx+1,tkey.Length-idx-1)


let getNested x = 
    let pkey = keyToList (cleanEnds x) in if pkey = [] then [""] else pkey      

open System.Collections.Generic

let inline findKVPMaxes (kvps:seq<KeyValuePair<string,_>>) =
    if Seq.length kvps = 0 then 0,0 else
    ((0,0),kvps) 
    ||> Seq.fold (fun (kmax,vmax) kvp ->
        max kmax kvp.Key.Length, max vmax (string kvp.Value).Length)