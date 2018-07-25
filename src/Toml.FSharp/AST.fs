module TomlFSharp.AST
#nowarn "62"

open System
open System.Text
open System.Collections.Generic
open TomlFSharp.Prelude

type ('key,'value) Dictionary with
    member self.TryAdd (key,value) =
        if self.ContainsKey key then false else self.Add (key,value); true

    member self.AddSeq xs = Seq.iter self.Add xs

    member self.AddOnlyNewKeys ls = ls |> List.iter (self.TryAdd>>ignore)


let inline private seqstr xs (modstr:string) =
    let sb = System.Text.StringBuilder()
    xs |> Seq.iter (fun x -> sb.Append(string x).Append(",").Append(modstr)|>ignore)
    string sb  


type [<RequireQualifiedAccess>] 
    Key = 
    | Bare of string | Quoted of string
    override key.ToString () = key |> function Bare k -> k | Quoted k -> k 

and [<RequireQualifiedAccess>] 
    //[<StructuredFormatDisplay "{Display}">]
    Value =
    | String        of string
    | Int           of int64
    | Float         of float
    | Bool          of bool
    | DateTime      of DateTime 
    | Array         of Value list
    | ArrayOfTables of Table list
    | InlineTable   of Table
    override value.ToString () =
        match value with
        | String v          -> v
        | Int v             -> string v
        | Float v           -> string v
        | Bool v            -> string v
        | DateTime v        -> string v
        | InlineTable v     -> v.InlineTableString()
        | Array vs          -> 
            match vs with
            | InlineTable _ ::_ -> vs |> List.map string |> String.concat "; " |> sprintf "[| %s\n|]" 
            | _ -> vs |> List.map string |> String.concat "; " |> sprintf "[| %s |]" 
        | ArrayOfTables vs  -> 
            vs |> List.map string |> String.concat ";\n"
            |> String.indent 2|> sprintf "[|\n%s\n|]"
and Table () = 
    let tables      = Dictionary<string,Table>()
    let elems       = Dictionary<string,Value>()
    let contains    = HashSet<string>()

    member private __.Elems       = elems
    member private __.Tables      = tables
    member private __.ContainSet  = contains

    member self.ContainsKey key = self.ContainSet.Contains key 

    member self.Add (key:string,table:Table) = 
        if key.IndexOf "." = -1 then
            if  not (self.ContainSet.Add key) && 
                self.Tables.ContainsKey key  then false else
//            if not (self.ContainSet.Add key) then false else
            self.Tables.Add (key,table)
            true
        else
        let keys = keyToList key
        let rec loop (keys:string list) (curTable:Table) =
            match keys with
            | [hd] -> curTable.Add( hd,Table())
            | hd::tl -> 
                if curTable.Tables.ContainsKey ( hd) then 
                    loop tl curTable.[hd]
                else
                    let tbl = Table() in 
                    curTable.Add( hd,tbl) |> ignore
                    loop tl tbl  
            | [] -> false
        loop keys self

    member self.Add (key:string,value:Value) = 
        if key.IndexOf "." = -1 then
            if not (self.ContainSet.Add key) then false else
            self.Elems.Add (key,value)
            true
        else
        let tbl = parentKey key
        if self.ContainsKey tbl then () else self.Add (tbl,Table())|>ignore
        let keys = keyToList key
        let rec loop (keys:string list) (curTable:Table) =
            match keys with
            | [hd] -> 
                if not (curTable.Elems.ContainsKey hd) then 
                    curTable.Elems.Add (hd,value) 
                    true
                else false
            | hd::tl ->  
                if not (curTable.Tables.ContainsKey hd) then
                    curTable.[hd] <- Table()
                loop tl curTable.[hd] 
            | [] -> false
        loop keys self 

    member __.SubTables() = tables |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    member __.Item 
        with get key    = tables.[key]
        and  set key v  = tables.[key] <- v
    
    member __.Elem
        with get key    = elems.[key]
        and  set key v  = elems.[key] <- v
    
    member __.IsEmpty 
        with get () = tables.Count = 0 && elems.Count = 0 

    member self.AddSeq (kvps:(string*Value)seq) = 
        kvps |> Seq.iter (self.Add>>ignore)

    new (kvps:(string*Value)seq) as self = 
        Table () then kvps |> Seq.iter self.Elems.Add
        
    new (key:string, table:Table) as self = 
        Table () then self.Tables.Add (key,table)

    member self.InlineTableString() =
        let sb = StringBuilder ()
        let maxklen, _ = findKVPMaxes (elems :> seq<_>)
        elems |> Seq.iteri (fun idx topkvp -> 
            match topkvp.Value with
            | Value.ArrayOfTables tbls ->
                sb.AppendLine (sprintf "%-*s = [|" maxklen topkvp.Key )|>ignore
                tbls |> List.iter (fun elems ->
                    let eb = StringBuilder()
                    eb.AppendLine(sprintf "[[%s]] {" topkvp.Key)|>ignore
                    elems.Elems |> Seq.iter (fun kvp -> 
                        sprintf "\n%-*s = %s" maxklen kvp.Key (string kvp.Value)|> eb.Append|>ignore)
                    let substr = string eb |> String.indent 3
                    substr |> sb.Append |> ignore
                    "};\n" |> sb.Append |> ignore 
                )
                sb.AppendLine ("|]")|>ignore
            | _ -> 
                if idx = 0 then
                    sprintf "%-*s = %s" maxklen topkvp.Key (string topkvp.Value) |> sb.Append |> ignore 
                else
                    sprintf "\n   %-*s = %s" maxklen topkvp.Key (string topkvp.Value) |> sb.Append |> ignore
        ) 
        // the toplevel kvps are stored in the stringbuilder
        self.SubTables() |> Seq.iter (fun (name,data)  ->
            let substr = sprintf "[%s]\n%s" name (string data) |> String.indent 2
            sb.AppendLine substr|>ignore
            |> ignore)
        (string sb |> String.indent 2).TrimEnd [|'\n';' '|] |> sprintf "\n  {%s  }"         

    override self.ToString () =
        let sb = StringBuilder ()
        sb.AppendLine() |> ignore
        let maxklen, _ = findKVPMaxes (elems :> seq<_>)
        elems |> Seq.iter (fun topkvp -> 
            match topkvp.Value with
            | Value.ArrayOfTables tbls ->
                sb.AppendLine (sprintf "%-*s = [|" maxklen topkvp.Key )|>ignore
                tbls |> List.iter (fun elems ->
                    let eb = StringBuilder()
                    eb.AppendLine(sprintf "[[%s]] {" topkvp.Key)|>ignore
                    elems.Elems |> Seq.iter (fun kvp -> 
                        sprintf "%-*s = %s" maxklen kvp.Key (string kvp.Value)|> eb.AppendLine|>ignore)
                    let substr = string eb |> String.indent 2
                    substr |> sb.Append |> ignore
                    "};\n" |> sb.Append |> ignore 
                )
                sb.AppendLine ("|]")|>ignore
            | _ ->    sprintf "%-*s = %s" maxklen topkvp.Key (string topkvp.Value) |> sb.AppendLine |> ignore) 
        // the toplevel kvps are stored in the stringbuilder
        self.SubTables() |> Seq.iter (fun (name,data) ->
            let substr = sprintf "[%s]\n%s" name (string data) |> String.indent 2
            sb.AppendLine substr|>ignore
            |> ignore)
        (string sb |> String.indent 2).TrimEnd [|'\n';' '|] |> sprintf "{%s\n}" 

