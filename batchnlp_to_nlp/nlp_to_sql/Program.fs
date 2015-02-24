
open System.Text.RegularExpressions

//For example, elements containing curly braces, commas (or the data type's delimiter 
//character), double quotes, backslashes, or leading or trailing whitespace must be 
//double-quoted. Empty strings and strings matching the word NULL must be quoted, too. 

let escape_pgsql str =
    let s1 = Regex.Replace(str, "\"", "\\\"")
    let s2 = Regex.Replace(str, "\\\\", "\\\\")
    "\"" + s2 + "\""
;;

type Word = 
    {    
    id : int;
    word : string;
    pos: string;
    ner: string;
    lemma: string;
    dep_label: string;
    dep_id: string;
    sent_id: int;
    prov: string;
    }
    member this.ToString sentid =
        [|string this.id;this.word;this.pos;this.ner;this.lemma;this.dep_label;this.dep_id;("SENT_" + (string sentid));this.prov|]
        |> String.concat "\t"
;;

let _words_to_str words =
    let ids = [for Some(w) in words -> string w.id]
    let ws = [for Some(w) in words -> w.word]
    let poss = [for Some(w) in words -> w.pos]
    let ners = [for Some(w) in words -> w.ner]
    let lemmas = [for Some(w) in words -> w.lemma]
    let dep_labels = [for Some(w) in words -> w.dep_label]
    let dep_ids = [for Some(w) in words -> w.dep_id]
    [|ids; ws; poss; ners; lemmas; dep_labels; dep_ids|]
    |> Array.toSeq
    |> Seq.map (fun values -> "{" + (values |> Seq.map (fun v -> escape_pgsql v) |> String.concat ",") + "}")
    |> String.concat "\t"
;;

let _do filenames chunkid =

    System.IO.File.WriteAllLines((string chunkid)+".sql",
        filenames
        |> Seq.map (fun (filename:string) ->
            let docid : string = Seq.last (Array.toSeq (filename.Split '/'))
            System.IO.File.ReadLines(filename)
            |> Seq.map (fun line -> line.Split '\t')
            |> Seq.map (fun columns -> 
                match columns with
                | [|id;word;pos;ner;lemma;dep_label;dep_id;sent_id;prov|] -> 
                    Some({id=int id;word=word;pos=pos;ner=ner;lemma=lemma;dep_label=dep_label;dep_id=dep_id;
                    sent_id=(int (sent_id.Replace("SENT_", "")));prov=prov;})
                | [|""|] -> None
                | _ -> failwith "Error"
            )
            |> Seq.filter (fun word -> word.IsSome)
            |> Seq.groupBy (fun word -> match word with | Some(word) -> word.sent_id |_ -> failwith "Error")
            |> Seq.map (fun (sent_id, words) -> 
                [|(escape_pgsql docid);(string sent_id);(_words_to_str words)|] |> String.concat "\t"
            )
            |> String.concat "\n"
        )
    )
;;

[<EntryPoint>]
let main argv = 
   
    let r = System.Random()

    //argv
    [|"/Users/czhang/Desktop/code/scripts/batchnlp_to_nlp/batchnlp_to_nlp/bin/Debug/rs"|]
    |> Seq.iter (fun dirname ->
       System.IO.Directory.GetFiles(dirname)
       |> Array.toSeq
       |> Seq.groupBy (fun _ -> (r.Next() % 10))
       |> Seq.iter (fun (chunk_id,filenames) -> _do filenames chunk_id)
    )

    0 // return an integer exit code

