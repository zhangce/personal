open System.Text.RegularExpressions
open System.Collections.Concurrent
open System.Threading.Tasks

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

type Sentence = 
    {
    words : Word list;
    doc_id: string;
    least_sentid_in_doc: int;
    sent_id: int;
    }
    member this.ToString =
        this.words 
        |> List.sortBy (fun word -> word.id)
        |> List.map (fun word -> word.ToString (this.sent_id-this.least_sentid_in_doc+1))
        |> String.concat "\n"
;;

type State = { 
   current_doc_id : string;
   least_sentid_in_current_doc : int;
};;

let _do filename =
    let sents =
        System.IO.File.ReadLines(filename)
        |> Seq.map (fun line -> line.Split '\t')
        |> Seq.map (fun fields ->
            match fields with
            | [|id;word;pos;ner;lemma;dep_label;dep_id;sent_id;prov|] -> 
                Some({id=int id;word=word;pos=pos;ner=ner;lemma=lemma;dep_label=dep_label;dep_id=dep_id;
                sent_id=(int (sent_id.Replace("SENT_", "")));prov=prov;})
            | [|""|] -> None
            | _ -> failwith "Error"
            )
        |> Seq.fold (fun sents word ->
            match sents with
            | sent :: ss ->
                match word with
                | Some(word) -> {sent with words=(word::sent.words); sent_id=word.sent_id;} :: ss
                | None -> {words=[];doc_id="";least_sentid_in_doc=(-1);sent_id=(-1)} :: sents
            | [] ->
                match word with
                | Some(word) -> {words=[word;];doc_id="";least_sentid_in_doc=(-1);sent_id=word.sent_id;} :: []
                | None -> failwith "Error"
            ) []
         |> List.sortBy (fun sent -> sent.sent_id)
    
    let assign_sent_docid sents =
        let rec assign_sent_docid' sents state rs =
            match sents with
            | sent :: ss ->
                match List.rev(sent.words) with
                | {id=1;word="WKU"} :: ws ->
                    match ws with
                    | w2 :: ws -> 
                        let new_doc_id = Regex.Replace(w2.word, "[^0-9]", "")
                        (assign_sent_docid' ss {state with current_doc_id=new_doc_id; least_sentid_in_current_doc=w2.sent_id}
                        ({sent with doc_id=new_doc_id; least_sentid_in_doc=w2.sent_id}::rs))
                    | _ -> failwith "Error"
                | ws -> 
                    assign_sent_docid' ss state
                        ({sent with doc_id=state.current_doc_id; least_sentid_in_doc=state.least_sentid_in_current_doc}::rs)
            | [] -> rs
        List.rev (assign_sent_docid' sents {current_doc_id="";least_sentid_in_current_doc=(-1)} [])

    assign_sent_docid sents
    |> List.toSeq
    |> Seq.groupBy (fun sent -> sent.doc_id)
    |> Seq.iter (fun (docid, sents) ->
            System.IO.File.WriteAllLines("rs/" + docid + ".nlp",
                sents |> Seq.map (fun sent -> sent.ToString + "\n")
            )
        )
;;

[<EntryPoint>]
let main args = 
    let rs = Parallel.ForEach(args, (fun j ->
        _do j
    ))
    0 
