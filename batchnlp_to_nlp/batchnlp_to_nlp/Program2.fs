

[<EntryPoint>]
let main args = 

    let rs = Parallel.ForEach(args, (fun j ->
        _do j
    ))

    0 
