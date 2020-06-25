namespace Pulumi.FSharp.Azure

module Tags =
    
    ()
    // Avoid repetition of set tags code on all resources
    (*type ITags =
        [<CustomOperation("tags")>]
        member inline __.Tags(args : ^a when ^a : (member SetTags : ^a -> ^a),
                              tags) =
            args.SetTags(tags)
        
        member inline __.Tags(args : ^a when ^a : (member SetTags : ^a -> ^a),
                              tags) =
            args.SetTags(tags |>
                         List.map (fun (n, v) -> (n, input v)))*)