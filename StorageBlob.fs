[<AutoOpen>]
module Pulumi.FSharp.Azure.Legacy.StorageBlob

open Pulumi.FSharp.Azure.Common
open Pulumi.Azure.Storage
open Pulumi.FSharp
open Pulumi

type BlobType =
    | Block

module StorageBlobInternal =    
    type StorageBlobArgsRecord = {
        Name: string
        BlobName: string option
        StorageAccount: IOArg<Account>
        StorageContainer: IOArg<Container>
        Type: BlobType
        Source: Input<AssetOrArchive>
    }

open StorageBlobInternal

let private run args =
    BlobArgs(
        Name = (match args.BlobName with
                | None      -> null
                | Some name -> input name),
        StorageAccountName = getName args.StorageAccount,
        StorageContainerName = getName args.StorageContainer,
        Type = input (match args.Type with | Block -> "Block"),
        Source = args.Source
    )
    |> fun ba -> Blob(args.Name, ba)

type StorageBlobBuilder internal () =
    member __.Yield _ = {
        Name = ""
        BlobName = None
        StorageAccount = Name ""
        StorageContainer = Name ""
        Type = Block
        Source = null
    }

    [<CustomOperation("blobName")>]
    member __.BlobName(args, blobName) =
        { args with BlobName = match blobName with
                               | null
                               | ""   -> None
                               | name -> Some name }
    
    [<CustomOperation("name")>]
    member __.Name(args : StorageBlobArgsRecord, name) =
        { args with Name = name }
    
    [<CustomOperation("blobType")>]
    member __.Access(args, blobType) =
        { args with Type = blobType }
    
    [<CustomOperation("container")>]
    member __.Container(args, container) =
        { args with StorageContainer = Object container }
    
    member __.Container(args, containerName) =
        { args with StorageContainer = Name containerName }
   
    [<CustomOperation("source")>]
    member __.Source(args, source) =
        { args with Source = input source }
        
    member __.Source(args, source) =
        { args with Source = io source }
        
    member __.Source(args, source : FileAsset) =
        { args with Source = (source :> AssetOrArchive |> input) }
        
    member __.Source(args, sourcePath) =
        { args with Source = (sourcePath |> FileAsset :> AssetOrArchive |> input) }
    
    [<CustomOperation("content")>]
    member __.Content(args, content) =
        { args with Source = (content |> StringAsset :> AssetOrArchive |> input) }
    
    member __.Run (args) =
         run args

    [<CustomOperation("account")>]
    member __.StorageAccount(args : StorageBlobArgsRecord, storageAccount) = {
        args with StorageAccount = Object storageAccount
    }
    
    member __.StorageAccount(args : StorageBlobArgsRecord, storageAccount) = {
        args with StorageAccount = Name storageAccount
    }
    
    // Support all Input and Output (and string) types without overloading
    member __.StorageAccount(args : StorageBlobArgsRecord, storageAccount) = {
        args with StorageAccount = IO storageAccount
    } 

let storageBlob = StorageBlobBuilder()