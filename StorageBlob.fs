[<AutoOpen>]
module Pulumi.FSharp.Azure.StorageBlob

open Pulumi.FSharp.Azure.Core
open Pulumi.Azure.Storage
open Pulumi.FSharp
open Pulumi

type BlobType =
    | Block

module StorageBlobInternal =    
    type StorageBlobArgsRecord = {
        Name: string
        StorageAccount: IOArg<Account>
        StorageContainer: IOArg<Container>
        Type: BlobType
        Source: Input<AssetOrArchive>
    }

open StorageBlobInternal

let private run args =
    BlobArgs(
        StorageAccountName = getName args.StorageAccount,
        StorageContainerName = getName args.StorageContainer,
        Type = input (match args.Type with | Block -> "Block"),
        Source = args.Source
    ) |>
    fun ba -> Blob(args.Name, ba)

type StorageBlobBuilder internal () =
    member __.Yield _ = {
        Name = ""
        StorageAccount = Name ""
        StorageContainer = Name ""
        Type = Block
        Source = null
    }

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
        { args with Source = source }
    
    member __.Run (args) =
         run args

    [<CustomOperation("storageAccount")>]
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