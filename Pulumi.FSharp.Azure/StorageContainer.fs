[<AutoOpen>]
module Pulumi.FSharp.Azure.Legacy.StorageContainer

open Pulumi.FSharp.Azure.Common
open Pulumi.Azure.Storage
open Pulumi.FSharp

type Access =
    | Private

module StorageContainerInternal =
    type StorageContainerArgsRecord = {
        Name: string
        StorageAccount: IOArg<Account>
        Access: Access
        ContainerName: string option
    }

open StorageContainerInternal

type StorageContainerBuilder internal () =
    member __.Yield _ = {
        Name = ""
        StorageAccount = Name ""
        Access = Private
        ContainerName = None
    }

    [<CustomOperation("name")>]
    member __.Name(args : StorageContainerArgsRecord, name) = { args with Name = name }
    
    [<CustomOperation("access")>]
    member __.Access(args, access) = { args with Access = access }
    
    [<CustomOperation("containerName")>]
    member __.ContainerName(args, name) = { args with ContainerName = Some name }
   
    member __.Run (args : StorageContainerArgsRecord) =
         ContainerArgs(ContainerAccessType = input (match args.Access with | Private -> "private"),
                       Name = (Option.map input args.ContainerName |> Option.defaultValue null),
                       StorageAccountName = getName args.StorageAccount) |>
         fun sca -> Container(args.Name, sca)

    [<CustomOperation("account")>]
    member __.StorageAccount(args, storageAccount) = {
        args with StorageAccount = Object storageAccount
    }
    
    member __.StorageAccount(args, storageAccount) = {
        args with StorageAccount = Name storageAccount
    }
    
    // Support all Input and Output (and string) types without overloading
    member __.StorageAccount(args, storageAccount) = {
        args with StorageAccount = IO storageAccount
    } 

let storageContainer = StorageContainerBuilder()