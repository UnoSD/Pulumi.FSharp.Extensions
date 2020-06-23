namespace Pulumi.FSharp.Azure

open Pulumi.Azure.Storage
open Pulumi.FSharp
open Pulumi

[<AutoOpen>]
module StorageContainer =
    type StorageAccountArg =
        | Object of Account
        | Name of string
        | IO of Output<string>
    
    type Access =
        | Private
    
    type StorageContainerArgsRecord = {
        Name: string
        StorageAccount: StorageAccountArg
        Access: Access
        ContainerName: string
    }

    type StorageContainerBuilder internal () =
        member __.Yield _ = {
            Name = ""
            StorageAccount = Name ""
            Access = Private
            ContainerName = ""
        }

        [<CustomOperation("name")>]
        member __.Name(args : StorageContainerArgsRecord, name) = { args with Name = name }
        
        [<CustomOperation("access")>]
        member __.Access(args, access) = { args with Access = access }
        
        [<CustomOperation("containerName")>]
        member __.ContainerName(args, name) = { args with ContainerName = name }
       
        member __.Run (args : StorageContainerArgsRecord) =
             ContainerArgs(ContainerAccessType = input (match args.Access with | Private -> "Standard"),
                           StorageAccountName = (match args.StorageAccount with
                                                 | Object sa -> io sa.Name
                                                 | Name n -> input n
                                                 | IO i -> io i)) |>
             fun sca -> Container(args.Name, sca)

        [<CustomOperation("storageAccount")>]
        member __.StorageAccount(args, storageAccount) = {
            args with StorageAccount = Object storageAccount
        }
        
        [<CustomOperation("storageAccountName")>]
        member __.StorageAccountName(args, storageAccount) = {
            args with StorageAccount = Name storageAccount
        }
        
        // Support all Input and Output (and string) types without overloading
        [<CustomOperation("storageAccountInput")>]
        member __.StorageAccountInput(args, storageAccount) = {
            args with StorageAccount = IO storageAccount
        } 

    let storageContainer = StorageContainerBuilder()