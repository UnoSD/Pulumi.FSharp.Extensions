module Pulumi.FSharp.AzureStorageSasToken

open Pulumi.Azure.Storage.Inputs
open Pulumi.FSharp.Outputs
open Pulumi.Azure.Storage
open System

type TimeRange = {
    From: DateTime
    To: DateTime
}

type Permission =
    | Add
    | List
    | Read
    | Write
    | Create
    | Delete

module SasTokenInternal =    
    type StorageBlobArgsRecord = {
        StorageAccount: Account
        StorageContainer: Container
        Blob: Blob
        TimeRange: TimeRange option
        Permissions: Permission list
    }

open SasTokenInternal

type SasTokenBuilder internal () =
    
    let granularSas args range permissions =
        output {
            let! connectionString =
                args.StorageAccount.PrimaryConnectionString
            
            let! containerName =
                args.StorageContainer.Name
            
            let! result = 
                GetAccountBlobContainerSASArgs(
                    ConnectionString = connectionString,
                    ContainerName = containerName,
                    Start = range.From.ToString("u").Replace(' ', 'T'),
                    Expiry = range.To.ToString("u").Replace(' ', 'T'),
                    Permissions = permissions
                ) |>
                GetAccountBlobContainerSAS.InvokeAsync            
        
            return result.Sas
        }
    
    let toArgs permissions =
        let has permission =
            permissions |>
            List.contains permission
        
        GetAccountBlobContainerSASPermissionsArgs(
            Add = (has Add),
            List = (has List),
            Read = (has Read),
            Write = (has Write),
            Create = (has Create),
            Delete = (has Delete)
        )
    
    let defaultDuration = {
        From = DateTime.Now
        To = DateTime.Now.AddDays(1.)
    }
    
    member __.Yield _ = {
        StorageAccount = null
        StorageContainer = null
        Blob = null
        TimeRange = None
        Permissions = []
    }
    
    member __.Run (args) =
         let token =
            match args.TimeRange, args.Permissions with
            | None, []
            | None, [ Read ]    -> SharedAccessSignature.SignedBlobReadUrl(args.Blob,
                                                                           args.StorageAccount)
            | Some range, perms -> granularSas args range (perms |> toArgs)
            | None, perms       -> granularSas args defaultDuration (perms |> toArgs)
         
         secretOutput {
             return! token
         }
    
    [<CustomOperation("blob")>]
    member __.Blob(args, blob) =
        { args with Blob = blob }

    [<CustomOperation("account")>]
    member __.Account(args, storageAccount) =
        { args with StorageAccount = storageAccount }
        
    [<CustomOperation("container")>]
    member __.Container(args, storageContainer) =
        { args with StorageContainer = storageContainer }

    [<CustomOperation("duration")>]
    member __.Duration(args, range) =
        { args with TimeRange = Some range }
        
    [<CustomOperation("permission")>]
    member __.Permission(args, permission) =
        { args with Permissions = permission :: args.Permissions }
        
    [<CustomOperation("permissions")>]
    member __.Permissions(args, permissions) =
        { args with Permissions = args.Permissions @ permissions }

let sasToken = SasTokenBuilder()