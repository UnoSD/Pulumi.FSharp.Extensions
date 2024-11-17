module Pulumi.FSharp.Core

open System
open Pulumi
open Pulumi.FSharp

type CustomTimeoutsBuilder() =
    member _.Yield(_: unit) = [ id ]

    member _.Run(opts) =
        List.fold (fun opts f -> f opts) (CustomTimeouts()) opts

    member this.Combine(lOpts, rOpts) = lOpts @ rOpts
    member this.For(opts, delayedOpts) = this.Combine(opts, delayedOpts ())
    member _.Delay(f) = f ()
    member _.Zero _ = ()
    
    /// The optional create timeout.
    [<CustomOperation("create")>]
    member _.Create(opts, value) =
        (fun (opts: CustomTimeouts) ->
            opts.Create <- Nullable value
            opts) :: opts

    /// The optional update timeout.
    [<CustomOperation("update")>]
    member _.Update(opts, value) =
        (fun (opts: CustomTimeouts) ->
            opts.Update <- Nullable value
            opts) :: opts

    /// The optional delete timeout.
    [<CustomOperation("delete")>]
    member _.Delete(opts, value) =
        (fun (opts: CustomTimeouts) ->
            opts.Delete <- Nullable value
            opts) :: opts

let customTimeouts = CustomTimeoutsBuilder()
            
type CustomResourceOptionsBuilder() =
    member _.Yield(_: unit) = [ id ]

    member _.Run(opts) =
        List.fold (fun opts f -> f opts) (CustomResourceOptions()) opts

    member this.Combine(lOpts, rOpts) = lOpts @ rOpts
    member this.For(opts, delayedOpts) = this.Combine(opts, delayedOpts ())
    member _.Delay(f) = f ()
    member _.Zero _ = ()

    /// When set to <c>true</c>, indicates that this resource should be deleted before its
    /// replacement is created when replacement is necessary.
    [<CustomOperation("deleteBeforeReplace")>]
    member _.DeleteBeforeReplace(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.DeleteBeforeReplace <- Nullable value
            opts) :: opts

    /// The names of outputs for this resource that should be treated as secrets. This augments
    /// the list that the resource provider and pulumi engine already determine based on inputs
    /// to your resource. It can be used to mark certain outputs as a secrets on a per resource
    /// basis.
    [<CustomOperation("additionalSecretOutputs")>]
    member _.AdditionalSecretOutputs(opts, value : #seq<_>) =
        (fun (opts: CustomResourceOptions) ->
            opts.AdditionalSecretOutputs <- ResizeArray value
            opts) :: opts

    /// The names of outputs for this resource that should be treated as secrets. This augments
    /// the list that the resource provider and pulumi engine already determine based on inputs
    /// to your resource. It can be used to mark certain outputs as a secrets on a per resource
    /// basis.
    member _.AdditionalSecretOutputs(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.AdditionalSecretOutputs <- ResizeArray [ value ]
            opts) :: opts

    /// When provided with a resource ID, import indicates that this resource's provider should
    /// import its state from the cloud resource with the given ID.The inputs to the resource's
    /// constructor must align with the resource's current state.Once a resource has been
    /// imported, the import property must be removed from the resource's options.
    [<CustomOperation("importId")>]
    member _.ImportId(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.ImportId <- value
            opts) :: opts

    /// An optional existing ID to load, rather than create.
    [<CustomOperation("id")>]
    member _.Id(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Id <- input value
            opts) :: opts

    /// An optional existing ID to load, rather than create.
    member _.Id(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Id <- io value
            opts) :: opts

    /// An optional parent resource to which this resource belongs.
    [<CustomOperation("parent")>]
    member _.Parent(opts, value) =
        (fun (opts: CustomResourceOptions) ->
             opts.Parent <- value
             opts) :: opts

    /// Optional additional explicit dependencies on other resources.
    [<CustomOperation("dependsOn")>]
    member _.DependsOn(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.DependsOn <- inputList value
            opts) :: opts

    /// Optional additional explicit dependencies on other resources.
    member _.DependsOn(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.DependsOn <- inputList [ input value ]
            opts) :: opts

    /// Optional additional explicit dependencies on other resources.
    member _.DependsOn(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.DependsOn <- inputList [ io value ]
            opts) :: opts

    /// Optional additional explicit dependencies on other resources.
    member _.DependsOn(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.DependsOn <- inputList (Seq.map input value)
            opts) :: opts

    /// Optional additional explicit dependencies on other resources.
    member _.DependsOn(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.DependsOn <- inputList (Seq.map io value)
            opts) :: opts

    /// When set to true, protect ensures this resource cannot be deleted.
    [<CustomOperation("protect")>]
    member _.Protect(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Protect <- Nullable value
            opts) :: opts

    /// Ignore changes to any of the specified properties.
    [<CustomOperation("ignoreChanges")>]
    member _.IgnoreChanges(opts, value : #seq<_>) =
        (fun (opts: CustomResourceOptions) ->
            opts.IgnoreChanges <- ResizeArray value
            opts) :: opts

    /// Ignore changes to any of the specified properties.
    member _.IgnoreChanges(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.IgnoreChanges <- ResizeArray [ value ]
            opts) :: opts

    /// An optional version, corresponding to the version of the provider plugin that should be
    /// used when operating on this resource. This version overrides the version information
    /// inferred from the current package and should rarely be used.
    [<CustomOperation("version")>]
    member _.Version(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Version <- value
            opts) :: opts

    /// An optional provider to use for this resource's CRUD operations. If no provider is
    /// supplied, the default provider for the resource's package will be used. The default
    /// provider is pulled from the parent's provider bag (see also
    /// ComponentResourceOptions.providers).
    ///
    /// If this is a <see cref="ComponentResourceOptions"/> do not provide both <see
    /// cref="Provider"/> and <see cref="ComponentResourceOptions.Providers"/>.
    [<CustomOperation("provider")>]
    member _.Provider(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Provider <- value
            opts) :: opts

    member _.Yield(value) =
        [fun (opts: CustomResourceOptions) ->
            opts.CustomTimeouts <- value
            opts]

    /// Optional list of transformations to apply to this resource during construction.The
    /// transformations are applied in order, and are applied prior to transformation applied to
    /// parents walking from the resource up to the stack.
    [<CustomOperation("resourceTransformations")>]
    member _.ResourceTransformations(opts, value : #seq<_>) =
        (fun (opts: CustomResourceOptions) ->
            opts.ResourceTransformations <- ResizeArray value
            opts) :: opts
            
    /// Optional list of transformations to apply to this resource during construction.The
    /// transformations are applied in order, and are applied prior to transformation applied to
    /// parents walking from the resource up to the stack.
    member _.ResourceTransformations(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.ResourceTransformations <- ResizeArray [ value ]
            opts) :: opts

    /// An optional list of aliases to treat this resource as matching.
    [<CustomOperation("aliases")>]
    member _.Aliases(opts, value : #seq<_>) =
        (fun (opts: CustomResourceOptions) ->
            opts.Aliases <- ResizeArray value
            opts) :: opts

    /// An optional list of aliases to treat this resource as matching.
    member _.Aliases(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Aliases <- ResizeArray [ input value ]
            opts) :: opts

    /// An optional list of aliases to treat this resource as matching.
    member _.Aliases(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Aliases <- ResizeArray [ io value ]
            opts) :: opts

    /// An optional list of aliases to treat this resource as matching.
    member _.Aliases(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Aliases <- ResizeArray (Seq.map input value)
            opts) :: opts

    /// An optional list of aliases to treat this resource as matching.
    member _.Aliases(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Aliases <- ResizeArray (Seq.map io value)
            opts) :: opts

    /// The URN of a previously-registered resource of this type to read from the engine.
    [<CustomOperation("urn")>]
    member _.Urn(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.Urn <- value
            opts) :: opts

    /// Changes to any of these property paths will force a replacement.  If this list
    /// includes `"*"`, changes to any properties will force a replacement.  Initialization
    /// errors from previous deployments will require replacement instead of update only if
    /// `"*"` is passed.
    [<CustomOperation("replaceOnChanges")>]
    member _.ReplaceOnChanges(opts, value : #seq<_>) =
        (fun (opts: CustomResourceOptions) ->
            opts.ReplaceOnChanges <- ResizeArray value
            opts) :: opts

    /// Changes to any of these property paths will force a replacement.  If this list
    /// includes `"*"`, changes to any properties will force a replacement.  Initialization
    /// errors from previous deployments will require replacement instead of update only if
    /// `"*"` is passed.
    member _.ReplaceOnChanges(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.ReplaceOnChanges <- ResizeArray [ value ]
            opts) :: opts

    /// An optional URL, corresponding to the url from which the provider plugin that should be
    /// used when operating on this resource is downloaded from. This URL overrides the download URL
    /// inferred from the current package and should rarely be used.
    [<CustomOperation("pluginDownloadURL")>]
    member _.PluginDownloadURL(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.PluginDownloadURL <- value
            opts) :: opts

    /// If set to True, the providers Delete method will not be called for this resource.
    [<CustomOperation("retainOnDelete")>]
    member _.RetainOnDelete(opts, value) =
        (fun (opts: CustomResourceOptions) ->
            opts.RetainOnDelete <- Nullable value
            opts) :: opts

let customResourceOptions = CustomResourceOptionsBuilder()
