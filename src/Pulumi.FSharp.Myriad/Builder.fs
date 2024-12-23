module AstBuilder

open Myriad.Core.AstExtensions
open AstOperations
open AstInstance
open AstHelpers
open AstMember
open AstYield
open AstRun
open Core

open System.Text.RegularExpressions
open FSharp.Text.RegexProvider

// "azure:compute/virtualMachine:VirtualMachine"
// CloudProvider - Always the same for each schema (azure here)
type InfoProvider =
    Regex<"(?<CloudProvider>[a-z0-9-]+):(?<ResourceProviderNamespace>[A-Za-z0-9.]+)(/(?<SubNamespace>\w+))?:(?<ResourceType>\w+)">

let typeInfoProvider = InfoProvider(RegexOptions.Compiled)

type BuilderType =
    | Type of InfoProvider.MatchType
    | Resource of InfoProvider.MatchType

let private argIdent = Pat.ident ("arg")

let private argToInput = Expr.app ("input", "arg")

let private args = Expr.ident ("args")

let private funcIdent = Expr.ident ("func")

let private yieldReturnExpr = Expr.list ([ Expr.ident ("id") ])

let private combineExpr = Expr.app ("_combine", "args")

let private combineCrosExpr = Expr.app ("_combineCros", "args")

let private combineArgs = Pat.ident ("args")

let private combineMember =
    createMember' None "this" "Combine" combineArgs [] combineExpr

let private combineCrosMember =
    createMember' None "this" "Combine" combineArgs [] combineCrosExpr

let private forArgs = Pat.paren (Pat.tuple ("args", "delayedArgs"))

let private forExpr =
    Expr.methodCall (
        "this.Combine",
        [
            Expr.ident ("args")
            Expr.app ("delayedArgs", Expr.unit)
        ]
    )

let private forMember = createMember' None "this" "For" forArgs [] forExpr

let private delayMember =
    createMember "Delay" (Pat.ident ("f")) [] (Expr.app ("f", []))

let private zeroMember = createMember "Zero" Pat.wild [] Expr.unit

let private yieldMember isType =
    createYield isType yieldReturnExpr yieldReturnExpr

let private newNameExpr =
    Expr.tuple (Expr.ident ("newName"), Expr.ident ("args"), Expr.ident ("cros"))

let private nameMember = createNameOperation newNameExpr

let createYieldFor argsType propType =
    let setExpr =
        Expr.sequential (
            [
                Expr.set (
                    "args."
                    + propType.Name,
                    argToInput
                )
                args
            ]
        )

    let expr =
        Expr.list (
            [
                Expr.paren (
                    Expr.sequential (
                        [
                            Expr.let' ("func", [ Pat.typed ("args", argsType) ], setExpr)
                            funcIdent
                        ]
                    )
                )
            ]
        )

    let cros = Expr.list ([ Expr.ident ("id") ])

    [ createYield' (not propType.IsResource) argIdent expr cros ]

let mapOperationType yieldSelector opsSelector =
    function
    | {
          Type = PRef _
          CanGenerateYield = true
      } & pt -> yieldSelector pt
    | { Type = PRef _ } & pt
    | { Type = PString } & pt
    | { Type = PInteger } & pt
    | { Type = PFloat } & pt
    | { Type = PBoolean } & pt
    | { Type = PArray _ } & pt
    | { Type = PUnion _ } & pt
    | { Type = PJson _ } & pt
    | { Type = PMap _ } & pt
    | { Type = PAssetOrArchive _ } & pt
    | { Type = PAny _ } & pt
    | { Type = PArchive _ } & pt -> opsSelector pt

let createBuilderClass isType isComponent name pTypes =
    let argsType = $"{name}Args"

    let apply varname =
        let args =
            match varname with
            | "args" -> argsType
            | _ when isComponent -> "ComponentResourceOptions"
            | _ -> "CustomResourceOptions"

        Expr.app (
            "List.fold",
            [
                Expr.paren (
                    Expr.lambda (
                        [
                            varname
                            "f"
                        ],
                        Expr.app ("f", varname)
                    )
                )
                Expr.paren (createInstance args Expr.unit)
                Expr.ident (varname)
            ]
        )

    let resourceRunExp () =
        Expr.paren (Expr.tuple (Expr.ident ("name"), (apply "args"), (apply "cros")))
        |> createInstance name

    let createOperations =
        mapOperationType (createYieldFor argsType) (createOperationsFor' argsType)

    let operations =
        pTypes
        |> Seq.collect createOperations

    let inputListOfInput argName =
        Expr.app (
            Expr.ident ("inputList"),
            Expr.list ([ Expr.app (Expr.ident ("input"), Expr.ident (argName)) ])
        )

    let inputListOfResources argName =
        Expr.app (
            Expr.ident ("inputList"),
            Expr.paren (
                Expr.app (
                    Expr.longIdent ("Seq.map"),
                    Expr.app (Expr.ident ("input"), Expr.ident (argName))
                )
            )
        )

    Module.type' (
        name
        + "Builder",
        [
            //Type.ctor()

            yieldMember isType

            if isType then
                apply "args"
                |> createRunType
            else
                resourceRunExp ()
                |> createRunResource

            if isType then combineMember else combineCrosMember
            forMember
            delayMember
            zeroMember

            yield! if isType then [] else [ nameMember ]

            yield! operations

            if not isType then
                croOperation
                    "DependsOn"
                    ResourceOptions
                    "Ensure this resource gets created after its dependency"
                    "dependency"
                    (inputListOfInput "dependency")
                    false
                    true

                croOperation
                    "DependsOn"
                    ResourceOptions
                    "Ensure this resource gets created after its dependency"
                    "dependency"
                    (inputListOfResources "dependency")
                    true
                    false

                croOperation
                    "Id"
                    ResourceOptions
                    "An optional existing ID to load, rather than create."
                    "resourceId"
                    (Expr.ident "resourceId")
                    false
                    true

                croOperation
                    "IgnoreChanges"
                    ResourceOptions
                    "If set to True, the providers Delete method will not be called for this resource."
                    "paths"
                    (Expr.ident "paths")
                    true
                    true

                croOperation
                    "ReplaceOnChanges"
                    ResourceOptions
                    """Changes to any of these property paths will force a replacement.  If this list includes `"*"`, changes to any properties will force a replacement.  Initialization errors from previous deployments will require replacement instead of update only if `"*"` is passed."""
                    "paths"
                    (Expr.ident "paths")
                    true
                    true

                croOperation
                    "ResourceTransformations"
                    ResourceOptions
                    "Optional list of transformations to apply to this resource during construction.The transformations are applied in order, and are applied prior to transformation applied to parents walking from the resource up to the stack."
                    "transformations"
                    (Expr.ident "transformations")
                    true
                    true

                croOperation
                    "RetainOnDelete"
                    ResourceOptions
                    "If set to True, the providers Delete method will not be called for this resource."
                    "retain"
                    (Expr.ident "retain")
                    false
                    true

                croOperation
                    "Urn"
                    ResourceOptions
                    "The URN of a previously-registered resource of this type to read from the engine."
                    "urn"
                    (Expr.ident "urn")
                    false
                    true

                croOperation
                    "Protect"
                    ResourceOptions
                    "When set to true, protect ensures this resource cannot be deleted."
                    "isProtected"
                    (Expr.ident "isProtected")
                    false
                    true

                croOperation
                    "Parent"
                    ResourceOptions
                    "An optional parent resource to which this resource belongs."
                    "parent"
                    (Expr.ident "parent")
                    false
                    true

                // CustomResourceOptions-specific types
                if not isComponent then
                    croOperation
                        "AdditionalSecretOutputs"
                        CustomResourceOptions
                        "The names of outputs for this resource that should be treated as secrets. This augments the list that the resource provider and pulumi engine already determine based on inputs to your resource. It can be used to mark certain outputs as a secrets on a per resource basis."
                        "outputs"
                        (Expr.ident "outputs")
                        true
                        true

                    croOperation
                        "DeleteBeforeReplace"
                        CustomResourceOptions
                        "When set to `true`, indicates that this resource should be deleted before its replacement is created when replacement is necessary."
                        "deletedBeforeReplace"
                        (Expr.ident "deletedBeforeReplace")
                        false
                        true

                    croOperation
                        "ImportId"
                        CustomResourceOptions
                        "When provided with a resource ID, import indicates that this resource's provider should import its state from the cloud resource with the given ID. The inputs to the resource's constructor must align with the resource's current state. Once a resource has been imported, the import property must be removed from the resource's options."
                        "resourceId"
                        (Expr.ident "resourceId")
                        false
                        true

                    croOperation
                        "Provider"
                        ResourceOptions
                        "An optional provider to use for this resource's CRUD operations. If no provider is supplied, the default provider for the resource's package will be used. The default provider is pulled from the parent's provider bag (see also ComponentResourceOptions.providers)."
                        "resourceProvider"
                        (Expr.ident "resourceProvider")
                        false
                        true

                else
                    croOperation
                        "Providers"
                        ComponentResourceOptions
                        "An optional set of providers to use for child resources."
                        "resourceProviders"
                        (Expr.ident "resourceProviders")
                        true
                        true
        ]
    )
