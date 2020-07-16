module AstType

open FSharp.Data
open Core
open FSharp.Text.RegexProvider

type PossibleValues =
    Regex<"Possible values are `(?<Value>[\w])`(?:, `(?<Value>[\w])`)? and `(?<Value>[\w])`.">
    
// Version needs to match NuGet package
[<Literal>]
let private pulumiSchemaUrl =
    "https://raw.githubusercontent.com/pulumi/pulumi-azure/v3.11.0/provider/cmd/pulumi-resource-azure/schema.json"

type private PulumiProvider =
    JsonProvider<pulumiSchemaUrl>
    
#nowarn "25"
    
let private getTypeInfo (typeName : string) =
    let [| fullProvider; fullType |] = typeName.Split("/")
    let [| tProvider; category |] = fullProvider.Split(':')
    let [| resourceType; _(*subtype*) |] = fullType.Split(':')
    let formattedTypeName = toPascalCase resourceType
    
    fullProvider, fullType, tProvider, category, resourceType, formattedTypeName
    
let createType (provider : PulumiProvider.Root) (fqType : string, jValue : JsonValue) =
    let getComplexType typeFullPath =
        provider.Types.JsonValue.Properties() |>
        Array.find (fun (t, _) -> ("#/types/" + t) = typeFullPath) |>
        (fun (complexType, _) -> "complex:" + (getTypeInfo complexType |> (fun (_,_,_,_,_,t) -> t)))

    let (_,
         _,
         tProvider,
         category,
         _,
         typeName) =
        getTypeInfo fqType
        
    let properties = jValue.GetProperty("inputProperties").Properties()
    
    let serviceProvider =
        provider.Language.Csharp.Namespaces.JsonValue.Properties() |>
        Array.find (fun (p, _) -> p = category) |>
        snd |>
        (fun jv -> jv.AsString())
    
    let ns =
        serviceProvider |>
        sprintf "Pulumi.%s.%s" (toPascalCase tProvider)

    let nameAndType (name, jValue : JsonValue) =
        let tName =
            match jValue.Properties() |>
                  Array.tryFind (fun (p, _) -> p = "language") |>
                  Option.bind (fun (_, v) -> v.Properties() |>
                                             Array.tryFind (fun (p, _) -> p = "csharp") |>
                                             Option.map snd) |>
                  Option.map (fun v -> v.GetProperty("name").AsString()) with
            | Some name -> name
            | None      -> name
        
        let _ =
            jValue.Properties() |>
            Array.tryFind (fun (p, _) -> p = "description") |>
            Option.map (snd >> (fun x -> x.AsString() |> PossibleValues().TypedMatches) >> (fun x -> x))
        
        let pType =
            jValue.Properties() |>
            Array.choose (fun (p, v) -> match p with
                                        | "type" -> v.AsString() |> Some // Array type has also "items"
                                        | "$ref" -> getComplexType (v.AsString()) |> Some
                                        (*| "description"*)
                                        | _ -> None) |>
            Array.head
        
        (tName, pType)
    
    ns, typeName, properties, nameAndType, serviceProvider