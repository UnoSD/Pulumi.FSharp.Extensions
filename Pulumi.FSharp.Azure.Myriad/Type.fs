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
    
let createType isType (provider : JsonValue) (fqType : string, jValue : JsonValue) =
    let getComplexType typeFullPath =
        provider.["types"].Properties()
        |> Array.tryFind (fun (t, _) -> 
            ("#/types/" + t) = typeFullPath)
        |> (fun o -> 
            match o with
            | Some x -> x
            | None -> 
                if typeFullPath.StartsWith("pulumi.json#") 
                then "complex", JsonValue.Null
                else sprintf "Not found: %s" typeFullPath |> failwith)
        |> (fun (complexType, _) ->
            if complexType = "complex" 
            then "complex"
            else "complex:" + (getTypeInfo complexType |> (fun (_,_,_,_,_,t) -> t)))

    let (_,
         _,
         tProvider,
         category,
         _,
         typeName) =
        getTypeInfo fqType
        
    let propertiesProperty =
        if isType then "properties" else "inputProperties"
        
    let properties = jValue.GetProperty(propertiesProperty).Properties()
    
    let serviceProvider =
        provider.["language"].["csharp"].["namespaces"].Properties()
        |> Array.find (fun (p, _) -> p = category)
        |> snd
        |> (fun jv -> jv.AsString())
    
    let ns =
        serviceProvider
        |> sprintf "Pulumi.%s.%s" (toPascalCase tProvider)

    let nameAndType (name, jValue : JsonValue) =
        let tName =
            let nameFromProperties =
                jValue.Properties()
                |> Array.tryFind (fun (p, _) -> p = "language")
                |> Option.bind (fun (_, v) ->
                    v.Properties()
                    |> Array.tryFind (fun (p, _) -> p = "csharp")
                    |> Option.map snd)
                |> Option.map (fun v -> v.GetProperty("name").AsString())
            match nameFromProperties with
            | Some name -> name
            | None      -> name
        
        let _ =
            jValue.Properties()
            |> Array.tryFind (fun (p, _) -> p = "description")
            |> Option.map (snd >> (fun x -> x.AsString() |> PossibleValues().TypedMatches) >> (fun x -> x))
        
        let pType =
            jValue.Properties()
            |> Array.choose (fun (p, v) ->
                match p with
                | "type" -> v.AsString() |> Some // Array type has also "items"
                | "$ref" -> getComplexType (v.AsString()) |> Some
                (*| "description"*)
                | _ -> None)
            |> Array.head
        
        (tName, pType)
    
    ns, typeName, properties, nameAndType, serviceProvider