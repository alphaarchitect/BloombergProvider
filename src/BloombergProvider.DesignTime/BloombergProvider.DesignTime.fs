module BloombergProviderImplementation

open AlphaArchitect.Common

open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

open FSharp.Data
open System
open System.Collections.Concurrent

[<Literal>]
let private BlpSchemaPath =
    __SOURCE_DIRECTORY__ + "\\blp.schema.xsd"

type private BlpSchema =
    XmlProvider<Schema=BlpSchemaPath, Global=true, InferTypesFromValues=true, EmbeddedResource="BloombergProvider.DesignTime, BloombergProvider.DesignTime.blpschema.xsd">

module Enumeration =
    type Name = string
    type Label = string
    type Description = string

    type EnumerationType =
        | String
        | Int32

    type Enumeration =
        | String of Map<Label, Description>
        | Int32 of Map<Label * Int32, Description>

    type Status = Active

    type EnumeratorError =
        | Int32Format of Label: Label * Int32: string
        | StringInconsistent of Label: Label * String: string
        | MultipleValues of Label: Label
        | UnknownStatus of Label: string * Status: String
        | MissingValue of Label: Label

    type EnumerationError =
        | Empty of Name: Name
        | Enumerator of Name: Name * list<EnumeratorError>
        | Duplicates of Name: Name * list<Label>
        | TypeInconsistent of Name: Name
        | UnknownType of Name: Name * Type: string

    let private enumeratorToTuple (x: BlpSchema.Enumerator) =
        match (x.Status |> Option.map Union.fromString<Status>) with
        | None
        | Some (Some Active) ->
            match x.Value.Int32, x.Value.String with
            | Some s, None ->
                try
                    Ok
                    <| (EnumerationType.Int32, x.Name, int32 s |> Some, x.Description)
                with :? FormatException -> Int32Format(x.Name, s) |> Error
            | None, Some s when s = x.Name || (s = "THAIL" && x.Name = "THAI") ->
                Ok
                <| (EnumerationType.String, x.Name, None, x.Description)
            | None, Some s -> StringInconsistent(x.Name, s) |> Error
            | None, None -> MissingValue(x.Name) |> Error
            | Some _, Some _ -> MultipleValues(x.Name) |> Error
        | Some None -> UnknownStatus(x.Name, x.Status.Value) |> Error

    let private effectiveEnumerationType
        (
            e: BlpSchema.EnumerationType,
            enumeratorType: EnumerationType,
            enumerators: (EnumerationType * string * int32 option * string) list
        ) =
        match Union.fromString<EnumerationType> (e.Type), enumeratorType with
        | Some EnumerationType.String, EnumerationType.String ->
            enumerators
            |> List.map (fun (_, label, _, doc) -> label, doc)
            |> Map.ofList
            |> Enumeration.String
            |> Ok
        | Some EnumerationType.Int32, EnumerationType.Int32 ->
            enumerators
            |> List.map (fun (_, label, value, doc) -> (label, value.Value), doc)
            |> Map.ofList
            |> Enumeration.Int32
            |> Ok
        | Some _, _ -> Error <| TypeInconsistent(e.Name)
        | None, _ -> Error <| UnknownType(e.Name, e.Type)

    let createType (assembly: Assembly, nameSpace, e: BlpSchema.EnumerationType) =
        let duplicates =
            e.Enumerators
            |> Seq.countBy (fun x -> x.Name)
            |> List.ofSeq
            |> List.filter (fun (_, i) -> i > 1)
            |> List.map fst

        match e.Enumerators with
        | [||] -> Error <| Empty(e.Name)
        | _ when not (duplicates |> List.isEmpty) -> Error <| Duplicates(e.Name, duplicates)
        | _ ->
            let providedEnumType =
                ProvidedTypeDefinition(
                    assembly,
                    nameSpace,
                    e.Name,
                    baseType = Some typeof<Enum>,
                    hideObjectMethods = true,
                    isErased = false
                )

            let enumerators =
                e.Enumerators
                |> Array.map enumeratorToTuple
                |> Array.fold Result.folder (Result.Ok [])

            let enumeration =
                match enumerators with
                | Ok enums ->
                    let types =
                        enums
                        |> List.map (fun (x, _, _, _) -> x)
                        |> Set.ofList

                    match Set.count types with
                    | 1 -> effectiveEnumerationType (e, types |> Set.toSeq |> Seq.head, enums)
                    | _ -> TypeInconsistent(e.Name) |> Error
                | Error es -> Enumerator(e.Name, es) |> Error

            enumeration
            |> Result.map
                (fun x ->
                    let enumerators =
                        match x with
                        | Enumeration.Int32 xs ->
                            xs
                            |> Map.toList
                            |> List.map
                                (fun ((label, value), description) ->
                                    ProvidedField.Literal(label, providedEnumType, value), description)
                        | Enumeration.String xs ->
                            xs
                            |> Map.toList
                            |> List.map
                                (fun (label, description) ->
                                    ProvidedField.Literal(label, providedEnumType, label), description)
                        |> List.map
                            (fun (t, description) ->
                                match description with
                                | "" -> t
                                | _ ->
                                    t.AddXmlDoc(description)
                                    t)

                    providedEnumType.AddMembers enumerators
                    providedEnumType)

[<TypeProvider>]
type BasicGenerativeProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config,
                                      assemblyReplacementMap = [ ("BloombergProvider.DesignTime",
                                                                  "BloombergProvider.Runtime") ])

    let nameSpace = "AlphaArchitect"

    let createRootType (typeName, schemaPath: string) =
        let tempAssembly = ProvidedAssembly()

        let schema = BlpSchema.Load(schemaPath)

        let enumerationTypes =
            schema.Schema.EnumerationTypes
            |> Array.map (fun x -> Enumeration.createType (tempAssembly, nameSpace, x))
            |> Array.fold Result.folder (Result.Ok [])

        match enumerationTypes with
        | Ok e ->
            let rootType =
                ProvidedTypeDefinition(
                    tempAssembly,
                    nameSpace,
                    typeName,
                    baseType = Some typeof<obj>,
                    hideObjectMethods = true,
                    isErased = false
                )

            rootType.AddMembers e

            tempAssembly.AddTypes [ rootType ]

            rootType
        | Error es -> failwithf "%A" es

    let assembly = Assembly.GetExecutingAssembly()
    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do
        assert (typeof<AlphaArchitect.DataSource>.Assembly.GetName()
            .Name = assembly.GetName().Name)

    
    let cache =
        ConcurrentDictionary<string, Lazy<ProvidedTypeDefinition>>()

    let rootType =
        let t =
            ProvidedTypeDefinition(
                assembly,
                nameSpace,
                "BloombergProvider",
                Some typeof<obj>,
                hideObjectMethods = true,
                isErased = false
            )

        let staticParams =
            [ ProvidedStaticParameter("Schema", typeof<string>) ]

        t.AddXmlDoc
            """
<summary>Bloomberg Provider types based on blp schema file.</summary>
<param name='SchemaFile'>The full path to the schema file to base this provider on.</param>
"""

        t.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction =
                fun typeName args -> cache.GetOrAdd(typeName, lazy (createRootType (typeName, (string args.[0])))).Value)
        t

    do this.AddNamespace(nameSpace, [ rootType ])

[<TypeProviderAssembly>]
do ()
