module BloombergProviderImplementation

open AlphaArchitect.Common
open BloombergProvider.Schema
open BloombergProvider

open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open QuikGraph.Algorithms

open System
open System.Reflection

module Common =
    type Name = string
    type Label = string
    type Description = string
    type Status = Active

open Common
open Microsoft.FSharp.Quotations

module Sequence =
    type ElementError =
        | UnknownStatus of Label: string * Status: string
        | MissingValue of Label: Label
        | UnknownMinMaxOccurs of MinOccurs: string * MaxOccurs: string
        | UnknownType of TypeName: string

    type SequenceError =
        | Duplicates of Name: Name * list<Label>
        | UnknownType of Name: Name * Type: string
        | UnknownStatus of Name: Name * Status: string
        | Element of Name: Name * list<ElementError>

    type Property =
        { Parameter: option<ProvidedParameter>
          Field: ProvidedField
          Property: ProvidedProperty }

    let private elementToProperty (x: BlpSchema.Element) tryGetType isMutable =
        match x.Status |> Option.map Union.fromString<Status> with
        | None
        | Some (Some Active) ->
            let propertyType =
                let t =
                    match Union.fromString<PrimitiveType> (x.Type) with
                    | Some p ->
                        let p' =
                            match p with
                            | Boolean -> typeof<bool>
                            | Char -> typeof<char>
                            (* TODO: YodaTime *)
                            | Date
                            | Datetime
                            | Time -> typeof<DateTime>
                            | Float32 -> typeof<float32>
                            | Float64 -> typeof<double>
                            | Int32 -> typeof<int32>
                            | Int64 -> typeof<int64>
                            | String -> typeof<string>
                        Result.Ok p'
                    | None ->
                        match tryGetType (x.Type) with
                        | Some t -> Result.Ok t
                        | None -> ElementError.UnknownType (x.Type) |> Result.Error

                t
                |> Result.bind (fun t' ->
                    match x.MinOccurs |> Option.defaultValue "1", x.MaxOccurs |> Option.defaultValue "1" with
                    | "1", "1" -> Ok {| DefaultValue = None; Type = t' |}
                    | "0", "1" ->
                        {| DefaultValue = Some None
                           Type = typedefof<option<_>>.MakeGenericType (t') |}
                        |> Ok
                    | "1", "unbounded" ->
                        {| DefaultValue = None
                           Type = typedefof<_ * _>.MakeGenericType (t', typedefof<list<_>>.MakeGenericType t') |}
                        |> Ok
                    | "0", "unbounded" ->
                        {| DefaultValue = None
                           Type = typedefof<list<_>>.MakeGenericType (t') |}
                        |> Ok
                    | min, max -> UnknownMinMaxOccurs(min, max) |> Error)

            propertyType
            |> Result.map
                (fun y ->
                    let n = x.Name
                    let t = y.Type

                    let f = ProvidedField(n, t)

                    let c =
                        match isMutable with
                        | true -> None
                        | false ->
                            match y.DefaultValue with
                            | Some o -> ProvidedParameter(n, t, optionalValue = o)
                            | None -> ProvidedParameter(n, t)
                            |> Some

                    let p =
                        let getterCode = (fun args ->
                            match args with
                            | this::[] -> Expr.FieldGet(this, f)
                            | _ -> failwith "Unsupported arguments to getter")
                        match isMutable with
                        | true -> ProvidedProperty(n, t, getterCode = getterCode, setterCode =
                            (fun args ->
                                match args with
                                | this::[x] -> Expr.FieldSet(this, f, x)
                                | _ -> failwith "Unsupported arguments to setter"))
                        | false -> ProvidedProperty(n, t, getterCode = getterCode)

                    p.AddXmlDoc(x.Description)

                    { Parameter = c
                      Field = f
                      Property = p })
        | Some None ->
            ElementError.UnknownStatus(x.Name, x.Status.Value)
            |> Error

    let createType (s: BlpSchema.SequenceType) (assembly: Assembly) nameSpace (tryGetType: string -> option<Type>) =
        match (s.Status |> Option.map Union.fromString<Status>) with
        | Some None -> UnknownStatus(s.Name, s.Status.Value) |> Error
        | None
        | Some (Some Active) ->
            let duplicates =
                s.Elements
                |> Seq.countBy (fun x -> x.Name)
                |> List.ofSeq
                |> List.filter (fun (_, i) -> i > 1)
                |> List.map fst

            match s.Elements with
            | _ when not (duplicates |> List.isEmpty) -> Error <| Duplicates(s.Name, duplicates)
            | _ ->

                let MAXIMUM_CONSTRUCTOR_PARAMETERS = 288
                let isMutable = s.Elements |> Array.length > MAXIMUM_CONSTRUCTOR_PARAMETERS

                let elements =
                    s.Elements
                    |> Array.map (fun e -> elementToProperty e tryGetType isMutable)
                    |> Array.fold Result.folder (Result.Ok [])

                match elements with
                | Ok es ->
                    let providedSequenceType =
                        ProvidedTypeDefinition(
                            assembly,
                            nameSpace,
                            s.Name,
                            baseType = Some typeof<obj>,
                            hideObjectMethods = false,
                            isErased = false
                        )

                    providedSequenceType.AddMember
                    <| ProvidedConstructor(
                        es
                        |> List.choose ((fun x -> x.Parameter) >> id),
                        invokeCode =
                            fun args ->
                                match args with
                                | this :: args ->
                                    List.zip args (es |> List.choose (fun x -> x.Parameter |> Option.map (fun _ -> x.Field)))
                                    |> List.map (fun (arg, field) -> Expr.FieldSet(this, field, arg))
                                    |> (fun xs ->
                                        match xs with
                                        | [] -> Expr.Value(())
                                        | x :: xs -> List.fold (fun e x -> Expr.Sequential(e, x)) x xs)
                                | _ -> failwith "wrong ctor parameters"
                    )

                    es
                    |> List.iter
                        (fun e ->
                            providedSequenceType.AddMembers [ e.Field :> MemberInfo
                                                              e.Property :> MemberInfo ])

                    providedSequenceType |> Ok
                | Error es -> Element(s.Name, es) |> Error

module Choice =
    type ChoiceError =
        | Empty of Name: Name
        | Duplicates of Name: Name * list<Label>
        | UnknownType of Name: Name * Type: string
        | UnknownStatus of Name: Name * Status: string

    let createType (c: BlpSchema.ChoiceType) (assembly: Assembly) nameSpace =
        let providedChoiceType =
            ProvidedTypeDefinition(
                assembly,
                nameSpace,
                c.Name,
                baseType = Some typeof<obj>,
                hideObjectMethods = false,
                isErased = false
            )

        providedChoiceType
        |> Result.Ok

module Enumeration =
    type EnumerationType =
        | String
        | Int32

    type Enumeration =
        | String of Map<Label, Description>
        | Int32 of Map<Label * Int32, Description>

    type EnumeratorError =
        | Int32Format of Label: Label * Int32: string
        | StringInconsistent of Label: Label * String: string
        | MultipleValues of Label: Label
        | UnknownStatus of Label: string * Status: string
        | MissingValue of Label: Label

    type EnumerationError =
        | Empty of Name: Name
        | Enumerator of Name: Name * list<EnumeratorError>
        | Duplicates of Name: Name * list<Label>
        | TypeInconsistent of Name: Name
        | UnknownType of Name: Name * Type: string
        | UnknownStatus of Name: Name * Status: string

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
        | Some None ->
            EnumeratorError.UnknownStatus(x.Name, x.Status.Value)
            |> Error

    let private effectiveEnumerationType
        (e: BlpSchema.EnumerationType)
        (enumeratorType: EnumerationType)
        (enumerators: (EnumerationType * string * int32 option * string) list)
        =
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

    let createType (e: BlpSchema.EnumerationType) (assembly: Assembly) nameSpace =
        match e.Status |> Option.map Union.fromString<Status> with
        | Some None ->
            EnumerationError.UnknownStatus(e.Name, e.Status.Value)
            |> Error
        | None
        | Some (Some Active) ->
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

                providedEnumType.AddXmlDoc(e.Description)

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
                        | 1 -> effectiveEnumerationType e (types |> Set.toSeq |> Seq.head) enums
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

type TypeError =
    | EnumerationError of Enumeration.EnumerationError
    | ChoiceError of Choice.ChoiceError
    | SequenceError of Sequence.SequenceError

open System.IO
open System.Collections.Concurrent

[<TypeProvider>]
type BasicGenerativeProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config,
                                      assemblyReplacementMap = [ ("BloombergProvider.DesignTime",
                                                                  "BloombergProvider.Runtime") ])

    let nameSpace = "AlphaArchitect"

    let createRootType (typeName, schemaPath: string) =
        let tempAssembly = ProvidedAssembly()

        let schema = BlpSchema.Load(schemaPath)

        let rootType =
            ProvidedTypeDefinition(
                tempAssembly,
                nameSpace,
                typeName,
                baseType = Some typeof<obj>,
                hideObjectMethods = true,
                isErased = false
            )

        let dependencyOrder =
            DependencyGraph.create(schema.Schema).TopologicalSort() |> Array.ofSeq |> Array.rev

        let mutable typeCache = Map.empty<string, Type>
        
        let types =
            dependencyOrder
            |> Array.map (fun x ->
                let typeRef =
                    match x with
                    | Sequence s ->
                        Sequence.createType s tempAssembly nameSpace typeCache.TryFind
                        |> Result.mapError TypeError.SequenceError
                    | Choice c ->
                        Choice.createType c tempAssembly nameSpace
                        |> Result.mapError TypeError.ChoiceError
                    | Enumeration e ->
                        Enumeration.createType e tempAssembly nameSpace
                        |> Result.mapError TypeError.EnumerationError

                match typeRef with
                | Result.Ok x ->
                    typeCache <- typeCache |> Map.add x.Name (x :> Type)
                | _ -> ()

                typeRef)
            |> Array.fold Result.folder (Result.Ok [])

        match types with
        | Ok e -> rootType.AddMembers e
        | Error es -> failwithf "%A" es

        tempAssembly.AddTypes [ rootType ]
        rootType

    let assembly = Assembly.GetExecutingAssembly()
    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do
        assert (typeof<AlphaArchitect.Runtime>.Assembly.GetName()
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
                fun typeName args ->
                    cache
                        .GetOrAdd(
                            typeName,
                            lazy (createRootType (typeName, (string args.[0])))
                        )
                        .Value
        )

        t

    do this.AddNamespace(nameSpace, [ rootType ])

[<TypeProviderAssembly>]
do ()
