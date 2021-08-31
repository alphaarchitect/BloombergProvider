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
    type NameSpace = string

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

    type MinMaxOccurs =
        | One
        | ZeroOrOne
        | OneOrMore
        | ZeroOrMore
        | Unknown of MinOccurs: string * MaxOccurs: string

    type Mutability =
        | AllMutable
        | OptionMutable
        | Immutable

    type Parameter =
        | Optional
        | Required

    let private elementToMinMaxOccurs (x: BlpSchema.Element) =
        match x.MinOccurs |> Option.defaultValue "1", x.MaxOccurs |> Option.defaultValue "1" with
        | "1", "1" -> One
        | "0", "1" -> ZeroOrOne
        | "1", "unbounded" -> OneOrMore
        | "0", "unbounded" -> ZeroOrMore
        | min, max -> Unknown(MinOccurs = min, MaxOccurs = max)

    let private primitiveTypeToType = function
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

    let internal createImmutableProperty, internal createMutableProperty  =
        let inline createAutoProperty (isMutable: bool) (name: string) (type': Type) =
            let field = ProvidedField(name, type')
            let getterCode = (fun args ->
                match args with
                | this::[] -> Expr.FieldGet(this, field)
                | _ -> failwith "Unsupported arguments to getter")
            let property =
                match isMutable with
                | true ->
                    let setterCode = (fun args ->
                        match args with
                        | this::[x] -> Expr.FieldSet(this, field, x)
                        | _ -> failwith "Unsupported arguments to setter")
                    ProvidedProperty(name, type', getterCode = getterCode, setterCode = setterCode)
                | false ->
                    ProvidedProperty(name, type', getterCode = getterCode)
            field, property
        (false |> createAutoProperty), (true |> createAutoProperty)

    let private elementToProperty (x: BlpSchema.Element) tryGetType mutableType =
        match x.Status |> Option.map Union.fromString<Status> with
        | None
        | Some (Some Active) ->
            let propertyType =
                let t =
                    match Union.fromString<PrimitiveType> (x.Type) with
                    | Some p -> primitiveTypeToType p |> Result.Ok
                    | None ->
                        match tryGetType (x.Type) with
                        | Some t -> Result.Ok t
                        | None -> ElementError.UnknownType (x.Type) |> Result.Error

                t
                |> Result.bind (fun t' ->
                        match elementToMinMaxOccurs(x) with
                        | One -> (t', Required) |> Ok
                        | ZeroOrOne ->
                            (typedefof<option<_>>.MakeGenericType (t'), Optional)
                            |> Ok
                        | OneOrMore ->
                            (typedefof<_ * _>.MakeGenericType (t', typedefof<list<_>>.MakeGenericType t'), Required)
                            |> Ok
                        | ZeroOrMore ->
                            (typedefof<list<_>>.MakeGenericType (t'), Required)
                            |> Ok
                        | Unknown (min, max) -> UnknownMinMaxOccurs(MinOccurs = min, MaxOccurs = max) |> Error)

            propertyType
            |> Result.map
                (fun (type', parameter) ->
                    let f =
                        match parameter with
                        | Required
                        | Optional -> ProvidedField(x.Name, type')

                    let f, p =
                        match mutableType, parameter with
                        | AllMutable, _
                        | OptionMutable, Optional ->
                            createMutableProperty x.Name type'
                        | _, Required
                        | _, Optional ->
                            createImmutableProperty x.Name type'

                    let c =
                        match mutableType, parameter with
                        | AllMutable, _
                        | OptionMutable, Optional -> None
                        | OptionMutable, Required
                        | Immutable, Required -> ProvidedParameter(x.Name, type') |> Some
                        | _, Optional -> ProvidedParameter(x.Name, type', optionalValue = None) |> Some

                    p.AddXmlDoc(x.Description)

                    { Parameter = c
                      Field = f
                      Property = p })
        | Some None ->
            ElementError.UnknownStatus(x.Name, x.Status.Value)
            |> Error

    let createType (s: BlpSchema.SequenceType) (assembly: Assembly) (nameSpace: NameSpace) (tryGetType: string -> option<Type>) =
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

                let optional, required =
                    s.Elements
                    |> Array.partition (fun x ->
                        match elementToMinMaxOccurs(x) with
                        | ZeroOrOne -> true
                        | _ -> false)

                let mutableType =
                    let allMutable =
                        required |> Array.length > MAXIMUM_CONSTRUCTOR_PARAMETERS
                    let optionalMutable =
                        optional |> Array.length > MAXIMUM_CONSTRUCTOR_PARAMETERS || s.Elements |> Array.length > MAXIMUM_CONSTRUCTOR_PARAMETERS
                    match allMutable, optionalMutable with
                    | true, _ -> Mutability.AllMutable
                    | false, false -> Mutability.Immutable
                    | false, true -> Mutability.OptionMutable

                let elements =
                    s.Elements
                    |> Array.map (fun e -> elementToProperty e tryGetType mutableType)
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
                                | _ -> failwith "wrong ctor parameters")

                    es
                    |> List.iter
                        (fun e ->
                            providedSequenceType.AddMembers [ e.Field :> MemberInfo
                                                              e.Property :> MemberInfo ])

                    providedSequenceType |> Ok
                | Error es -> Element(s.Name, es) |> Error

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
        | TypeInconsistent of Label: Label

    type EnumerationError =
        | Empty of Name: Name
        | Enumerator of Name: Name * list<EnumeratorError>
        | Duplicates of Name: Name * list<Label>
        | UnknownType of Name: Name * Type: string
        | UnknownStatus of Name: Name * Status: string

    type EnumerationField =
        | String of Name: string * Description: string
        | Int32 of Name: string * Value: int32 * Description: string

    let private schemaEnumeratorToEnumerationField (x: BlpSchema.Enumerator) =
        match (x.Status |> Option.map Union.fromString<Status>) with
        | None
        | Some (Some Active) ->
            match x.Value.Int32, x.Value.String with
            | Some s, None ->
                try
                    Ok
                    <| EnumerationField.Int32(Name = x.Name, Value = int32 s, Description = x.Description)
                with :? FormatException -> Int32Format(x.Name, s) |> Error
            | None, Some s when s = x.Name || (s = "THAIL" && x.Name = "THAI") ->
                Ok
                <| EnumerationField.String(Name = x.Name, Description = x.Description)
            | None, Some s -> StringInconsistent(x.Name, s) |> Error
            | None, None -> MissingValue(x.Name) |> Error
            | Some _, Some _ -> MultipleValues(x.Name) |> Error
        | Some None ->
            EnumeratorError.UnknownStatus(x.Name, x.Status.Value)
            |> Error

    let internal effectiveEnumerationType
        (name: Name)
        (enumerationType: EnumerationType)
        (enumerators: EnumerationField list)
        =
        match enumerationType with
        | EnumerationType.String ->
            enumerators
            |> List.map (fun x ->
                match x with
                | EnumerationField.String (name, documentation) -> Ok (name, documentation)
                | EnumerationField.Int32 (name, _, _) -> Error <| TypeInconsistent(name))
            |> List.fold Result.folder (Result.Ok [])
            |> Result.map (Map.ofList >> Enumeration.String)
            |> Result.mapError (fun xs -> Enumerator(name, xs))
        | EnumerationType.Int32 ->
            enumerators
            |> List.map (fun x ->
                match x with
                | EnumerationField.String (name, _) -> Error <| TypeInconsistent(name)
                | EnumerationField.Int32 (name, value, description) -> Ok ((name, value), description))
            |> List.fold Result.folder (Result.Ok [])
            |> Result.map (Map.ofList >> Enumeration.Int32)
            |> Result.mapError (fun xs -> Enumerator(name, xs))

    let internal createEnumerationType (assembly: Assembly) (nameSpace: string) (name: Name) (enumerationType: EnumerationType) (description: Description) (enumerators: list<EnumerationField>) =
        let duplicates =
            enumerators
            |> Seq.countBy (fun x ->
                match x with
                | String (n, _)
                | Int32 (n, _, _) -> n)
            |> List.ofSeq
            |> List.filter (fun (_, i) -> i > 1)
            |> List.map fst

        match enumerators with
        | [] -> Error <| Empty(name)
        | _ when not (duplicates |> List.isEmpty) -> Error <| Duplicates(name, duplicates)
        | _ ->
            let providedEnumType =
                ProvidedTypeDefinition(
                    assembly,
                    nameSpace,
                    name,
                    baseType = Some typeof<Enum>,
                    hideObjectMethods = true,
                    isErased = false
                )

            providedEnumType.AddXmlDoc(description)

            effectiveEnumerationType name enumerationType enumerators
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

    let createType (e: BlpSchema.EnumerationType) (assembly: Assembly) (nameSpace: NameSpace) =
        match e.Status |> Option.map Union.fromString<Status> with
        | Some None ->
            EnumerationError.UnknownStatus(e.Name, e.Status.Value)
            |> Error
        | None
        | Some (Some Active) ->
            e.Enumerators
            |> Array.map schemaEnumeratorToEnumerationField
            |> Array.fold Result.folder (Result.Ok [])
            |> Result.mapError (fun xs -> EnumerationError.Enumerator(e.Name, xs))
            |> Result.bind (fun enumerators ->
                match Union.fromString<EnumerationType>(e.Type) with
                | Some x -> x |> Ok
                | None -> UnknownType(e.Name, e.Type) |> Error
                |> Result.bind (fun enumerationType ->
                    createEnumerationType assembly nameSpace e.Name enumerationType e.Description enumerators))


module Choice =
    type ElementError =
        | UnknownType of Name: Name * TypeName: string

    type ChoiceError =
        | Empty of Name: Name
        | Enumeration of Enumeration.EnumerationError
        | UnknownStatus of Name: Name * Status: string
        | Element of Name: Name * list<ElementError>

    type ChoiceType =
        {
            Enum: ProvidedTypeDefinition
            Object: ProvidedTypeDefinition
        }

    let createType (c: BlpSchema.ChoiceType) (assembly: Assembly) (nameSpace: NameSpace) (tryGetType: string -> option<Type>) =
        let providedChoiceType =
            ProvidedTypeDefinition(
                assembly,
                nameSpace,
                c.Name,
                baseType = Some typeof<obj>,
                hideObjectMethods = false,
                isErased = false
            )

        match c.Status |> Option.map Union.fromString<Status> with
        | Some None ->
            ChoiceError.UnknownStatus(c.Name, c.Status.Value)
            |> Error
        | None
        | Some (Some Active) ->
            let enumerators =
                c.Elements
                |> List.ofArray
                |> List.map (fun x ->
                    Enumeration.EnumerationField.String(x.Name, x.Description))
            let enumName = sprintf "%sType" c.Name
            Enumeration.createEnumerationType assembly nameSpace enumName Enumeration.EnumerationType.String c.Description enumerators
            |> Result.mapError Enumeration
            |> Result.bind (fun enum ->
                let enumField, property = Sequence.createImmutableProperty enumName enum
                providedChoiceType.AddMember enumField
                providedChoiceType.AddMember property

                let es =
                    c.Elements
                    |> Array.map (fun x ->
                        match tryGetType (x.Type) with
                        | Some t ->
                            let field, property = Sequence.createImmutableProperty x.Name t
                            providedChoiceType.AddMember field
                            providedChoiceType.AddMember property
                            field |> Result.Ok
                        | None -> UnknownType (x.Name, x.Type) |> Result.Error)
                    |> Array.fold Result.folder (Result.Ok [])

                es
                |> Result.mapError (fun xs -> ChoiceError.Element(c.Name, xs))
                |> Result.map (fun xs ->
                    xs
                    |> List.iter (fun field ->
                        let parameter = ProvidedParameter(field.Name, field.FieldType)
                        providedChoiceType.AddMember
                        <| ProvidedConstructor(
                            [parameter],
                            invokeCode =
                                fun args ->
                                    match args with
                                    | this :: [arg] ->
                                        let setValue = Expr.FieldSet(this, field, arg)
                                        let enumField = Expr.FieldSet(this, enumField, Expr.FieldGet(enum.GetField(field.Name)))
                                        Expr.Sequential(enumField, setValue)
                                    | _ -> failwith "wrong ctor parameters"))
                    {
                        Enum = enum
                        Object = providedChoiceType
                    }))

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
            DependencyGraph.create(schema.Schema).TopologicalSort() |> List.ofSeq |> List.rev

        let mutable typeCache = Map.empty<string, Type>
        
        let types =
            dependencyOrder
            |> List.map (fun x ->
                let typeRef =
                    match x with
                    | Sequence s ->
                        Sequence.createType s tempAssembly nameSpace typeCache.TryFind
                        |> Result.map (fun x -> [x])
                        |> Result.mapError TypeError.SequenceError
                    | Choice c ->
                        Choice.createType c tempAssembly nameSpace typeCache.TryFind
                        |> Result.map (fun x -> [x.Enum; x.Object])
                        |> Result.mapError TypeError.ChoiceError
                    | Enumeration e ->
                        Enumeration.createType e tempAssembly nameSpace
                        |> Result.map (fun x -> [x])
                        |> Result.mapError TypeError.EnumerationError

                match typeRef with
                | Result.Ok xs ->
                    (* Depends on the ordering scheme above, we are adding the choice object not enum *)
                    let x = List.last xs
                    typeCache <- typeCache |> Map.add x.Name (x :> Type)
                | _ -> ()

                typeRef)
            |> List.fold Result.folder (Result.Ok [])
            |> Result.map List.concat

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
