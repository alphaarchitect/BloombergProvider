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
    type Name = private Name of string

    module Name =
        let ofString = Name
        let toString (Name name) = name

    type Label = private Label of string

    module Label =
        let ofString = Label
        let toString (Label label) = label

    type Description = private Description of string

    module Description =
        let ofString = Description
        let toString (Description description) = description

    type Status = Active
    type NameSpace = private NameSpace of string

    module NameSpace =
        let ofString = NameSpace
        let toString (NameSpace nameSpace) = nameSpace

open Common
open Microsoft.FSharp.Quotations

[<AutoOpen>]
module internal ActivePatterns =
    let (|Singleton|) =
        function
        | [ x ] -> x
        | _ -> failwith "Invalid parameters"

module Inference =
    type ParameterType =
        | Optional of InnerType: Type
        | Required

    type Element =
        { Parameter: option<ProvidedParameter * ParameterType>
          Field: ProvidedField
          Property: ProvidedProperty }

    type Mutability =
        | Mutable
        | OptionMutable
        | Immutable

let makeOptionType (type': Type) =
    typedefof<option<_>>.MakeGenericType (type')

module Sequence =
    open Inference

    type ElementError =
        | UnknownStatus of Label: Label * Status: string
        | MissingValue of Label: Label
        | UnknownMinMaxOccurs of MinOccurs: string * MaxOccurs: string
        | UnknownType of TypeName: string

    type SequenceError =
        | Duplicates of Name: Name * list<Label>
        | UnknownType of Name: Name * Type: string
        | UnknownStatus of Name: Name * Status: string
        | Element of Name: Name * list<ElementError>

    type Multiplicity =
        | Single
        | OptionalSingle
        | SingleOrMultiple
        | Multiple
        | Unknown of MinOccurs: string * MaxOccurs: string

    let private elementToMultiplicity (element: BlpSchema.Element) =
        match element.MinOccurs |> Option.defaultValue "1", element.MaxOccurs |> Option.defaultValue "1" with
        | "1", "1" -> Single
        | "0", "1" -> OptionalSingle
        | "1", "unbounded" -> SingleOrMultiple
        | "0", "unbounded" -> Multiple
        | min, max -> Unknown(MinOccurs = min, MaxOccurs = max)

    let private primitiveTypeToType =
        function
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

    let internal createImmutableProperty, internal createMutableProperty =
        let inline createAutoProperty (isMutable: bool) (name: Name) (propertyType: Type) =
            let name = Name.toString name
            let field = ProvidedField(name, propertyType)
            field.SetFieldAttributes(FieldAttributes.Private)
            let getterCode = (fun (Singleton this) -> Expr.FieldGet(this, field))

            let property =
                match isMutable with
                | true ->
                    let setterCode =
                        (fun args ->
                            match args with
                            | this :: [ arg ] -> Expr.FieldSet(this, field, arg)
                            | _ -> failwith "Unsupported arguments to setter")

                    ProvidedProperty(name, propertyType, getterCode = getterCode, setterCode = setterCode)
                | false -> ProvidedProperty(name, propertyType, getterCode = getterCode)

            field, property

        (false |> createAutoProperty), (true |> createAutoProperty)

    let private elementToProperty (element: BlpSchema.Element) tryGetType mutableType =
        match element.Status |> Option.map Union.fromString<Status> with
        | None
        | Some(Some Active) ->
            let propertyType =
                let propertyType =
                    match Union.fromString<PrimitiveType> (element.Type) with
                    | Some primitiveType -> primitiveTypeToType primitiveType |> Result.Ok
                    | None ->
                        match tryGetType (element.Type) with
                        | Some propertyType -> Result.Ok propertyType
                        | None -> ElementError.UnknownType(element.Type) |> Result.Error

                propertyType
                |> Result.bind (fun propertyType ->
                    match elementToMultiplicity (element) with
                    | Single -> (propertyType, Required) |> Ok
                    | OptionalSingle -> (makeOptionType (propertyType), Optional(propertyType)) |> Ok
                    | SingleOrMultiple ->
                        (typedefof<_ * _>.MakeGenericType
                            (propertyType, typedefof<list<_>>.MakeGenericType propertyType),
                         Required)
                        |> Ok
                    | Multiple -> (typedefof<list<_>>.MakeGenericType (propertyType), Required) |> Ok
                    | Unknown(min, max) -> UnknownMinMaxOccurs(MinOccurs = min, MaxOccurs = max) |> Error)

            let name = Name.ofString element.Name

            propertyType
            |> Result.map (fun (propertyType, parameter) ->
                let field, property =
                    match mutableType, parameter with
                    | Mutable, _
                    | OptionMutable, Optional _ -> createMutableProperty name propertyType
                    | _, Required
                    | _, Optional _ -> createImmutableProperty name propertyType

                let parameter =
                    match mutableType, parameter with
                    | Mutable, _
                    | OptionMutable, Optional _ -> None
                    | OptionMutable, Required
                    | Immutable, Required -> (ProvidedParameter(element.Name, propertyType), parameter) |> Some
                    | _, Optional _ ->
                        (ProvidedParameter(element.Name, propertyType, optionalValue = None), parameter)
                        |> Some

                property.AddXmlDoc(element.Description)

                { Parameter = parameter
                  Field = field
                  Property = property })
        | Some None ->
            ElementError.UnknownStatus(Label.ofString element.Name, element.Status.Value)
            |> Error

    let createType
        (sequence: BlpSchema.SequenceType)
        (assembly: Assembly)
        (nameSpace: NameSpace)
        (tryGetType: string -> option<Type>)
        =
        let name = Name.ofString sequence.Name

        match (sequence.Status |> Option.map Union.fromString<Status>) with
        | Some None -> UnknownStatus(name, sequence.Status.Value) |> Error
        | None
        | Some(Some Active) ->
            let duplicates =
                sequence.Elements
                |> Seq.countBy (fun element -> element.Name)
                |> List.ofSeq
                |> List.filter (fun (_, count) -> count > 1)
                |> List.map (fst >> Label.ofString)

            match sequence.Elements with
            | _ when not (duplicates |> List.isEmpty) -> Error <| Duplicates(name, duplicates)
            | _ ->

                let MAXIMUM_CONSTRUCTOR_PARAMETERS = 288

                let optional, required =
                    sequence.Elements
                    |> Array.partition (fun element ->
                        match elementToMultiplicity (element) with
                        | OptionalSingle -> true
                        | _ -> false)

                let mutableType =
                    let mutable' = required |> Array.length > MAXIMUM_CONSTRUCTOR_PARAMETERS

                    let optionalMutable =
                        optional |> Array.length > MAXIMUM_CONSTRUCTOR_PARAMETERS
                        || sequence.Elements |> Array.length > MAXIMUM_CONSTRUCTOR_PARAMETERS

                    match mutable', optionalMutable with
                    | true, _ -> Mutability.Mutable
                    | false, false -> Mutability.Immutable
                    | false, true -> Mutability.OptionMutable

                let elements =
                    sequence.Elements
                    |> Array.map (fun element -> elementToProperty element tryGetType mutableType)
                    |> Array.fold Result.folder (Result.Ok [])

                match elements with
                | Ok elements ->
                    let providedSequenceType =
                        ProvidedTypeDefinition(
                            assembly,
                            NameSpace.toString nameSpace,
                            sequence.Name,
                            baseType = Some typeof<obj>,
                            hideObjectMethods = false,
                            isErased = false
                        )

                    providedSequenceType.AddMember
                    <| ProvidedConstructor(
                        elements
                        |> List.choose ((fun element -> element.Parameter) >> id)
                        |> List.map fst,
                        invokeCode =
                            fun args ->
                                match args with
                                | this :: args ->
                                    List.zip
                                        args
                                        (elements
                                         |> List.choose (fun element ->
                                             element.Parameter |> Option.map (fun _ -> element.Field)))
                                    |> List.map (fun (arg, field) -> Expr.FieldSet(this, field, arg))
                                    |> (fun exprs ->
                                        match exprs with
                                        | [] -> Expr.Value(())
                                        | expr :: exprs ->
                                            List.fold (fun first second -> Expr.Sequential(first, second)) expr exprs)
                                | _ -> failwith "wrong ctor parameters"
                    )

                    elements
                    |> List.iter (fun element ->
                        providedSequenceType.AddMembers [ element.Field :> MemberInfo; element.Property :> MemberInfo ])

                    providedSequenceType |> Ok
                | Error errors -> Element(name, errors) |> Error

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
        | UnknownStatus of Label: Label * Status: string
        | MissingValue of Label: Label
        | TypeInconsistent of Label: Label

    type EnumerationError =
        | Empty of Name: Name
        | Enumerator of Name: Name * list<EnumeratorError>
        | Duplicates of Name: Name * list<Label>
        | UnknownType of Name: Name * Type: string
        | UnknownStatus of Name: Name * Status: string

    type EnumerationField =
        | String of Label: Label * Description: Description
        | Int32 of Label: Label * Value: int32 * Description: Description

    let private schemaEnumeratorToEnumerationField (enumerator: BlpSchema.Enumerator) =
        let label = Label.ofString enumerator.Name

        match (enumerator.Status |> Option.map Union.fromString<Status>) with
        | None
        | Some(Some Active) ->
            let description = Description.ofString enumerator.Description

            match enumerator.Value.Int32, enumerator.Value.String with
            | Some value, None ->
                try
                    Ok
                    <| EnumerationField.Int32(Label = label, Value = int32 value, Description = description)
                with :? FormatException ->
                    Int32Format(label, value) |> Error
            | None, Some value when
                value = enumerator.Name
                || (value = "THAIL" && enumerator.Name = "THAI")
                || (value = "THAIK" && enumerator.Name = "THAI")
                ->
                Ok <| EnumerationField.String(Label = label, Description = description)
            | None, Some value -> StringInconsistent(label, value) |> Error
            | None, None -> MissingValue(label) |> Error
            | Some _, Some _ -> MultipleValues(label) |> Error
        | Some None -> EnumeratorError.UnknownStatus(label, enumerator.Status.Value) |> Error

    let internal effectiveEnumerationType
        (name: Name)
        (enumerationType: EnumerationType)
        (enumerators: EnumerationField list)
        =
        match enumerationType with
        | EnumerationType.String ->
            enumerators
            |> List.map (fun enumerationField ->
                match enumerationField with
                | EnumerationField.String(label, documentation) -> Ok(label, documentation)
                | EnumerationField.Int32(label, _, _) -> Error <| TypeInconsistent(label))
            |> List.fold Result.folder (Result.Ok [])
            |> Result.map (Map.ofList >> Enumeration.String)
            |> Result.mapError (fun enumerators -> Enumerator(name, enumerators))
        | EnumerationType.Int32 ->
            enumerators
            |> List.map (fun enumeratorField ->
                match enumeratorField with
                | EnumerationField.String(label, _) -> Error <| TypeInconsistent(label)
                | EnumerationField.Int32(label, value, description) -> Ok((label, value), description))
            |> List.fold Result.folder (Result.Ok [])
            |> Result.map (Map.ofList >> Enumeration.Int32)
            |> Result.mapError (fun errors -> Enumerator(name, errors))

    let internal createEnumerationType
        (assembly: Assembly)
        (nameSpace: NameSpace)
        (name: Name)
        (enumerationType: EnumerationType)
        (description: Description)
        (enumerators: list<EnumerationField>)
        =
        let duplicates =
            enumerators
            |> Seq.countBy (fun enumerationField ->
                match enumerationField with
                | String(label, _)
                | Int32(label, _, _) -> label)
            |> List.ofSeq
            |> List.filter (fun (_, count) -> count > 1)
            |> List.map fst

        match enumerators with
        | [] -> Error <| Empty(name)
        | _ when not (duplicates |> List.isEmpty) -> Error <| Duplicates(name, duplicates)
        | _ ->
            let providedEnumType =
                ProvidedTypeDefinition(
                    assembly,
                    NameSpace.toString nameSpace,
                    Name.toString name,
                    baseType = Some typeof<Enum>,
                    hideObjectMethods = true,
                    isErased = false
                )

            let description = Description.toString description
            providedEnumType.AddXmlDoc(description)

            effectiveEnumerationType name enumerationType enumerators
            |> Result.map (fun enumeration ->
                let enumerators =
                    match enumeration with
                    | Enumeration.Int32 enumerators ->
                        enumerators
                        |> Map.toList
                        |> List.map (fun ((label, value), description) ->
                            let label = Label.toString label
                            ProvidedField.Literal(label, providedEnumType, value), description)
                    | Enumeration.String enumerators ->
                        enumerators
                        |> Map.toList
                        |> List.map (fun (label, description) ->
                            let label = Label.toString label
                            ProvidedField.Literal(label, providedEnumType, label), description)
                    |> List.map (fun (providedField, description) ->
                        let description = Description.toString description

                        match description with
                        | "" -> providedField
                        | _ ->
                            providedField.AddXmlDoc(description)
                            providedField)

                providedEnumType.AddMembers enumerators
                providedEnumType)

    let createType (enumeration: BlpSchema.EnumerationType) (assembly: Assembly) (nameSpace: NameSpace) =
        let name = Name.ofString enumeration.Name

        match enumeration.Status |> Option.map Union.fromString<Status> with
        | Some None -> EnumerationError.UnknownStatus(name, enumeration.Status.Value) |> Error
        | None
        | Some(Some Active) ->
            enumeration.Enumerators
            |> Array.map schemaEnumeratorToEnumerationField
            |> Array.fold Result.folder (Result.Ok [])
            |> Result.mapError (fun errors -> EnumerationError.Enumerator(name, errors))
            |> Result.bind (fun enumerators ->
                match Union.fromString<EnumerationType> (enumeration.Type) with
                | Some enumeration -> enumeration |> Ok
                | None -> UnknownType(name, enumeration.Type) |> Error
                |> Result.bind (fun enumerationType ->
                    createEnumerationType
                        assembly
                        nameSpace
                        name
                        enumerationType
                        (Description.ofString enumeration.Description)
                        enumerators))

module Choice =
    type ElementError = UnknownType of Name: Name * TypeName: string

    type ChoiceError =
        | Empty of Name: Name
        | Enumeration of Enumeration.EnumerationError
        | UnknownStatus of Name: Name * Status: string
        | Element of Name: Name * list<ElementError>

    type ChoiceType =
        { Enum: ProvidedTypeDefinition
          Object: ProvidedTypeDefinition }

    let createType
        (choice: BlpSchema.ChoiceType)
        (assembly: Assembly)
        (nameSpace: NameSpace)
        (tryGetType: string -> option<Type>)
        =
        let providedChoiceType =
            ProvidedTypeDefinition(
                assembly,
                NameSpace.toString nameSpace,
                choice.Name,
                baseType = Some typeof<obj>,
                hideObjectMethods = false,
                isErased = false
            )

        let choiceName = Name.ofString choice.Name

        match choice.Status |> Option.map Union.fromString<Status> with
        | Some None -> ChoiceError.UnknownStatus(choiceName, choice.Status.Value) |> Error
        | None
        | Some(Some Active) ->
            let enumerators =
                choice.Elements
                |> List.ofArray
                |> List.map (fun element ->
                    Enumeration.EnumerationField.String(
                        Label.ofString element.Name,
                        Description.ofString element.Description
                    ))

            let enumName = sprintf "%sType" choice.Name |> Name.ofString

            Enumeration.createEnumerationType
                assembly
                nameSpace
                enumName
                Enumeration.EnumerationType.String
                (Description.ofString choice.Description)
                enumerators
            |> Result.mapError Enumeration
            |> Result.bind (fun enum ->
                let enumField, property = Sequence.createImmutableProperty enumName enum
                providedChoiceType.AddMember enumField
                providedChoiceType.AddMember property

                let elements =
                    choice.Elements
                    |> Array.map (fun element ->
                        let name = Name.ofString element.Name

                        match tryGetType (element.Type) with
                        | Some propertyType ->
                            let field, property =
                                Sequence.createImmutableProperty name (makeOptionType (propertyType))

                            providedChoiceType.AddMember field
                            providedChoiceType.AddMember property
                            (propertyType, field) |> Result.Ok
                        | None -> UnknownType(name, element.Type) |> Result.Error)

                    |> Array.fold Result.folder (Result.Ok [])

                elements
                |> Result.mapError (fun errors -> ChoiceError.Element(choiceName, errors))
                |> Result.map (fun fields ->
                    fields
                    |> List.map snd
                    (* TODO: Allow declaring constructors with identical arity, current blocker is the inability to create a private constructor *)
                    |> List.distinctBy (fun field -> field.FieldType.Name)
                    |> List.iter (fun field ->
                        let parameter = ProvidedParameter(field.Name, field.FieldType)

                        providedChoiceType.AddMember
                        <| ProvidedConstructor(
                            [ parameter ],
                            invokeCode =
                                fun args ->
                                    match args with
                                    | this :: [ arg ] ->
                                        let setValue = Expr.FieldSet(this, field, arg)

                                        let enumField =
                                            Expr.FieldSet(this, enumField, Expr.FieldGet(enum.GetField(field.Name)))

                                        Expr.Sequential(enumField, setValue)
                                    | _ -> failwith "wrong ctor parameters"
                        ))

                    { Enum = enum
                      Object = providedChoiceType }))

type TypeError =
    | EnumerationError of Enumeration.EnumerationError
    | ChoiceError of Choice.ChoiceError
    | SequenceError of Sequence.SequenceError

open System.IO
open System.Collections.Concurrent

[<TypeProvider>]
type BasicGenerativeProvider(config: TypeProviderConfig) as this =
    inherit
        TypeProviderForNamespaces(
            config,
            assemblyReplacementMap = [ ("BloombergProvider.DesignTime", "BloombergProvider.Runtime") ]
        )

    let nameSpace = NameSpace.ofString "AlphaArchitect"

    let createRootType (typeName, schemaPath: string) =
        let tempAssembly = ProvidedAssembly()

        let schema = BlpSchema.Load(schemaPath)

        let rootType =
            ProvidedTypeDefinition(
                tempAssembly,
                NameSpace.toString nameSpace,
                typeName,
                baseType = Some typeof<obj>,
                hideObjectMethods = true,
                isErased = false
            )

        let dependencyOrder =
            DependencyGraph.create(schema.Schema).TopologicalSort()
            |> List.ofSeq
            |> List.rev

        let mutable typeCache = Map.empty<string, Type>

        let types =
            dependencyOrder
            |> List.map (fun complex ->
                let typeRef =
                    match complex with
                    | Sequence sequence ->
                        Sequence.createType sequence tempAssembly nameSpace typeCache.TryFind
                        |> Result.map (fun providedType -> [ providedType ])
                        |> Result.mapError TypeError.SequenceError
                    | Choice choice ->
                        Choice.createType choice tempAssembly nameSpace typeCache.TryFind
                        |> Result.map (fun providedType -> [ providedType.Enum; providedType.Object ])
                        |> Result.mapError TypeError.ChoiceError
                    | Enumeration enumeration ->
                        Enumeration.createType enumeration tempAssembly nameSpace
                        |> Result.map (fun providedType -> [ providedType ])
                        |> Result.mapError TypeError.EnumerationError

                match typeRef with
                | Result.Ok providedTypes ->
                    (* Depends on the ordering scheme above, we are adding the choice object not enum *)
                    let lastProvidedType = List.last providedTypes
                    typeCache <- typeCache |> Map.add lastProvidedType.Name (lastProvidedType :> Type)
                | _ -> ()

                typeRef)
            |> List.fold Result.folder (Result.Ok [])
            |> Result.map List.concat

        match types with
        | Ok providedTypes -> rootType.AddMembers providedTypes
        | Error errors -> failwithf "%A" errors

        tempAssembly.AddTypes [ rootType ]
        rootType

    let assembly = Assembly.GetExecutingAssembly()
    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<AlphaArchitect.Runtime>.Assembly.GetName().Name = assembly.GetName().Name)


    let cache = ConcurrentDictionary<string, Lazy<ProvidedTypeDefinition>>()

    let rootType =
        let rootType =
            ProvidedTypeDefinition(
                assembly,
                NameSpace.toString nameSpace,
                "BloombergProvider",
                Some typeof<obj>,
                hideObjectMethods = true,
                isErased = false
            )

        let staticParams = [ ProvidedStaticParameter("Schema", typeof<string>) ]

        rootType.AddXmlDoc
            """
<summary>Bloomberg Provider types based on blp schema file.</summary>
<param name='SchemaFile'>The full path to the schema file to base this provider on.</param>
"""

        rootType.DefineStaticParameters(
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

        rootType

    do this.AddNamespace(NameSpace.toString nameSpace, [ rootType ])

[<TypeProviderAssembly>]
do ()
