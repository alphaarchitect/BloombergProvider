namespace BloombergProvider

module Schema =
    open FSharp.Data

    [<Literal>]
    let private BlpSchemaPath = __SOURCE_DIRECTORY__ + "\\blp.schema.xsd"

    type BlpSchema =
        XmlProvider<Schema=BlpSchemaPath, Global=true, InferTypesFromValues=true, EmbeddedResource="BloombergProvider.DesignTime, BloombergProvider.DesignTime.blpschema.xsd">

    type ComplexType =
        | Sequence of BlpSchema.SequenceType
        | Enumeration of BlpSchema.EnumerationType
        | Choice of BlpSchema.ChoiceType

    type PrimitiveType =
        | Boolean
        | Char
        | Date
        | Datetime
        | Float32
        | Float64
        | Int32
        | Int64
        | String
        | Time
