namespace BloombergProvider

module Schema =
    open FSharp.Data

    [<Literal>]
    let private BlpSchemaPath =
        __SOURCE_DIRECTORY__ + "\\blp.schema.xsd"

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

module DependencyGraph =
    open QuikGraph
    open QuikGraph.Graphviz
    open QuikGraph.Graphviz.Dot
    open Schema
    open AlphaArchitect.Common
    
    let create (schema: BlpSchema.Schema) =
        let typeArray =
            [|schema.EnumerationTypes |> Array.map (fun x -> x.Name, ComplexType.Enumeration x);
              schema.SequenceTypes |> Array.map (fun x -> x.Name, ComplexType.Sequence x);
              schema.ChoiceTypes |> Array.map (fun x -> x.Name, ComplexType.Choice x)|]
            |> Array.concat

        let graph = QuikGraph.AdjacencyGraph()
        typeArray
        |> Array.map snd
        |> graph.AddVertexRange
        |> ignore

        let typeMap =
            typeArray
            |> Map.ofArray

        schema.SequenceTypes
        |> Array.collect (fun s ->
            s.Elements
            |> Array.map (fun e -> e.Type)
            |> Array.distinct
            |> Array.filter (fun t -> Union.fromString<PrimitiveType>(t).IsNone)
            |> Array.map (fun t -> Edge(ComplexType.Sequence s, typeMap.[t])))
        |> graph.AddVerticesAndEdgeRange
        |> ignore

        schema.ChoiceTypes
        |> Array.collect (fun c ->
            c.Elements
            |> Array.map (fun e -> e.Type)
            |> Array.distinct
            |> Array.map (fun t -> Edge(ComplexType.Choice c, typeMap.[t])))
        |> graph.AddVerticesAndEdgeRange
        |> ignore

        graph

    let toGraphviz(graph: AdjacencyGraph<ComplexType, Edge<ComplexType>>) =
        let g = GraphvizAlgorithm(graph)
        g.CommonVertexFormat.Shape <- GraphvizVertexShape.Diamond
        g.FormatVertex.AddHandler(fun _ args ->
            let label = match args.Vertex with
                        | Enumeration e -> sprintf "Enumeration: %s" e.Name
                        | Sequence s -> sprintf "Sequence: %s" s.Name
                        | Choice c -> sprintf "Choice: %s" c.Name
            args.VertexFormat.Label <- label)
        g

    let writeDotFile<'a>(ga: GraphvizAlgorithm<'a, Edge<'a>>, fileName: string) =
        ga.Generate(new FileDotEngine(), fileName)
        |> ignore