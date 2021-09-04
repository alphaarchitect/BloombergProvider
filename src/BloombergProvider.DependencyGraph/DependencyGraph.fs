namespace BloombergProvider

module DependencyGraph =
    open QuikGraph
    open Schema
    open AlphaArchitect.Common
    
    let create (schema: BlpSchema.Schema) =
        let typeArray =
            [|schema.EnumerationTypes |> Array.map (fun enumeration -> enumeration.Name, ComplexType.Enumeration enumeration);
              schema.SequenceTypes |> Array.map (fun sequence -> sequence.Name, ComplexType.Sequence sequence);
              schema.ChoiceTypes |> Array.map (fun choice -> choice.Name, ComplexType.Choice choice)|]
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
        |> Array.collect (fun sequence ->
            sequence.Elements
            |> Array.map (fun element -> element.Type)
            |> Array.distinct
            |> Array.filter (fun type' -> Union.fromString<PrimitiveType>(type').IsNone)
            |> Array.map (fun type' -> Edge(ComplexType.Sequence sequence, typeMap.[type'])))
        |> graph.AddVerticesAndEdgeRange
        |> ignore

        schema.ChoiceTypes
        |> Array.collect (fun choice ->
            choice.Elements
            |> Array.map (fun element -> element.Type)
            |> Array.distinct
            |> Array.map (fun type' -> Edge(ComplexType.Choice choice, typeMap.[type'])))
        |> graph.AddVerticesAndEdgeRange
        |> ignore

        graph
