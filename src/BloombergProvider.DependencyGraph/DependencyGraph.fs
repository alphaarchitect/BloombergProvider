namespace BloombergProvider

module DependencyGraph =
    open QuikGraph
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
