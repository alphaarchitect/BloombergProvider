module BloombergProviderTests

open NUnit.Framework

open AlphaArchitect
open BloombergProvider
open BloombergProvider.Schema
open QuikGraph.Graphviz
open QuikGraph.Graphviz.Dot
open QuikGraph

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

[<Literal>]
let schemaPath = __SOURCE_DIRECTORY__ + "\\schemas\\"
[<Literal>]
let apiFldsSchemaPath = schemaPath + "blp.apiflds.schema.xml"
[<Literal>]
let instrumentsSchemaPath = schemaPath + "blp.instruments.schema.xml"
[<Literal>]
let irdCtk3SchemaPath = schemaPath + "blp.irdctk3.schema.xml"
[<Literal>]
let mktBarSchemaPath = schemaPath + "blp.mktbar.schema.xml"
[<Literal>]
let mktDataSchemaPath = schemaPath + "blp.mktdata.schema.xml"
[<Literal>]
let mktDepthDataSchemaPath = schemaPath + "blp.mktdepthdata.schema.xml"
[<Literal>]
let mktListSchemaPath = schemaPath + "blp.mktlist.schema.xml"
[<Literal>]
let mktVwapSchemaPath = schemaPath + "blp.mktvwap.schema.xml"
[<Literal>]
let pageDataSchemaPath = schemaPath + "blp.pageData.schema.xml"
[<Literal>]
let refDataSchemaPath = schemaPath + "blp.refdata.schema.xml"
[<Literal>]
let exampleSchemaPath = schemaPath + "blp.example.schema.xml"
[<Literal>]
let srcRefSchemaPath = schemaPath + "blp.srcref.schema.xml"
[<Literal>]
let taSvcSchemaPath = schemaPath + "blp.tasvc.schema.xml"

type ApiFlds = BloombergProvider<Schema=apiFldsSchemaPath>
type Instruments = BloombergProvider<Schema=instrumentsSchemaPath>
type IrdCtk3 = BloombergProvider<Schema=irdCtk3SchemaPath>
type MktBar = BloombergProvider<Schema=mktBarSchemaPath>
type MktData = BloombergProvider<Schema=mktDataSchemaPath>
type MktDepthData = BloombergProvider<Schema=mktDepthDataSchemaPath>
type MktList = BloombergProvider<Schema=mktListSchemaPath>
type MktVwap = BloombergProvider<Schema=mktVwapSchemaPath>
type PageData = BloombergProvider<Schema=pageDataSchemaPath>
type RefData = BloombergProvider<Schema=refDataSchemaPath>
type Example = BloombergProvider<Schema=exampleSchemaPath>
type SrcRef = BloombergProvider<Schema=srcRefSchemaPath>
type TaSVC = BloombergProvider<Schema=taSvcSchemaPath>

let schemas =
    [
        apiFldsSchemaPath;
        instrumentsSchemaPath;
        irdCtk3SchemaPath;
        mktBarSchemaPath;
        mktDataSchemaPath;
        mktDepthDataSchemaPath;
        mktListSchemaPath;
        mktVwapSchemaPath;
        pageDataSchemaPath;
        refDataSchemaPath;
        exampleSchemaPath;
        srcRefSchemaPath;
        taSvcSchemaPath;
    ]

[<Test>]
let ``Can access properties of generative provider`` () =
    let s = Example.SecurityRequest(name = "AAPL US Equity")
    Assert.AreEqual(s.name, "AAPL US Equity")
    let udf = Example.UserDefinedField(name = "Score", value = "1"), [Example.UserDefinedField(name = "Test", value = "Passed"); Example.UserDefinedField(name = "Core", value = "Seeds")]
    let s = Example.SecurityResponse(currency = Some Example.Currency.CAD, userDefined = udf, name = "AAPL US Equity")
    Assert.AreEqual(s.currency, Some Example.Currency.CAD)
    Assert.AreEqual(s.userDefined, udf)
    Assert.AreEqual(s.name, "AAPL US Equity")

open QuikGraph.Algorithms
[<Test>]
let ``Schemas are DAG`` () =
    schemas
    |> List.iter (fun schemaPath ->
        let schema = BlpSchema.Load(schemaPath)
        let graph = DependencyGraph.create(schema.Schema)
        Assert.IsTrue(graph.IsDirectedAcyclicGraph()))

open System.IO
[<Test>]
let ``Write Schemas As Dot Files`` () =
    schemas
    |> List.iter (fun schemaPath ->
        let schema = BlpSchema.Load(schemaPath)
        let graph = DependencyGraph.create(schema.Schema)
        writeDotFile(toGraphviz(graph), Path.Combine(Path.GetDirectoryName(schemaPath), Path.GetFileNameWithoutExtension(schemaPath))))