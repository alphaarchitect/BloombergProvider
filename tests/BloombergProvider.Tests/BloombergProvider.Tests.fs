module BloombergProviderTests

open NUnit.Framework

open AlphaArchitect
open BloombergProvider
open BloombergProvider.Schema

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
let simpleSchemaPath = schemaPath + "blp.simple.schema.xml"
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
type Simple = BloombergProvider<Schema=simpleSchemaPath>
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
        simpleSchemaPath;
        srcRefSchemaPath;
        taSvcSchemaPath;
    ]

[<Test>]
let ``Can access properties of generative provider`` () =
    let s = Simple.Simple(language = Some Simple.Language.ENGLISH, properties = ("test", []), returnFieldDocumentation = false, id = ("1", []))
    Assert.AreEqual(s.returnFieldDocumentation, false)
    let (p, _) = s.properties
    Assert.AreEqual(p, "test")
    let (id, _) = s.id
    Assert.AreEqual(id, "1")

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
        DependencyGraph.writeDotFile(DependencyGraph.toGraphviz(graph), Path.Combine(Path.GetDirectoryName(schemaPath), Path.GetFileNameWithoutExtension(schemaPath))))