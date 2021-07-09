module BloombergProviderTests

open NUnit.Framework

open AlphaArchitect

[<Literal>]
let schemaPath = __SOURCE_DIRECTORY__ + "\\schemas\\"

[<Literal>]
let apiFldsSchemaPath = schemaPath + "blp.apiflds.schema.xml"

[<Literal>]
let instrumentsSchemaPath =
    schemaPath + "blp.instruments.schema.xml"

[<Literal>]
let irdCtk3SchemaPath = schemaPath + "blp.irdctk3.schema.xml"

[<Literal>]
let mktBarSchemaPath = schemaPath + "blp.mktbar.schema.xml"

[<Literal>]
let mktDataSchemaPath = schemaPath + "blp.mktdata.schema.xml"

[<Literal>]
let mktDepthDataSchemaPath =
    schemaPath + "blp.mktdepthdata.schema.xml"

[<Literal>]
let mktListSchemaPath = schemaPath + "blp.mktlist.schema.xml"

[<Literal>]
let mktVwapSchemaPath = schemaPath + "blp.mktvwap.schema.xml"

[<Literal>]
let pageDataSchemaPath = schemaPath + "blp.pageData.schema.xml"

[<Literal>]
let refDataSchemaPath = schemaPath + "blp.refdata.schema.xml"

[<Literal>]
let srcRefSchemaPath = schemaPath + "blp.srcref.schema.xml"

[<Literal>]
let taSvcSchemaPath = schemaPath + "blp.tasvc.schema.xml"

type ApiFlds = BloombergProvider<Schema=apiFldsSchemaPath>
type Instruments = BloombergProvider<Schema=instrumentsSchemaPath>
type IrdCtk3 = BloombergProvider<Schema=irdCtk3SchemaPath>
(* type MktBar = BloombergProvider.BloombergProvider<Schema = mktBarSchemaPath> *)
type MktData = BloombergProvider<Schema=mktDataSchemaPath>
type MktDepthData = BloombergProvider<Schema=mktDepthDataSchemaPath>
type MktList = BloombergProvider<Schema=mktListSchemaPath>
type MktVwap = BloombergProvider<Schema=mktVwapSchemaPath>
type PageData = BloombergProvider<Schema=pageDataSchemaPath>
type RefData = BloombergProvider<Schema=refDataSchemaPath>
type SrcRef = BloombergProvider<Schema=srcRefSchemaPath>
type TaSVC = BloombergProvider<Schema=taSvcSchemaPath>

[<Test>]
let ``Can access properties of generative provider 4`` () = ()
