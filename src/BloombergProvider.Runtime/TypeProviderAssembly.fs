namespace AlphaArchitect

open FSharp.Core.CompilerServices

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly: TypeProviderAssembly("BloombergProvider.DesignTime")>]
do ()
