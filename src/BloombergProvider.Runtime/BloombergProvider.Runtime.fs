namespace AlphaArchitect

type Runtime = Empty

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly: CompilerServices.TypeProviderAssembly("BloombergProvider.DesignTime.dll")>]
do ()
