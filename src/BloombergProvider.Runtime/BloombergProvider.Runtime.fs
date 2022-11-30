namespace AlphaArchitect

type Runtime = Empty

#if !IS_DESIGNTIME
// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly: CompilerServices.TypeProviderAssembly("BloombergProvider.DesignTime.dll")>]
do ()
#endif
