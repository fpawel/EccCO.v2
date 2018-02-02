#I @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.0\"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "WindowsBase.dll"
#r "System.Xaml.dll"
#r "System.Drawing.dll"
#r "System.Security.dll"
#r "System.Xml.dll"

#r "UIAutomationTypes.dll"

module WPFEventLoop =     
    open System    
    open System.Text
    open System.Diagnostics
    open System.Windows    
    open System.Windows.Threading    
    open Microsoft.FSharp.Compiler.Interactive    
    open Microsoft.FSharp.Compiler.Interactive.Settings



    type BindingErrorTraceListener()  =
        inherit  TraceListener()

        let messageBuilder = StringBuilder()

        override x.Write(message:string) =
           messageBuilder.Append(message) |> ignore

        override x.WriteLine(message:string) =
           x.Write(message)
           
           printfn "%s" <| message.ToString()
           messageBuilder.Clear() |> ignore

    type RunDelegate<'b> = delegate of unit -> 'b         
    fsi.EventLoop <-
        let app  = 
            let app = Application.Current
            if app<>null then app else                        
            let app = Application.LoadComponent(new System.Uri("/UI;component/App.xaml", UriKind.Relative)) :?> Application in
            Window() |> ignore            
            PresentationTraceSources.DataBindingSource.Listeners.Add(new BindingErrorTraceListener()) |> ignore
            PresentationTraceSources.DataBindingSource.Switch.Level <- SourceLevels.Error 
            app
        {   new IEventLoop with             
                member x.Run() =                    
                    app.Run() |> ignore                 
                    false
                member x.Invoke(f) =                  
                    try 
                        let x = RunDelegate<_>( fun () -> box(f ()))
                        app.Dispatcher.Invoke( DispatcherPriority.Send, x) |> unbox
                    with  e -> 
                        eprintf "\n\n ERROR: %O\n" e
                        reraise()             

                member x.ScheduleRestart() = () }     

    

