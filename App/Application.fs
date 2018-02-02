module Application

open System
open System.Windows
open System.Diagnostics

[<AutoOpen>]
module private InitializeApplication = 
    let initializeWPFApplication (app:System.Windows.Application) = 

        PresentationTraceSources.DataBindingSource.Switch.Level <- SourceLevels.Error
        let _ =     
            let sb = Text.StringBuilder()        
            PresentationTraceSources.DataBindingSource.Listeners.Add 
                {   new TraceListener() with
                        override __.Write( message : string ) = 
                            sb.Append message |> ignore
                        override __.WriteLine( message : string ) = 
                            sb.Append message |> ignore
                            log.Error( sprintf "ОШИБКА БИНДИНГА, %A"  sb )
                            sb.Clear() |> ignore }     
        app.Exit.Add <| fun _ ->
            ProductTypeView.save()        
            AppSets.save()    
    
        app.DispatcherUnhandledException.Add( fun e -> 
            sprintf "Необработанное исключение UI\n%A" e.Exception |> log.Fatal
            e.Handled <- true
            UI.Log.Jouranl.add 
                (sprintf "Необработанное исключение %A. Подробности в лог-файле." e.Exception.Message)
                (sprintf "%A" e.Exception) 
                NLog.LogLevel.Fatal |> ignore)

    let createApplicationDataContext() =
        let d = Dynamic()
        ProductTypeView.setDataContext d
        PartiesView.setDataContext d
        MainwindowViewMode.setDataContext d
        d.["TemperatureInfo"] <- Work.temperature
        d.["Gases"] <- Var.gases
        d.["Gases1"] <- ""::Var.gases
        d.["Units"] <- Var.units
        d.["CalculateTermoMethods"] <- ["по двум точкам"; "по трём точкам"]
        //d.["CalculateTermoMethods1"] <- [""; "по двум точкам"; "по трём точкам"; "интерполяция"]
        
        d.["AppSets"] <- AppSets.sets
        d.["MenuScenaries"] <- Work.menu.Items
        d.["MenuScenaryTree"] <- Work.menu
        d.["Thread2"] <- Thread2.processInfo
        d.["BreakThread2"] <- Thread2.Break
        d.["Interrogate"] <- Work.interrogate.Perform
        d.["ProgParty"] <- Work.progParty.Perform
        d.["ProgInfo"] <- Programming.progInfo
        
        d.["LogJournalMessages"] <- UI.Log.Jouranl.journal

        d.["ReadFlashCommand"] <- Work.readFlash.Perform
        d.["WriteFlashCommand"] <- Work.writelash.Perform

        d.["AppVersion"] <- 
            try
                Reflection.Assembly.GetExecutingAssembly().GetName().Version                
            with e -> 
                failwithf "app version: %A" e
        

        d

    let initializeMainWindow (mainWindow:UI.MainWindow) = 
        let dataContext = createApplicationDataContext()       
        

        dataContext.["ShowSummaryTable"] <- wpfCommnad1 <| fun () ->
            let w = mainWindow.SummaryTable
            BatchView.Report.showSummaryTable PartiesView.curentPartyInfo.Party w.RowGroupSummaryTableContent
            w.RunCount.Text <- string w.RowGroupSummaryTableContent.Rows.Count
            MainwindowViewMode.state.State <- MainwindowViewMode.ReportSummaryTable

        let pasp isDax = wpfCommnad1 <| fun () ->
            BatchView.Report.showPasports PartiesView.curentPartyInfo.Party isDax mainWindow.SectionReportPasport
            MainwindowViewMode.state.State <- MainwindowViewMode.ReportPasport

        dataContext.["ShowPasportDax"] <- pasp true
        dataContext.["ShowPasportSou"] <- pasp false


        mainWindow.DataContext <- dataContext
        mainWindow.ChartTermoPlaceholder.Child <- BatchView.CalcTermo.chart
        mainWindow.ChartFlashPlaceholder.Child <- BatchView.FlashTermo.chart

        // обновление основонй таблицы данных после редактирования таблицы исполнений
        dataContext.["ApplyProductTypesEdit"] <- wpfCommnad1 <| fun () ->
            MainwindowViewMode.state.State <- MainwindowViewMode.Normal
            mainWindow.DataGridProducts.Items.Refresh()

        // показываемые колонки главной таблицы
        let cols = [ for x in mainWindow.DataGridProducts.Columns -> x ]
        cols |> List.iteri( fun n column ->
        column.Visibility <- if AppSets.sets.HiddenDataGridProductsColumns.Contains n then Visibility.Collapsed else Visibility.Visible )
        mainWindow.Closing.Add <| fun _ -> 
            AppSets.sets.HiddenDataGridProductsColumns <- 
                cols 
                |> List.mapi( fun n x -> n, x.Visibility <> Visibility.Visible )
                |> List.filter snd 
                |> List.map fst 
                |> Set.ofList

        Programmer.initialize(mainWindow,dataContext)

let run() =
    let application = Application.LoadComponent(new System.Uri("/UI;component/App.xaml", UriKind.Relative)) :?> Application
    initializeWPFApplication application  
    let mainWindow = 
        Application.LoadComponent(new System.Uri("/UI;component/MainWindow.xaml", UriKind.Relative))
        :?> UI.MainWindow    
    initializeMainWindow mainWindow

    application.DispatcherUnhandledException.Add( fun e -> 
        log.Fatal (sprintf "Необработанное исключение UI\n%A" e.Exception )
        e.Handled <- true
        UI.Log.Jouranl.add 
            (sprintf "Необработанное исключение %A. Подробности в лог-файле." e.Exception.Message)
            (sprintf "%A" e.Exception) 
            NLog.LogLevel.Fatal |> ignore)




    application.Run( mainWindow) |> ignore

    

