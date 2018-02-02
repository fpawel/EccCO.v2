open System
open System.Windows

[<STAThread>] 
do 
    try
        Application.run()
    with e ->
        log.Fatal ( sprintf "Выход с необработанным исключением %A" e )
        sprintf "Произошла ошибка. Приложение будет закрыто.\n\n%A" e.Message
        |> messageBoxFail "ECC.CO.v2" 

    

