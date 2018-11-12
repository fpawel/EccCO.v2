namespace EccCO.v2 

module Config =

    open System.Windows.Forms
    open System.Configuration
    open System.IO

    let private config = ConfigurationManager.OpenExeConfiguration(Application.ExecutablePath)
    let private sets = config.AppSettings.Settings    

    let mutable rootPath = try sets.["rootPath"].Value with _ -> ""

    let setup() = 
        if Directory.Exists(rootPath) &&
            MessageBox.Show
                (   "Этот путь к каталогу сохранённых партий ЭХЯ правильный?\n\n" + rootPath, 
                    "Важный вопрос", 
                    MessageBoxButtons.YesNo) <> DialogResult.Yes then
            rootPath <- ""                   
        if not (Directory.Exists rootPath) then
            let folderBrowserDialog = new FolderBrowserDialog()
            if folderBrowserDialog.ShowDialog() <> DialogResult.OK then  
                failwith "you must browse folder"
            rootPath <- folderBrowserDialog.SelectedPath
            sets.Remove "rootPath"
            sets.Add("rootPath", folderBrowserDialog.SelectedPath)
            config.Save(ConfigurationSaveMode.Minimal)
        

