module ChartImage

open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

type Settings = 
    {   Title : string option
        XTitle : string option
        YTitle : string option 
        Width : int
        Height : int }
let settings w h = 
    {   Title  = None
        XTitle = None
        YTitle = None
        Width = w
        Height = h }

let createBitmap points fxy sets = 
    let ar = new ChartArea()
    let chart = new Chart( Visible=true, Width = sets.Width, Height = sets.Height,
                           Palette = ChartColorPalette.Bright)
    chart.ChartAreas.Add(ar)
    match sets.Title with
    | Some x -> chart.Titles.Add( new Title(x))
    | _ -> ()
    match sets.XTitle with
    | Some x -> ar.AxisX.Title <- x
    | _ -> ()
    match sets.YTitle with
    | Some x -> ar.AxisY.Title <- x
    | _ -> ()

    let points = List.sort points

    match points, List.rev points with
    | (x0:decimal,_)::_, (xn:decimal,_)::_ when x0<xn -> 
        let series = new Series(ChartType = SeriesChartType.Point, MarkerSize=10, MarkerStyle=MarkerStyle.Circle)
        for x,(y:decimal) in points do
            series.Points.AddXY(x, y) |> ignore
        chart.Series.Add(series)
        
        let series = new Series(ShadowOffset=5, ChartType=SeriesChartType.FastLine)
        let d = (xn-x0) / 500m
        let pointsY = points |> List.map( fun (_,y) -> y )
        let minY = List.min pointsY 
        let maxY = List.max pointsY 

        let rec loop n minY maxY = 
            let x = x0 + d*(decimal n)
            let y : decimal = fxy x
            series.Points.AddXY(x, y) |> ignore
            let minY = if y<minY then y else minY
            let maxY = if y>maxY then y else maxY
            if x<xn then loop (n+1) minY maxY else minY,maxY

        let minY,maxY =  loop 0 minY maxY
        chart.Series.Add(series)
        let ar = chart.ChartAreas.[0]

        let d = (xn-x0) * 0.1m
        ar.AxisX.Minimum <- float (x0 - d)
        ar.AxisX.Maximum <- float (xn + d)
        let d = if minY=maxY then 0.1m else (maxY - minY) * 0.1m
        ar.AxisY.Minimum <- float(minY - d)
        ar.AxisY.Maximum <- float(maxY + d)
        let bmp = new Bitmap(sets.Width, sets.Height)
        chart.DrawToBitmap(bmp, new System.Drawing.Rectangle(0, 0, sets.Width, sets.Height))
        let memory = new System.IO.MemoryStream() 
        bmp.Save(memory, Imaging.ImageFormat.Png)
        Some memory
    | _ -> None
        

    
    
