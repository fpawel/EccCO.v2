module Chart

open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

open System.Windows.Media
type private Color1 = System.Drawing.Color

let private color1 (color : Color ) = 
    Color1.FromArgb(int color.R, int color.G, int color.B)

let private color2 color = 
    ColorConverter.ConvertFromString(color) :?> Color |> color1


let createNewChart () =    
    let chart = new Chart( Palette = ChartColorPalette.Bright)
    let ar = new ChartArea()
    chart.ChartAreas.Add(ar) 
    chart
    

let createChartImage (width,height) f =    
    let chart = createNewChart()
    chart.Width <- width
    chart.Height <- height 
    f chart
    let bmp = new Bitmap(width, height)
    chart.DrawToBitmap(bmp, new System.Drawing.Rectangle(0, 0, width, height))
    let memory = new System.IO.MemoryStream() 
    bmp.Save(memory, Imaging.ImageFormat.Png)
    memory

type Points = (decimal * decimal) list 

let initializeProductChart (chart : Chart) =
    let series1 = new Series(ShadowOffset=5, ChartType=SeriesChartType.FastLine, BorderWidth = 2, Color = color2 "#FF84BB96" )    
    chart.Series.Add(series1)
        
    let series2 = new Series(ShadowOffset=5, ChartType=SeriesChartType.FastLine, BorderWidth = 2, Color = Color1.FromArgb(86,156,214), 
                                YAxisType = AxisType.Secondary)
    chart.Series.Add(series2)
        
    let ar = chart.ChartAreas.[0]
        
    let dark = color2 "#FF252525"
    let grey = color1 Colors.DarkGray
    chart.BackColor <- dark
    ar.BackColor <- dark
    chart.ForeColor <- grey
    ar.AxisX.LineColor <- grey

    ar.AxisY.LineColor <- grey
    ar.AxisX.TitleForeColor <- grey
    ar.AxisY.TitleForeColor <- grey
    ar.AxisX.LabelStyle.ForeColor <- grey
    ar.AxisY.LabelStyle.ForeColor <- grey
    ar.AxisY.MajorGrid.LineColor <- grey
    ar.AxisX.MajorGrid.LineColor <- grey

    ar.AxisX.InterlacedColor <- grey
    ar.AxisY.InterlacedColor <- grey

    ar.AxisY.MajorTickMark.LineColor <- grey
    ar.AxisX.MajorTickMark.LineColor <- grey
    ar.AxisY.LabelStyle.ForeColor <- series1.Color

            
    ar.AxisY2.TitleForeColor <- series2.Color
    ar.AxisY2.LabelStyle.ForeColor <- series2.Color
    ar.AxisY2.LineColor <- grey
    ar.AxisY2.MajorGrid.LineColor <- grey

    series1, series2


//let create1 wh (ser1,ser2) = 
//    createChartImage wh <| fun chart ->
//        let series1, series2 = initializeECCChart chart        
//        for (x:decimal),(y : decimal) in ser1 do
//            series1.Points.AddXY(x, y) |> ignore
//        for (x:decimal),(y : decimal) in ser2 do
//            series2.Points.AddXY(x, y) |> ignore
        