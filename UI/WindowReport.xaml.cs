using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;

namespace UI
{
    /// <summary>
    /// Логика взаимодействия для WindowReport.xaml
    /// </summary>
    public partial class WindowReport : Window
    {
        public WindowReport(int fontsize=10)
        {
            InitializeComponent();
            m_docFontSize = fontsize;
        }

        int m_docFontSize = 8;
        public int DocFontSize
        {
            get { return m_docFontSize;  }
        }

        int m_SummaryTableFontSize = 14;
        public int SummaryTableFontSize
        {
            get { return m_SummaryTableFontSize; }
        }

        
        public string ReportTable_Party { get; set; }
        public string ReportTable_Gas { get; set; }
        public string ReportTable_Diap { get; set; }
        public string ReportTable_PGS { get; set; }
        public string ReportTable_Kind { get; set; }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            var printDialog = new PrintDialog();
            if (printDialog.ShowDialog() == true)
            {
                var paginator = ((IDocumentPaginatorSource)this.document).DocumentPaginator;

                try
                {
                    printDialog.PrintDocument(paginator, "Печать");
                }
                catch (Exception)
                {
                    MessageBox.Show(App.Current.MainWindow, "Ошибка печати. Проверьте настройки принтера.", "Ошибка печати",
                            MessageBoxButton.OK, MessageBoxImage.Error, MessageBoxResult.OK);
                }
            }
        }

        private void dialogWindow_Activated(object sender, EventArgs e)
        {
            this.Activated -= dialogWindow_Activated;
            this.erun_kind.Text = this.ReportTable_Kind;
            this.erun_party.Text = this.ReportTable_Party;
            this.erun_gas.Text = this.ReportTable_Gas;
            this.erun_diap.Text = this.ReportTable_Diap;
            this.erun_pgs.Text = this.ReportTable_PGS;
            this.erun_count.Text = (this.rowgroup_table_content.Rows.Count).ToString();
        }
    }
}
