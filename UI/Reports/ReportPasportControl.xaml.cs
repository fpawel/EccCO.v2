using System.Windows.Controls;

namespace UI.Reports
{
    /// <summary>
    /// Логика взаимодействия для ReportPasportControl.xaml
    /// </summary>
    public partial class ReportPasportControl : UserControl
    {
        public ReportPasportControl()
        {
            InitializeComponent();
        }
        public void SetDax()
        {
            this.SectionProduct.Blocks.Remove(TableSou);
        }
        public void SetSou()
        {
            this.SectionProduct.Blocks.Remove(TableDax);
            this.SectionProduct.Blocks.Remove(SectionTableDax);
        }
    }
}
