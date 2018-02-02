using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Threading.Tasks;


namespace UI
{
    /// <summary>
    /// Логика взаимодействия для PartiesTreeListWidget.xaml
    /// </summary>
    public partial class WgtPartiesTree : UserControl
    {
        public WgtPartiesTree()
        {
            InitializeComponent();
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            var b = (sender as Button);
            var popup = this.PopupConfirmDelete;
            popup.IsOpen = false;
            popup.PlacementTarget = b;
            popup.IsOpen = true;
        }
        private void Button_Click_2(object sender, RoutedEventArgs e)
        {
            PopupConfirmDelete.IsOpen = false;
        }
    }
}
