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
using System.Globalization;

using MahApps.Metro;
using MahApps.Metro.Controls;

namespace UI
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : MetroWindow
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void ShowPopup1(Control w, MenuItem menuItem)
        {
            
            w.Background = new SolidColorBrush((Color)ColorConverter.ConvertFromString("#FF252525"));
            new Popup()
            {
                Child = w,
                StaysOpen = false,
                PopupAnimation = PopupAnimation.Fade,
                AllowsTransparency = false,
                PlacementTarget = ((ContextMenu)menuItem.Parent).PlacementTarget,
                DataContext = menuItem.DataContext,
                IsOpen = true,
            };
        }
        
        private void ShowPopupOnButtonClick(object sender, RoutedEventArgs e)
        {
            var button = sender as Button;
            var popup = button.Tag as Popup;
            var pt = sender as UIElement;
            popup.PlacementTarget = sender as UIElement;
            popup.DataContext = button.DataContext;
            popup.IsOpen = true;
        }

        private void Print(FlowDocument d)
        {
            var printDialog = new PrintDialog();
            if (printDialog.ShowDialog() == true)
            {
                var paginator = ((IDocumentPaginatorSource)d).DocumentPaginator;

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

        private void Button_Click_OpenContextMenu(object sender, RoutedEventArgs e)
        {
            var menu = (sender as Button).ContextMenu;
            menu.PlacementTarget = sender as Button;
            menu.Placement =  PlacementMode.Bottom;
            menu.IsOpen = true;
        }

        private void MenuItem_Click(object sender, RoutedEventArgs e)
        {
            PopupBrowseRepository.IsOpen = true;
            //PopupBrowseRepository.PlacementTarget = (Button)sender;
        }

        private void OperationItemBorder_PreviewMouseDown(object sender, MouseButtonEventArgs e)
        {
            ButtonToolsMenu.ContextMenu.IsOpen = false;
            e.Handled = true;
            dynamic x = sender;
            System.Windows.Input.ICommand perform = x.DataContext.Perform;
            perform.Execute(null);
        }

        private void MenuItem_Click_1(object sender, RoutedEventArgs e)
        {
            var menuItem = sender as MenuItem;
            dynamic d = menuItem.DataContext;
            
            
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            this.Print(this.DocumentPasports);
        }

        private void Button_Click_1(object sender, RoutedEventArgs e)
        {
            this.Print(this.SummaryTable.Document1);
        }

        private void DataGrid_Selected(object sender, RoutedEventArgs e)
        {
            // Lookup for the source to be DataGridCell
            if (e.OriginalSource.GetType() == typeof(DataGridCell))
            {
                // Starts the Edit on the row;
                DataGrid grd = (DataGrid)sender;
                grd.BeginEdit(e);
            }
        }

        private void TextBox_PreviewTextInput(object sender, TextCompositionEventArgs e)
        {
            var textbox = (TextBox)sender;
            var len = textbox.SelectionStart + e.Text.Length;
            e.Handled = len > 2;            
        }
        private void TextBox_Loaded(object sender, RoutedEventArgs e)
        {
            EditingCommands.ToggleInsert.Execute(null, (TextBox) sender);
        }

        private void Button_Click_2(object sender, RoutedEventArgs e)
        {
            PopupReadFlashDialog.IsOpen = false;
        }

        private void MenuItem_Click_2(object sender, RoutedEventArgs e)
        {
            this.PopupAlchemySetKsnsT.IsOpen = true;
        }

        private void Button_Click_3(object sender, RoutedEventArgs e)
        {
            this.PopupAlchemySetKsnsT.IsOpen = false;
        }

        private void MenuItem_Click_3(object sender, RoutedEventArgs e)
        {
            this.PopupAbout.IsOpen = true;
        }

        
    }
}
