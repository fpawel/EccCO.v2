using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.ComponentModel;

namespace UI
{
    /// <summary>
    /// Логика взаимодействия для CommSetsView.xaml
    /// </summary>
    public partial class CommSetsView : UserControl, INotifyPropertyChanged
    {

        public CommSetsView()
        {
            InitializeComponent();
        }

        #region INotifyPropertyChanged

        public event PropertyChangedEventHandler PropertyChanged;

        private void RaisePropertyChanged(string propertyName)
        {
            if (PropertyChanged != null)
            {
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
            }
        }

        #endregion

        string _title = "";
        
        bool _isAddyTextBoxVisible = false;
        bool _isSelectPortNamecomboBoxVisible = true;
        int _titleWidth = 100;

        public static readonly DependencyProperty TitleProperty =
            DependencyProperty.Register("Title", typeof(string), typeof(CommSetsView),
                new UIPropertyMetadata("", new PropertyChangedCallback(OnValueChanged)));

        public static readonly DependencyProperty IsAddyTextBoxVisibleProperty =
            DependencyProperty.Register("IsAddyTextBoxVisible", typeof(bool), typeof(CommSetsView),
                new UIPropertyMetadata(false, new PropertyChangedCallback(OnValueChanged)));

        public static readonly DependencyProperty IsSelectPortNamecomboBoxVisibleProperty =
            DependencyProperty.Register("IsSelectPortNamecomboBoxVisible", typeof(bool), typeof(CommSetsView),
                new UIPropertyMetadata(true, new PropertyChangedCallback(OnValueChanged)));

        public static readonly DependencyProperty TitleWidthProperty =
            DependencyProperty.Register("TitleWidth", typeof(int), typeof(CommSetsView),
                new UIPropertyMetadata(100, new PropertyChangedCallback(OnValueChanged)));



        private static void OnValueChanged(DependencyObject obj, DependencyPropertyChangedEventArgs e)
        {
            var control = obj as CommSetsView;
            if( e.Property==TitleProperty) 
            {
                var value = (string)e.NewValue;
                control.Title = value;
            }
            else if (e.Property == IsAddyTextBoxVisibleProperty)
            {
                var value = (bool)e.NewValue;
                control.IsAddyTextBoxVisible = value;
            }
            else if (e.Property == IsSelectPortNamecomboBoxVisibleProperty)
            {
                var value = (bool)e.NewValue;
                control.IsSelectPortNamecomboBoxVisible = value;
            }
            else if (e.Property == TitleWidthProperty)
            {
                var value = (int)e.NewValue;
                control.TitleWidth = value;
            }
        }

        public string Title
        {
            get
            {
                return _title;
            }
            set 
            {
                if (value != _title) 
                {
                    _title = value;
                    RaisePropertyChanged("Title");
                }
            }
        }

        public int TitleWidth
        {
            get
            {
                return _titleWidth;
            }
            set
            {
                if (value != _titleWidth)
                {
                    _titleWidth = value;
                    RaisePropertyChanged("TitleWidth");
                }
            }
        }


        public bool IsSelectPortNamecomboBoxVisible
        {
            get
            {
                return _isSelectPortNamecomboBoxVisible;
            }
            set
            {
                if (value != _isSelectPortNamecomboBoxVisible)
                {
                    _isSelectPortNamecomboBoxVisible = value;
                    RaisePropertyChanged("IsSelectPortNamecomboBoxVisible");
                }
            }
        }


        public bool IsAddyTextBoxVisible
        {
            get
            {
                return _isAddyTextBoxVisible;
            }
            set
            {
                if (value != _isAddyTextBoxVisible)
                {
                    _isAddyTextBoxVisible = value;
                    RaisePropertyChanged("IsAddyTextBoxVisible");
                }
            }
        }

        public IEnumerable<string> SerialPortsNames
        {
            get
            {
                var result = new List<string>(from x in System.IO.Ports.SerialPort.GetPortNames() select x);
                result.Add("");
                return result;
            }
        }

        private void ComboBox_DropDownOpened(object sender, EventArgs e)
        {
            RaisePropertyChanged("SerialPortsNames");
        }


        private void TextBox_MouseWheel(object sender, MouseWheelEventArgs e)
        {
            var textbox = sender as TextBox;
            int v = 0;
            if (Int32.TryParse(textbox.Text, out v))
            {   
                v += e.Delta > 0 ? 1 : -1;
                textbox.Text = v.ToString();
            }
        }

        private void Button_Click_29(object sender, RoutedEventArgs e)
        {
            PopupSettings.PlacementTarget = sender as UIElement;
            PopupSettings.IsOpen = true;
        }
    }
}
