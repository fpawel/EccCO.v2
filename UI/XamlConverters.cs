using System;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Media;

namespace UI
{
    public class ShowIfNotNullConverter : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return value != null ? Visibility.Visible : Visibility.Collapsed;
        }
        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }

    public class ShowIfGreateThenNullConverter : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return (int)value >0 ? Visibility.Visible : Visibility.Collapsed;
        }
        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }

    public class ShowIfFalseConverter : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return (bool)value ? Visibility.Collapsed : Visibility.Visible;
        }
        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }

    public class Col1WidthConverter : IValueConverter
    {
        public object Convert(object x, Type type, object parameter, CultureInfo culture)
        {
            var y = (double)x;
            return y>24 ? y - 24 : 0;
        }

        public object ConvertBack(object o, Type type, object parameter,
                                  CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }

    public class BoolNotConverter : IValueConverter
    {
        public object Convert(object x, Type type, object parameter, CultureInfo culture)
        {
            return ! (bool) x ;
        }

        public object ConvertBack(object x, Type type, object parameter, CultureInfo culture)
        {
            return ! (bool)x;
        }
    }


    public class Col1WidthConverter2 : IMultiValueConverter
    {
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
        {
            return (double)values[0] - (int)values[1] * 13.2;
        }

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }

    public class MainWindowTitlePartyInfoConverter : IMultiValueConverter
    {
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
        {
            var prodt = (string)values[0];
            var name = (string)values[1];
            var date = (DateTime)values[2];
            return string.Format("Партия {0}, {1}, \"{2}\"", prodt, date.ToString("MM/dd/yyyy HH:mm"), name);
        }

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }

    public class DataGridcolumnVisibilityConverter : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is Visibility && (Visibility)value == Visibility.Visible)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is bool && (bool)value)
            {
                return Visibility.Visible;
            }
            else
            {
                return Visibility.Collapsed;
            }
        }
    }

    public class DebugConverter : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var t = value.GetType();
            var props = t.GetProperties();
            foreach (var p in props)
            {
                Console.WriteLine(p);
            }
            return value;
        }

        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return value;
        }
    }

    public class GridColumnDefinitionWidthConverter1 : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return (bool)value ? new GridLength(5, GridUnitType.Star) : new GridLength(0);
        }

        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }

    public class ForegroundConverter1 : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return new SolidColorBrush((bool)value ? (Color)ColorConverter.ConvertFromString("#FF569CD6") : Colors.Yellow);
        }

        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }
    public class ForegroundConverter2 : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return new SolidColorBrush((bool)value ? (Color)ColorConverter.ConvertFromString("#FF84BB96") : Colors.Yellow);
        }

        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }


    public class ReportForegroundConverter1 : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return new SolidColorBrush((bool)value ? Colors.Black : Colors.White);
        }

        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }
    public class ReportBackgroundConverter2 : IValueConverter
    {
        public virtual object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return (bool)value ? null : new SolidColorBrush(Colors.DarkGray);
        }

        public virtual object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }

    
}