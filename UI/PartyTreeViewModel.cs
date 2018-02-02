using System;
using System.Collections.ObjectModel;
using System.Windows.Input;
namespace UI
{
    [PropertyChanged.ImplementPropertyChanged]
    public class PartyTreeViewModel
    {
        public string Name { get; set; }
        public string What { get; set; }
        public int Level { get; set; }
        public string ProductType { get; set; }
        public ObservableCollection<PartyTreeViewModel> Items { get; set; }
        public bool HasSelectedParty { get; set; }
        public bool IsSelected { get; set; }
        public bool IsExpanded { get; set; }
        public bool IsBatchInfoItem { get; set; }
        public bool IsCatalogueItem { get; set; }        
        public ICommand Select { get; set; }
        public ICommand Delete { get; set; }

        public bool IsVisible { get; set; }

    }
}
