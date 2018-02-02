using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ECC.CO.Domain
{    
    public interface IEccRepository
    {
        IEnumerable<Party> Parties { get; }
        void Add( IEnumerable<Party> parties);
        void Remove(IEnumerable<Party> parties);

        IEnumerable<Kind> Kinds { get; }
        void Add(IEnumerable<Kind> items);
        void Remove(Kind item);
                
        void Add(IEnumerable<Ecc> items);
        void Remove(IEnumerable<Ecc> items);
                        
        
        void SaveChanges();
    }
}
