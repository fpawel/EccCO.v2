using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data.Entity;
using System.Data.Entity.Validation;
using System.ComponentModel;

namespace ECC.CO.Domain
{
    public class EntityFrameworkEccRepository : IEccRepository
    {
        private class EccDbContext : DbContext
        {
            public EccDbContext(string path)
                : base(path)
            {
                //base.Database.Log = NLog.LogManager.GetCurrentClassLogger().Info;
            }
            public DbSet<Kind> Kinds { get; set; }            
            public DbSet<Party> Parties { get; set; }
            public DbSet<Ecc> Eccs { get; set; }

            protected override void OnModelCreating(DbModelBuilder modelBuilder)
            {
                modelBuilder.Properties<decimal>().Configure(config => config.HasPrecision(20, 10));
                modelBuilder.Properties<string>().Configure(config => config.HasMaxLength(30));
                base.OnModelCreating(modelBuilder);
            }
            
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

        readonly EccDbContext _context;
        public EntityFrameworkEccRepository(string path)
        {
            _context = new EccDbContext(path);
        }

        IEnumerable<Party> IEccRepository.Parties 
        {
            get
            {
                var x = from party in _context.Parties select party;
                return x;
            }
        }

        void IEccRepository.Add(IEnumerable<Party> parties)
        {
            _context.Parties.AddRange(parties);

        }

        void IEccRepository.Remove(IEnumerable<Party> parties)
        {
            foreach (var party in parties)
                _context.Eccs.RemoveRange(party.Eccs);                
            
            _context.Parties.RemoveRange(parties);
        }
        
        IEnumerable<Kind> IEccRepository.Kinds 
        {
            get
            {
                var x = from kind in _context.Kinds select kind;
                return x;
            }
        }

        void IEccRepository.Add(IEnumerable<Kind> items)
        {
            _context.Kinds.AddRange(items);
        }
        void IEccRepository.Remove(Kind kind)
        {            
            _context.Kinds.Remove(kind);
            
            foreach (var party in _context.Parties.Local)
            {
                if (party.Kind == kind)
                    party.Kind = _context.Kinds.Local.Last();
            }

        }

        void IEccRepository.Add(IEnumerable<Ecc> items)
        {
            _context.Eccs.AddRange(items);
        }
        void IEccRepository.Remove(IEnumerable<Ecc> items)
        {
            _context.Eccs.RemoveRange(items);
        }

        

        void IEccRepository.SaveChanges()
        {
            try
            {
                _context.SaveChanges();
            }
            catch (DbEntityValidationException e)
            {
                var sb = new StringBuilder();
                foreach (var eve in e.EntityValidationErrors)
                {
                    sb.AppendFormat("Сущность типа \"{0}\" в состоянии \"{1}\" содержит следующие ошибки валидации:\r\n",
                        eve.Entry.Entity.GetType().Name, eve.Entry.State);
                    foreach (var ve in eve.ValidationErrors)
                    {
                        sb.AppendFormat("- Property: \"{0}\", Error: \"{1}\"\r\n",
                            ve.PropertyName, ve.ErrorMessage);
                    }                    
                    throw;
                }
            }
        }                
        
    }
}
