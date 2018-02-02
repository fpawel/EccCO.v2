using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.DataAnnotations;

namespace ECC.CO.Domain
{

    public class Kind
    {
        public int KindId { get; set; }

        [MaxLength(50)]
        public string Name { get; set; }
        [MaxLength(30)]
        public string Gas { get; set; }
        [MaxLength(30)]
        public string Units { get; set; }
        public decimal NobleMetalContent { get; set; }
        public int LifetimeWarrianty { get; set; }
        public bool Is64 { get; set; }
        public int TermoCalcWay { get; set; }
        public decimal Scale { get; set; }

        public decimal? DeltaIfon_max { get; set; }

        public decimal? Delta_nei_max { get; set; }
        
        public decimal? Delta_t_max { get; set; }
        public decimal? Delta_t_min { get; set; }

        public decimal? Ifon_max { get; set; }

        public decimal? Ks40_max { get; set; }
        public decimal? Ks40_min { get; set; }

        public decimal? Ksns_max { get; set; }
        public decimal? Ksns_min { get; set; }

        public virtual List<Party> Parties { get; set; }
    }

    public class Party
    {
        public int PartyId { get; set; }
        public DateTime? Date { get; set; }

        public int KindId { get; set; }
        public virtual Kind Kind { get; set; }

        [MaxLength(30)]
        public string Name { get; set; }

        public decimal PGS1 { get; set; }
        public decimal PGS2 { get; set; }
        public decimal PGS3 { get; set; }
        
        public int? TermoCalcWay { get; set; }
        public virtual List<Ecc> Eccs { get; set; }
    }

    public class Ecc
    {

        public int EccId { get; set; }

        public int PartyId { get; set; }
        public virtual Party Party { get; set; }

        public bool IsSelected { get; set; }
        public bool IsReportIncluded { get; set; }


        public byte[] EEPROM { get; set; }

        public int? EccKindId { get; set; }

        public decimal? Ifon { get; set; }
        public decimal? Isns { get; set; }
        public decimal? I13 { get; set; }
        public decimal? I24 { get; set; }
        public decimal? I35 { get; set; }
        public decimal? I26 { get; set; }
        public decimal? I17 { get; set; }
        public decimal? In { get; set; }
        public decimal? If_20 { get; set; }
        public decimal? Is_20 { get; set; }
        public decimal? If50 { get; set; }
        public decimal? Is50 { get; set; }

        public int? Serial { get; set; }
    }
}
