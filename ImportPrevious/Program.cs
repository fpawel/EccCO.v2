using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ECC.CO.Domain
{
    class Program
    {        
        static void Main(string[] args)
        {
            try
            {
                IEccRepository r = new EntityFrameworkEccRepository("ecc_repository");
                Console.WriteLine("{0} партий, {1} исполнений", r.Parties.Count(), r.Kinds.Count());

                foreach (var t in r.Kinds)
                {
                    Import.addProductType(t.Name, t.Gas, t.Units, t.Scale, t.NobleMetalContent, t.LifetimeWarrianty, t.Is64, t.TermoCalcWay,
                        t.Ifon_max, t.DeltaIfon_max, t.Ksns_min, t.Ksns_max, t.Delta_t_min, t.Delta_t_max, t.Ks40_min, t.Ks40_max, t.Delta_nei_max);
                }
                Import.saveProductTypes();

                foreach (var b in r.Parties)
                {
                    var eccs = new List<DataModel.Product>();
                    if (b.Eccs.Count == 64)
                    {
                        for (int n = 0; n < 64; ++n)
                        {
                            var e = b.Eccs[n];
                            var eccprodt = e.EccKindId.HasValue && e.EccKindId.Value < r.Kinds.Count() ? r.Kinds.ElementAt(e.EccKindId.Value).Name : "";
                            var ecc = Import.ecc(n, e.IsSelected, e.IsReportIncluded, eccprodt, e.EEPROM, e.Ifon, e.Isns, e.I13, e.I24, e.I35, e.I26, e.I17,
                                e.In, e.If_20, e.Is_20, e.If50, e.Is50, e.Serial);
                            eccs.Add(ecc);
                        }
                        Import.addBatch(b.Date.Value, eccs.ToArray(), b.Kind.Name, b.PGS1, b.PGS2, b.PGS3, b.Name);
                        Console.WriteLine("-> {0}, {1}", b.Date.Value, b.Name);
                    }
                    

                    
                }


                Console.WriteLine("ОК!");
            }
            catch (Exception e)
            {
                Console.Write(e);
            }
            
            Console.ReadKey();
        }
    }
}
