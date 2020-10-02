concrete FoodsKlingon of Foods = open Prelude in {
  lincat
    Comment = SS ;
    Item = LinItem; -- {s : Str; n : Number} ;
    Kind = LinKind; -- {s : Str};
    Quality = LinQuality ;

  lin
    -- : Item -> Quality -> Comment ;
    Pred i q = 
      ss (i.s ++ copula ! i.n ++ q.s);
    -- : Kind -> Item ;
    This = det Sg "ghu'vam" ;
    That = det Sg "net" ;
    These = det Pl "qetlh" ;
    Those = det Pl "nuv" ;

    -- : Quality -> Kind -> Kind ;
    Mod q k = {
      s = table {
        num => k.s ! num ++ q.s }
    } ; 

    Very q = {
       s = "'el" ++ q.s;
    };

    Pizza = mkKind "pltSa' chab" ;
    Cheese = mkKind "nlm wlb ngogh" ;
    Wine = mkKind "hlq" ;
    Fish = mkKind "ghotl'" ;

    Fresh = mkQuality "ghoQ" ;
    Warm = mkQuality "veS" ;
    Italian = mkQuality "tlhIngan" ;
    Expensive = mkQuality "wagh" ;
    Boring = mkQuality "Dal" ;
    Delicious = mkQuality "Soj'eyqu'" ;
    Good = mkQuality "maj" ;

  param
    Number = Sg | Pl;

  oper
    copula : Number => Str ;
    copula = table {
      Sg => "ghaH" ;
      Pl => "maghaH"
    } ;

    LinItem : Type = {s : Str; n : Number};
    -- determiner

    det     :  Number -> Str -> LinKind -> LinItem =
    \n,d,kind -> {
        s = d ++ kind.s ! n ;
        n = n;
    };
    LinKind : Type = {s : Number => Str};
    mkKind : Str -> LinKind ;
    mkKind str = {
      s = table {
        Sg => str ;
        Pl => str + "mey"
      }
    } ;
   
    LinQuality : Type = {s: Str} ;
    mkQuality : Str -> LinQuality ;
    mkQuality str = {s = str} ;
}