--  Bounded sets: sets with a finite domain.  Well, in Ada terms,
--  that's a discrete domain.

generic
   type Element is (<>);
package WL.Sets.Bounded is

   pragma Elaborate_Body;

   type Element_List is array (Positive range <>) of Element;
   type Set is private;
   function Empty return Set;
   function Is_Empty (S : Set) return Boolean;
   function "+" (E : Element) return Set;
   function "+" (E : Element_List) return Set;
   function "+" (E1, E2 : Element) return Set;
   function "+" (E : Element; S : Set) return Set;
   function "+" (S : Set; E : Element) return Set;
   function "+" (S1, S2 : Set) return Set;
   function "-" (S1, S2 : Set) return Set;
   function "-" (S : Set; E : Element) return Set;
   function "<=" (E : Element; S : Set) return Boolean;

   procedure Take (S : in out Set; E : out Element);
   procedure Add (S : in out Set; E : Element);
   procedure Add (S1 : in out Set; S2 : Set);
   procedure Remove (S : in out Set; E : Element);
   procedure Remove (S1 : in out Set; S2 : Set);

   function Image (S : Set) return String;

private type Set is array (Element) of Boolean;
   for Set'Component_Size use 1;

end WL.Sets.Bounded;
