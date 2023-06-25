with Ada.Finalization;

--  Unbounded sets: sets with a non-discrete domain.  E.g., sets of
--  access types.  Obviously not as fast as the bounded sets version.

generic
   type Element is private;
package WL.Sets.Unbounded is

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

   function Elements (S : Set) return Natural;
   function To_List (S : Set) return Element_List;

   --  Contents requires that the set have exactly one
   --  element, otherwise Set_Error is raised
   function Contents (S : Set) return Element;

   procedure Take (S : in out Set; E : out Element);
   procedure Add (S : in out Set; E : Element);
   procedure Add (S1 : in out Set; S2 : Set);
   procedure Remove (S : in out Set; E : Element);
   procedure Remove (S1 : in out Set; S2 : Set);

   --  function Image (S : Set) return String;

   type Compare_Function is
     access function (Left, Right : Element) return Boolean;

   --  function Extract
   --  returns a set which contains only those elements X such
   --  that X <= S and Condition (Left, X) is true.
   function Extract
     (S         : Set;
      Left      : Element;
      Condition : Compare_Function)
   return Set;

   --  procedure Extract
   --  procedural version of above.  Less abstract, more efficient.
   procedure Extract
     (S         : in out Set;
      Left      : in Element;
      Condition : in Compare_Function);


private type Set_List is access Element_List;
   type Set is new Ada.Finalization.Controlled with
      record
         List  : Set_List;
         Count : Natural;
      end record;

   procedure Initialize (Object : in out Set);
   procedure Adjust (Object : in out Set);
   procedure Finalize (Object : in out Set);

end WL.Sets.Unbounded;
