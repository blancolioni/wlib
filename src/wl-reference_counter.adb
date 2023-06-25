with Ada.Unchecked_Deallocation;

package body WL.Reference_Counter is

   use Ada.Finalization;

   procedure Free is
      new Ada.Unchecked_Deallocation
     (Object => Natural, Name => Reference_Count);

   procedure Initialize (Ref : in out Reference_Controlled) is
   begin
      Ref.References := new Natural'(1);
   end Initialize;

   procedure Adjust (Ref : in out Reference_Controlled) is
   begin
      Ref.References.all := Ref.References.all + 1;
   end Adjust;

   procedure Finalize (Ref : in out Reference_Controlled) is
   begin
      if Ref.References = null then
         --  this shouldn't happen!
         null;
      else
         Ref.References.all := Ref.References.all - 1;
         if Ref.References.all = 0 then
            Free (Ref.References);
            Finalize_Reference (Reference_Controlled'Class (Ref));
         end if;
      end if;

   end Finalize;

end WL.Reference_Counter;
