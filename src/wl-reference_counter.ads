with Ada.Finalization;

package WL.Reference_Counter is

   pragma Elaborate_Body;

   type Reference_Controlled is abstract tagged private;

   procedure Finalize_Reference (R : in out Reference_Controlled) is
   abstract;

private
   type Reference_Count is access Natural;

   type Reference_Controlled is
     abstract new Ada.Finalization.Controlled with
      record
         References : Reference_Count;
      end record;

   procedure Initialize (Ref : in out Reference_Controlled);
   procedure Adjust (Ref : in out Reference_Controlled);
   procedure Finalize (Ref : in out Reference_Controlled);

end WL.Reference_Counter;
