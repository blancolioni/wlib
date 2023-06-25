with Unchecked_Deallocation;

package body WL.Strings is

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      S      : out String_Access)
   is
      Str : constant String := String'Input (Stream);
   begin
      S := new String'(Str);
   end Read;

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      S      : in String_Access)
   is
   begin
      String'Output (Stream, S.all);
   end Write;

   function "+" (S : String) return String_Access is
   begin
      return new String'(S);
   end "+";

   function "-" (S : String_Access) return String is
   begin
      return S.all;
   end "-";

   function "&" (S1 : String; S2 : String_Access) return String is
   begin
      return S1 & S2.all;
   end "&";

   function "&" (S1 : String_Access; S2 : String) return String is
   begin
      return S1.all & S2;
   end "&";

   function "&" (S1 : String_Access; S2 : String_Access) return String is
   begin
      return S1.all & S2.all;
   end "&";

   function "&" (S : String_Access; Ch : Character) return String is
   begin
      return S.all & Ch;
   end "&";

   function "&" (Ch : Character; S : String_Access) return String is
   begin
      return Ch & S.all;
   end "&";


   procedure Free_String is
      new Unchecked_Deallocation (String, String_Access);
   procedure Free (S : in out String_Access) is
   begin
      Free_String (S);
   end Free;

end WL.Strings;
