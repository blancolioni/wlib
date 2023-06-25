--  This package puts a few useful properties around a string access
--  type.  I use it quite heavily when strings are computed at run
--  time, but are reasonably static thereafter.

with Ada.Streams;

package WL.Strings is

   pragma Preelaborate;

   type String_Access is access all String;
   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      S      : out String_Access);
   for String_Access'Read use Read;

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      S      : in String_Access);
   for String_Access'Write use Write;

   function "+" (S : String) return String_Access;
   function "-" (S : String_Access) return String;
   function "&" (S1 : String; S2 : String_Access) return String;
   function "&" (S1 : String_Access; S2 : String) return String;
   function "&" (S1 : String_Access; S2 : String_Access) return String;
   function "&" (S : String_Access; Ch : Character) return String;
   function "&" (Ch : Character; S : String_Access) return String;
   procedure Free (S : in out String_Access);

private
   pragma Inline ("&");
   pragma Inline ("+");
   pragma Inline ("-");

end WL.Strings;
