package WL.Localisation is

   function Local_Text
     (Tag : String;
      Arg_1 : String := "";
      Arg_2 : String := "";
      Arg_3 : String := "";
      Arg_4 : String := "")
      return String;

   function Local_Number
     (Value : Natural)
      return String;

   function Has_Local_Text
     (Tag          : String)
      return Boolean;

   procedure Read_Localisation
     (Path : String);

   type Language_Type is private;

   function To_Language
     (Code : String)
      return Language_Type;

   function Is_Valid
     (Language : Language_Type)
      return Boolean;

   function Local_Text
     (Language : Language_Type;
      Tag      : String;
      Arg_1    : String := "";
      Arg_2    : String := "";
      Arg_3    : String := "";
      Arg_4    : String := "")
      return String;

   function Has_Local_Text
     (Language     : Language_Type;
      Tag          : String)
      return Boolean;

   procedure Read_Localisation_File
     (Language : Language_Type;
      Path     : String);

   procedure Read_Localisation_Directory
     (Language : Language_Type;
      Path     : String);

   procedure Read_Language_Directories
     (Path     : String);
   --  for each directory contained in Path: treat the name of the
   --  directory as a language code, and call
   --  Read_Localisation_Directory (<directory name as language>,
   --                               Path/directory-name)

   type Localisation_Interface is limited interface;

   function Local_Text
     (Localisation : Localisation_Interface;
      Tag          : String;
      Arg_1        : String := "";
      Arg_2        : String := "";
      Arg_3        : String := "";
      Arg_4        : String := "")
      return String
      is abstract;

   function Has_Local_Text
     (Localisation : Localisation_Interface;
      Tag          : String)
      return Boolean
      is abstract;

   function Local_Number
     (Localisation : Localisation_Interface'Class;
      Value        : Natural)
      return String;

private

   type ISO_Language is new String (1 .. 2);
   type ISO_Country is new String (1 .. 2);

   type Language_Type is
      record
         Language : ISO_Language := "  ";
         Country  : ISO_Country  := "  ";
      end record;

   No_Language : constant Language_Type := ("  ", "  ");

   function Is_Valid
     (Language : Language_Type)
      return Boolean
   is (Language /= No_Language);

end WL.Localisation;
