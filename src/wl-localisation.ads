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

   procedure Read_Localisation
     (Language : out Language_Type;
      Path     : String);

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

   type Language_Type is new String (1 .. 5);

   function Local_Text
     (Language : Language_Type;
      Tag      : String;
      Arg_1    : String := "";
      Arg_2    : String := "";
      Arg_3    : String := "";
      Arg_4    : String := "")
      return String
   is (Local_Text (Tag, Arg_1, Arg_2, Arg_3, Arg_4));

   function Has_Local_Text
     (Language     : Language_Type;
      Tag          : String)
      return Boolean
   is (Has_Local_Text (Tag));

end WL.Localisation;
