package WL.Localisation is

   function Local_Text
     (Tag : String;
      Arg_1 : String := "";
      Arg_2 : String := "";
      Arg_3 : String := "";
      Arg_4 : String := "")
      return String;

   procedure Read_Localisation
     (Path : String);

end WL.Localisation;
