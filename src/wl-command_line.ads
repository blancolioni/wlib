package WL.Command_Line is

   procedure Load_Defaults
     (File_Path : String);

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character)
     return Boolean;

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character)
     return String;

   function Find_Option
     (Long_Name  : String;
      Short_Name : Character;
      Default    : Integer    := 0)
     return Integer;

end WL.Command_Line;
