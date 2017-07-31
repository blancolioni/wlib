package WL.Command_Line is

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
      Default    : Integer)
     return Integer;

end WL.Command_Line;
