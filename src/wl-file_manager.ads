package WL.File_Manager is

   procedure Add_Include_Path (Class : String; Path : String);

   function Find_File (Class : String; File_Name : String) return String;

end WL.File_Manager;
