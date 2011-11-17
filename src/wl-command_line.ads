package WL.Command_Line is

   type Root_Argument_Handler is abstract tagged private;

   procedure Handle (Handler : in out Root_Argument_Handler;
                     Value   : in     String)
      is abstract;

   function Has_Argument (Handler : Root_Argument_Handler) return Boolean;

   function Exit_After (Handler : Root_Argument_Handler) return Boolean;

   procedure Register (Long_Name  : String;
                       Short_Name : Character;
                       Handler    : Root_Argument_Handler'Class);

   procedure Register (Long_Name  : String;
                       Handler    : Root_Argument_Handler'Class);

   procedure Register (Short_Name : Character;
                       Handler    : Root_Argument_Handler'Class);

   procedure Process_Command_Line;
   procedure Show_Usage;

   function Argument_Count return Natural;
   function Argument (Index : Positive) return String;

   type Flag_Argument_Handler is
     abstract new Root_Argument_Handler with private;

   procedure Handle (Handler : in out Flag_Argument_Handler;
                     Value   : in     String);

   procedure Handle_Set (Handler : in out Flag_Argument_Handler) is abstract;
   procedure Handle_Clear (Handler : in out Flag_Argument_Handler) is null;

   type Dispatch_Handler is abstract new Root_Argument_Handler with private;

   procedure Execute (Handler : Dispatch_Handler)
      is abstract;

   procedure Handle (Handler : in out Dispatch_Handler;
                     Value   : in     String);

private

   type Root_Argument_Handler is abstract tagged null record;

   type Flag_Argument_Handler is abstract new Root_Argument_Handler
     with null record;

   type Dispatch_Handler is
     abstract new Root_Argument_Handler with null record;

end WL.Command_Line;
