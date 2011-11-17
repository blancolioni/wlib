------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                             W L . T R A C E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (c) 2004 Fraser Wilson                      --
--                                                                          --
-- WLib is free software; you can redistribute it and/or  modify  it  under --
-- terms  of  the  GNU  General  Public  License  as  published by the Free --
-- Software Foundation; either version 2, or (at  your  option)  any  later --
-- version.  WLib  is  distributed  in the hope that it will be useful, but --
-- WITHOUTANY WARRANTY; without even the implied warranty of  MERCHANTABIL- --
-- ITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details. You should have received a  copy  of  the  GNU --
-- General  Public License distributed with WLib; see file COPYING. If not, --
-- write to the Free Software Foundation, 59  Temple  Place  -  Suite  330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

package WL.Trace is
   pragma Elaborate_Body;

   type Trace_Level is new Natural;

   Default_Level : constant Trace_Level := 3;

   procedure Set_Default_Level
     (To_Level : Trace_Level := Default_Level);

   procedure Start_Trace
     (Level     : Trace_Level;
      File_Name : String := "trace.txt");

   procedure Put (Message : String);

   procedure Put (Level : in Trace_Level; Message : in String);

   procedure Put_Line (Message : String);

   procedure Put_Line (Level : in Trace_Level; Message : in String);

   procedure New_Line;
   procedure New_Line (Level : in Trace_Level);
   procedure New_Line (Level : in Trace_Level; Count : in Natural);

   procedure Set_Col (Col : in Positive);
   procedure Set_Col (Level : in Trace_Level; Col : in Positive);

   function Col return Positive;

   procedure End_Trace;

end WL.Trace;
