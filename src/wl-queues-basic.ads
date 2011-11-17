------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                      W L . Q U E U E S . B A S I C                       --
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

with Ada.Finalization;

generic
   type Element is private;
package WL.Queues.Basic is

   pragma Elaborate_Body;

   type Queue is private;

   function Empty (Q : Queue) return Boolean;

   procedure Add (Q : in out Queue; Elmt : in Element);

   procedure Add
     (Q        : in out Queue;
      Elmt     : in Element;
      Priority : in Natural);

   procedure Remove (Q : in out Queue; Result : out Element);

   type Array_Of_Elements is array (Positive range <>) of Element;
   function Get_Elements (Q : Queue) return Array_Of_Elements;

private
   type Queue_Element_Record;
   type Queue_Element is access Queue_Element_Record;

   type Queue is new Ada.Finalization.Controlled with
      record

         First, Last : Queue_Element;
      end record;

   procedure Initialize (Q : in out Queue);
   procedure Finalize (Q : in out Queue);
   procedure Adjust (Q : in out Queue);

end WL.Queues.Basic;
