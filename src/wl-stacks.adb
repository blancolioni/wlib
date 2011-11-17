------------------------------------------------------------------------------
--                                                                          --
--                         White Lion Ada Library                           --
--                                                                          --
--                            W L . S T A C K S                             --
--                                                                          --
--                                 B o d y                                  --
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

package body WL.Stacks is

   -----------
   -- Clear --
   -----------

   procedure Clear (S : in out Stack) is
   begin
      S.Top := 0;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Stack) return Boolean is
   begin
      return S.Top = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (S : Stack) return Boolean is
      pragma Unreferenced (S);
   begin
      return False;
   end Is_Full;

   ---------
   -- Pop --
   ---------

   procedure Pop (From : in out Stack; Item : out Stack_Element) is
   begin
      if From.Top = 0 then
         raise Stack_Error;
      end if;
      Item := Array_Of_Stack_Elements.Get (From.Elements, From.Top);
      From.Top := From.Top - 1;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (To : in out Stack; Item : in Stack_Element) is
   begin
      To.Top := To.Top + 1;
      Array_Of_Stack_Elements.Set (To.Elements, To.Top, Item);
   end Push;

   ----------
   -- Size --
   ----------

   function Size (S : in Stack) return Natural is
   begin
      return S.Top;
   end Size;

   ---------
   -- Top --
   ---------

   function Top (Of_Stack : Stack) return Stack_Element is
   begin
      if Of_Stack.Top = 0 then
         raise Stack_Error;
      end if;

      return Array_Of_Stack_Elements.Get
        (Of_Stack.Elements, Of_Stack.Top);
   end Top;

end WL.Stacks;
