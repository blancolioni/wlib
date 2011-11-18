package body WL.Text is

   function To_Key (X : String) return Replacement_Key;

   ---------
   -- Add --
   ---------

   procedure Add
     (To        : in out Text_Replacement;
      Old_Value : in     String;
      New_Value : in     String)
   is
      Key : constant Replacement_Key := To_Key (Old_Value);
   begin
      if To.Map.Contains (Key) then
         To.Map.Delete (Key);
      end if;
      To.Map.Insert (Key, New_Value);
   end Add;

   -------------
   -- Replace --
   -------------

   function Replace
     (Text     : String;
      Using    : Text_Replacement)
      return String
   is
      Index : Positive := Text'First;
   begin
      while Index < Text'Last loop
         if Text (Index) = '{' then
            declare
               Start : constant Positive := Index + 1;
               Finish : Positive := Start;
            begin
               while Finish <= Text'Last
                 and then Text (Finish) /= '}'
               loop
                  Finish := Finish + 1;
               end loop;

               if Finish <= Text'Last
                 and then Text (Finish) = '}'
               then
                  Index := Finish;
                  Finish := Finish - 1;
                  declare
                     Key : constant Replacement_Key :=
                             To_Key (Text (Start .. Finish));
                  begin
                     if Using.Map.Contains (Key) then
                        return Text (Text'First .. Start - 2)
                          & Using.Map.Element (Key)
                          & Replace (Text (Index + 1 .. Text'Last), Using);
                     end if;
                  end;
               else
                  return Text;
               end if;
            end;
         end if;
         Index := Index + 1;
      end loop;
      return Text;
   end Replace;

   ------------
   -- To_Key --
   ------------

   function To_Key (X : String) return Replacement_Key is
   begin
      return Result : Replacement_Key do
         Result := (others => ' ');
         Result (1 .. Natural'Min (X'Length, Result'Length)) :=
           X (X'First .. X'First + Natural'Min (X'Length, Result'Length) - 1);
      end return;
   end To_Key;

end WL.Text;
