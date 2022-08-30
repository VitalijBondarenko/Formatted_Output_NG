------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2016-2022 Vitalii Bondarenko <vibondare@gmail.com>         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- The MIT License (MIT)                                                    --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining a  --
-- copy of this software and associated documentation files (the            --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
------------------------------------------------------------------------------

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Formatted_Output.Modular_Output is

   package Item_Type_IO is new Ada.Text_IO.Modular_IO (Item_Type'Base);
   use Item_Type_IO;
 
   function Separate_Modular_Digit_Groups
     (Text_Value : String;
      Separator  : String;
      Group_Size : Integer) return String;
   
   function Format
     (Value         : Item_Type'Base;
      Initial_Width : Integer;
      Leading_Zero  : Boolean;
      Base          : Integer;
      Justification : Alignment;
      Base_Style    : Base_Style_Kind;
      Digit_Groups  : Digit_Grouping) return String;
   
   -----------------------------------
   -- Separate_Modular_Digit_Groups --
   -----------------------------------
   
   function Separate_Modular_Digit_Groups
     (Text_Value : String;
      Separator  : String;
      Group_Size : Integer) return String
   is
      FD : Natural := Index_Non_Blank (Text_Value, Forward);
      LD : Natural := Index_Non_Blank (Text_Value, Backward);
   begin
      if Separator'Length = 0 then
         return Text_Value;
      end if;
      
      return
        Text_Value (Text_Value'First .. FD - 1)
        & Separate_Digit_Groups (Text_Value (FD .. LD), Separator, Group_Size)
        & Text_Value (LD + 1 .. Text_Value'Last);
   end Separate_Modular_Digit_Groups;

   ------------
   -- Format --
   ------------

   function Format
     (Value         : Item_Type'Base;
      Initial_Width : Integer;
      Leading_Zero  : Boolean;
      Base          : Integer;
      Justification : Alignment;
      Base_Style    : Base_Style_Kind;
      Digit_Groups  : Digit_Grouping) return String
   is
      Img        : String (1 .. Maximal_Item_Length);
      Width      : Integer;
      Real_Width : Integer;
      Pre_First  : Natural;
      Last       : Natural;
   begin
      Put (Img, Value, Base);
      Last := Maximal_Item_Length;
      Pre_First := Last;

      if Base /= 10 then
         Last := Maximal_Item_Length - 1;
         Pre_First := Last;
         
         while Img (Pre_First) /= ' ' and then Img (Pre_First) /= '#' loop
            Pre_First := Pre_First - 1;
         end loop;
      else
         while Img (Pre_First) /= ' ' loop
            Pre_First := Pre_First - 1;
         end loop;
      end if;
      
      Real_Width := Last - Pre_First;
      Width := Integer'Max (Initial_Width, Real_Width);
      
      declare
         S : String (1 .. Width);
         V : String := Img (Pre_First + 1 .. Last);
         T : Unbounded_String;
         L : String :=
           (if Digit_Groups = NLS_Grouping_Style then Thousands_Sep_Character
            elsif Digit_Groups = Ada_Grouping_Style then Ada_Sep_Character
            else "");
         G : Integer :=
           (if Digit_Groups = NLS_Grouping_Style or Base = 10 then 3 else 4);
      begin
         case Digit_Groups is
            when None               =>
               T := To_Unbounded_String (V);
            when Ada_Grouping_Style =>
               T := To_Unbounded_String
                 (Separate_Modular_Digit_Groups (V, L, G));
            when NLS_Grouping_Style =>
               if Base = 10 then
                  T := To_Unbounded_String
                    (Separate_Modular_Digit_Groups (V, L, G));
               else
                  T := To_Unbounded_String (V);
               end if;
         end case;

         case Base_Style is
            when None           =>
               null;
            when C_Base_Style   =>
               case Base is
                  when 8      => T := "0" & T;
                  when 16     => T := "0x" & T;
                  when others => null;
               end case;
            when Ada_Base_Style =>
               case Base is
                  when 2      => T := "2#" & T & "#";
                  when 8      => T := "8#" & T & "#";
                  when 16     => T := "16#" & T & "#";
                  when others => null;
               end case;
         end case;

         if Length (T) > Width then
            return To_String (T);
         else
            Move
              (To_String (T),
               S,
               Justify => Justification,
               Pad     => Filler);
         
            if Leading_Zero then
               S := Set_Leading_Zero (S, L, G);
            end if;
         
            return S; 
         end if;
      end;
   end Format;

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : Item_Type'Base) return Format_Type
   is
      Command_Start         : constant Integer := Scan_To_Percent_Sign (Fmt);
      Leading_Zero          : Boolean := False;
      Width                 : Integer := 0;
      Digit_Occured         : Boolean := False;
      Justification_Changed : Boolean := False;
      Justification         : Alignment := Right;
      Base_Style            : Base_Style_Kind := None;
      Digit_Groups          : Digit_Grouping := None;
      Fmt_Copy              : Unbounded_String;
   begin
      if Command_Start /= 0 then
         Fmt_Copy := Unbounded_String (Fmt);
         
         for I in Command_Start + 1 .. Length (Fmt_Copy) loop
            case Element (Fmt_Copy, I) is
               when 'd'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format
                       (Value, Width, Leading_Zero, 10, Justification,
                        None, Digit_Groups));
                  return Format_Type (Fmt_Copy);
                  
               when 'x'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     To_Lower (
                       Format
                         (Value, Width, Leading_Zero, 16, Justification,
                          Base_Style, Digit_Groups)));
                  return Format_Type (Fmt_Copy);
                  
               when 'X'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     To_Upper (
                       Format
                         (Value, Width, Leading_Zero, 16, Justification,
                          Base_Style, Digit_Groups)));
                  return Format_Type (Fmt_Copy);
                  
               when 'o'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format
                       (Value, Width, Leading_Zero, 8, Justification,
                        Base_Style, Digit_Groups));
                  return Format_Type (Fmt_Copy);
                  
               when 'b'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format
                       (Value, Width, Leading_Zero, 2, Justification,
                        Base_Style, Digit_Groups));
                  return Format_Type (Fmt_Copy);
                  
               when '_'        =>
                  Digit_Groups := Ada_Grouping_Style;
                  
               when '''        =>
                  Digit_Groups := NLS_Grouping_Style;
                  
               when '#'        =>
                  Base_Style := C_Base_Style;
                  
               when '~'        =>
                  Base_Style := Ada_Base_Style;
                  
               when '-' | '*'  =>
                  if Justification_Changed or else Digit_Occured then
                     raise Format_Error;
                  end if;
                  
                  Justification_Changed := True;
                  
                  case Element (Fmt_Copy, I) is
                     when '-'    =>
                        Justification := Left;
                     when '*'    =>
                        Justification := Center;
                     when others =>
                        null;
                  end case;
                  
               when '0' .. '9' =>
                  Digit_Occured := True;
                  
                  if Width = 0 and then Element (Fmt_Copy, I) = '0' then
                     Leading_Zero := True;
                  else
                     Width := Width * 10
                       + Character'Pos (Element (Fmt_Copy, I))
                       - Character'Pos ('0');
                  end if;
                  
               when others     =>
                  raise Format_Error;
            end case;
         end loop;
      end if;
      
      raise Format_Error;
   end "&";

end Formatted_Output.Modular_Output;
