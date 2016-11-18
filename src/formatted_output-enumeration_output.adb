------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2016 Vitalij Bondarenko <vibondare@gmail.com>              --
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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body Formatted_Output.Enumeration_Output is

   package Item_Type_IO is new Enumeration_IO (Item_Type);
   use Item_Type_IO;

   type Style_Type is (Capitalized, Upper_Case, Lower_Case);

   ------------
   -- Format --
   ------------

   function Format
     (Value         : Item_Type;
      Initial_Width : Integer;
      Justification : Alignment;
      Style         : Style_Type) return String
   is
      Img        : String (1 .. Maximal_Item_Length);
      Width      : Integer;
      Real_Width : Integer;
      Past_Last  : Integer := 1;

   begin
      case Style is
         when Capitalized =>
            Put (Img, Value, Set => Type_Set'(Lower_Case));
            Img (1) := To_Upper (Img (1));
         when Lower_Case  =>
            Put (Img, Value, Set => Type_Set'(Lower_Case));
         when Upper_Case  =>
            Put (Img, Value, Set => Type_Set'(Upper_Case));
      end case;

      while Img (Past_Last) /= ' ' loop
         Past_Last := Past_Last + 1;
      end loop;

      Real_Width := Past_Last - 1;

      if Initial_Width < Real_Width then
         Width := Real_Width;
      else
         Width := Initial_Width;
      end if;

      declare
         S : String (1 .. Width);
      begin
         Move
           (Img (Past_Last - Real_Width .. Past_Last - 1),
            S,
            Justify => Justification,
            Pad     => Filler);
         return S;
      end;
   end Format;

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type is
      Command_Start         : constant Integer := Scan_To_Percent_Sign (Fmt);
      Width                 : Integer := 0;
      Digit_Occured         : Boolean := False;
      Justification_Changed : Boolean := False;
      Justification         : Alignment := Right;
      Fmt_Copy              : Unbounded_String;

   begin
      if Command_Start /= 0 then
         Fmt_Copy := Unbounded_String (Fmt);

         for I in Command_Start + 1 .. Length (Fmt_Copy) loop
            case Element (Fmt_Copy, I) is
               when 'c'             =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Capitalized));
                  return Format_Type (Fmt_Copy);

               when 'u'             =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Upper_Case));
                  return Format_Type (Fmt_Copy);

               when 'l'             =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Lower_Case));
                  return Format_Type (Fmt_Copy);

               when '-' | '+' | '*' =>
                  if Justification_Changed or else Digit_Occured then
                     raise Format_Error;
                  end if;

                  Justification_Changed := True;

                  case Element (Fmt_Copy, I) is
                     when '-'    => Justification := Left;
                     when '+'    => Justification := Right;
                     when '*'    => Justification := Center;
                     when others => null;
                  end case;

               when '0' .. '9'      =>
                  Width := Width * 10
                    + Character'Pos (Element (Fmt_Copy, I))
                    - Character'Pos ('0');

               when others          =>
                  raise Format_Error;
            end case;
         end loop;
      end if;

      raise Format_Error;
   end "&";

end Formatted_Output.Enumeration_Output;
