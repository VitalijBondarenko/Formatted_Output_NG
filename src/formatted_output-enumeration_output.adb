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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body Formatted_Output.Enumeration_Output is

   package Item_Type_IO is new Ada.Text_IO.Enumeration_IO (Item_Type);
   use Item_Type_IO;

   type Style_Type is
     (Style_Capitalized, Style_Lower_Case, Style_Upper_Case, Style_Mixed);

   function Format
     (Value         : Item_Type;
      Initial_Width : Integer;
      Justification : Alignment;
      Style         : Style_Type) return String;

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
      Past_Last  : Integer := Img'First;
      Ind        : Natural;
   begin
      Put (Img, Value, Lower_Case);
      Past_Last := Index_Non_Blank (Img, Backward);

      case Style is
         when Style_Capitalized =>
            Img (Img'First) := To_Upper (Img (Img'First));
         when Style_Lower_Case  =>
            null;
         when Style_Upper_Case  =>
            Img := To_Upper (Img);
         when Style_Mixed       =>
            Img (Img'First) := To_Upper (Img (Img'First));
            Ind := Img'First + 1;

            while Ind < Past_Last loop
               if Img (Ind) = '_' then
                  Ind := Ind + 1;
                  Img (Ind) := To_Upper (Img (Ind));
               end if;

               Ind := Ind + 1;
            end loop;
      end case;

      Real_Width := Past_Last;
      Width := Integer'Max (Initial_Width, Real_Width);

      declare
         S : String (1 .. Width);
      begin
         Move
           (Source  => Img (Past_Last - Real_Width + 1 .. Past_Last),
            Target  => S,
            Drop    => Error,
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
                     Format (Value, Width, Justification, Style_Capitalized));
                  return Format_Type (Fmt_Copy);

               when 'u'             =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Style_Upper_Case));
                  return Format_Type (Fmt_Copy);

               when 'l'             =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Style_Lower_Case));
                  return Format_Type (Fmt_Copy);

               when 'm'             =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Style_Mixed));
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
