------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2016-2024 Vitalii Bondarenko <vibondare@gmail.com>         --
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

with Formatted_Output.Utils;  use Formatted_Output.Utils;

package body Formatted_Output.Enumeration_Output is

   package Item_Type_IO is new Ada.Text_IO.Enumeration_IO (Item_Type);
   use Item_Type_IO;

   function Format_Enum
     (Value : Item_Type; Fmt_Spec : Format_Data_Record) return String;

   -----------------
   -- Format_Enum --
   -----------------

   function Format_Enum
     (Value : Item_Type; Fmt_Spec : Format_Data_Record) return String
   is
      Img : String := To_Lower (Item_Type'Image (Value));
      Ind : Natural;
   begin
      case Fmt_Spec.Letter_Case is
         when Capitalized =>
            Img (Img'First) := To_Upper (Img (Img'First));
         when Lower_Case  =>
            null;
         when Upper_Case  =>
            Img := To_Upper (Img);
         when Mixed       =>
            Img (Img'First) := To_Upper (Img (Img'First));
            Ind := Img'First + 1;

            while Ind < Img'Last loop
               if Img (Ind) = '_' then
                  Ind := Ind + 1;
                  Img (Ind) := To_Upper (Img (Ind));
               end if;

               Ind := Ind + 1;
            end loop;
      end case;

      declare
         S : String (1 .. Integer'Max (Fmt_Spec.Width, Img'Length));
      begin
         Move
           (Source  => Img,
            Target  => S,
            Drop    => Error,
            Justify => Fmt_Spec.Align,
            Pad     => Filler);
         return S;
      end;
   end Format_Enum;

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type is
      Fmt_Copy : Unbounded_String;
      Fmt_Spec : Format_Data_Record;
   begin
      Fmt_Copy := Unbounded_String (Fmt);

      Fmt_Spec.Align := Left;
      Parse_Format (Fmt, Fmt_Spec);

      Replace_Slice
        (Fmt_Copy, Fmt_Spec.Spec_Start, Fmt_Spec.Spec_End,
         Format_Enum (Value, Fmt_Spec));
      return Format_Type (Fmt_Copy);
   end "&";

end Formatted_Output.Enumeration_Output;
