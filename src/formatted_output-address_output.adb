------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2023-2024 Vitalii Bondarenko <vibondare@gmail.com>         --
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

with System.Address_Image;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Formatted_Output.Utils;  use Formatted_Output.Utils;

package body Formatted_Output.Address_Output is

   function Format_Address
     (Value : System.Address; Fmt_Spec : Format_Data_Record) return String;

   --------------------
   -- Format_Address --
   --------------------

   function Format_Address
     (Value : System.Address; Fmt_Spec : Format_Data_Record) return String
   is
      Min_Dig_Num : Integer := 0;
      Img         : Unbounded_String :=
        To_Unbounded_String (System.Address_Image (Value));
   begin
      --  Get a minimum number of digits
      if Fmt_Spec.Precision /= Undefined then
         Min_Dig_Num := Fmt_Spec.Precision;
      end if;

      --  Pad value to minimum number of digits
      if Min_Dig_Num > Length (Img) then
         Insert (Img, 1, String'((Min_Dig_Num - Length (Img)) * '0'));
      end if;

      --  Separate groups of digits
      case Fmt_Spec.Digit_Groups is
         when None =>
            null;
         when Ada_Grouping_Style =>
            Separate_Digit_Groups (Img, Fmt_Spec);
         when NLS_Grouping_Style =>
            null;
      end case;

      --  Put base
      case Fmt_Spec.Base_Style is
         when None           =>
            null;
         when C_Base_Style   =>
            Insert (Img, 1, "0X");
         when Ada_Base_Style =>
            Insert (Img, 1, "16#");
            Append (Img, "#");
      end case;

      --  Put leading zeroes
      if Fmt_Spec.Zero_Padded then
         Set_Zero_Padded (Img, Fmt_Spec.Width - Length (Img), Fmt_Spec);
      end if;

      --  Build a result string
      if Length (Img) >= Fmt_Spec.Width then
         return To_String (Img);
      else
         declare
            Buffer : String (1 .. Fmt_Spec.Width) := (others => ' ');
         begin
            Move
              (Source  => To_String (Img),
               Target  => Buffer,
               Drop    => Error,
               Justify => Fmt_Spec.Align,
               Pad     => Filler);
            return Buffer;
         end;
      end if;
   end Format_Address;

   ---------
   -- "&" --
   ---------

   function "&"
     (Fmt : Format_Type; Value : System.Address) return Format_Type
   is
      Fmt_Copy : Unbounded_String;
      Fmt_Spec : Format_Data_Record;
   begin
      Fmt_Copy := Unbounded_String (Fmt);

      Fmt_Spec.Value_Kind := V_Pointer;
      Fmt_Spec.Align := Right;
      Fmt_Spec.Base := 16;
      Fmt_Spec.Base_Style := None;
      Fmt_Spec.Notation := Decimal_Notation;
      Parse_Format (Fmt, Fmt_Spec);

      Replace_Slice
        (Fmt_Copy, Fmt_Spec.Spec_Start, Fmt_Spec.Spec_End,
         Format_Address (Value, Fmt_Spec));
      return Format_Type (Fmt_Copy);
   end "&";


end Formatted_Output.Address_Output;
