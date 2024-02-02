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

with System;
with Interfaces;              use Interfaces;

with Formatted_Output.Utils;  use Formatted_Output.Utils;

package body Formatted_Output.Modular_Output is

   type Longest_Unsigned is mod System.Max_Binary_Modulus;
   
   --------------------
   -- Uns_xx_To_Text --
   --------------------
   
   procedure Uns_8_To_Text is new Uns_To_Text (Unsigned_8);
   procedure Uns_16_To_Text is new Uns_To_Text (Unsigned_16);
   procedure Uns_32_To_Text is new Uns_To_Text (Unsigned_32);
   procedure Uns_64_To_Text is new Uns_To_Text (Unsigned_64);
   procedure Uns_Max_To_Text is new Uns_To_Text (Longest_Unsigned);

   --------------------
   -- Format_Modular --
   --------------------
   
   function Format_Modular
     (Value : Item_Type; Fmt_Spec : Format_Data_Record) return String
   is
      Img : Unbounded_String;
   begin
      --  Converts Value to text and formats it
      if Value = 0 and then Fmt_Spec.Precision = 0 then
         return "";
      else
         if Item_Type'Size > 64 then
            Uns_Max_To_Text (Img, Longest_Unsigned (Value), Fmt_Spec.Base);
         elsif Item_Type'Size > 32 then
            Uns_64_To_Text (Img, Unsigned_64 (Value), Fmt_Spec.Base);
         elsif Item_Type'Size > 16 then
            Uns_32_To_Text (Img, Unsigned_32 (Value), Fmt_Spec.Base);
         elsif Item_Type'Size > 8 then
            Uns_16_To_Text (Img, Unsigned_16 (Value), Fmt_Spec.Base);
         else
            Uns_8_To_Text (Img, Unsigned_8 (Value), Fmt_Spec.Base);
         end if;
      
         return Format_Integer (Img, Fmt_Spec);
      end if;
   end Format_Modular;

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type is
      Fmt_Copy : Unbounded_String;
      Fmt_Spec : Format_Data_Record;
   begin
      Fmt_Copy := Unbounded_String (Fmt);

      Fmt_Spec.Value_Kind := V_Unsigned_Integer;
      Fmt_Spec.Align := Right;
      Fmt_Spec.Base_Style := None;
      Parse_Format (Fmt, Fmt_Spec);

      Replace_Slice
        (Fmt_Copy, Fmt_Spec.Spec_Start, Fmt_Spec.Spec_End,
         Format_Modular (Value, Fmt_Spec));
      return Format_Type (Fmt_Copy);
   end "&";

end Formatted_Output.Modular_Output;
