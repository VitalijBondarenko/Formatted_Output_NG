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
with Interfaces;             use Interfaces;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

with Formatted_Output.Utils; use Formatted_Output.Utils;

package body Formatted_Output.Integer_Output is

   type Longest_Integer is range System.Min_Int .. System.Max_Int;
   
   type Longest_Unsigned is mod System.Max_Binary_Modulus;
   
   function Format_Integer
     (Value : Item_Type; Fmt_Spec : Format_Data_Record) return String;

   --------------------
   -- Int_xx_To_Text --
   --------------------
   
   procedure Int_8_To_Text is new Int_To_Text (Integer_8, Unsigned_8);
   procedure Int_16_To_Text is new Int_To_Text (Integer_16, Unsigned_16);
   procedure Int_32_To_Text is new Int_To_Text (Integer_32, Unsigned_32);
   procedure Int_64_To_Text is new Int_To_Text (Integer_64, Unsigned_64);
   procedure Int_Max_To_Text is
     new Int_To_Text (Longest_Integer, Longest_Unsigned);

   --------------------
   -- Format_Integer --
   --------------------

   function Format_Integer
     (Value : Item_Type; Fmt_Spec : Format_Data_Record) return String
   is
      Img     : Unbounded_String;
      Neg_Num : Signed_Number_Represent;
   begin
      if Fmt_Spec.Base_Style = C_Base_Style then
         Neg_Num := Twos_Complement;
      else
         Neg_Num := Sign_Magnitude;
      end if;
      
      --  Converts Value to text and formats it
      if Value = 0 and then Fmt_Spec.Precision = 0 then
         return "";
      else
         if Item_Type'Size > 64 then
            Int_Max_To_Text
              (Img, Longest_Integer (Value), Fmt_Spec.Base, Neg_Num);
         elsif Item_Type'Size > 32 then
            Int_64_To_Text (Img, Integer_64 (Value), Fmt_Spec.Base, Neg_Num);
         elsif Item_Type'Size > 16 then
            Int_32_To_Text (Img, Integer_32 (Value), Fmt_Spec.Base, Neg_Num);
         elsif Item_Type'Size > 8 then
            Int_16_To_Text (Img, Integer_16 (Value), Fmt_Spec.Base, Neg_Num);
         else
            Int_8_To_Text (Img, Integer_8 (Value), Fmt_Spec.Base, Neg_Num);
         end if;

         return Format_Integer (Img, Fmt_Spec);
      end if;
   end Format_Integer;

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type is
      Fmt_Copy : Unbounded_String;
      Fmt_Spec : Format_Data_Record;
   begin
      Fmt_Copy := Unbounded_String (Fmt);

      Fmt_Spec.Value_Kind := V_Signed_Integer;
      Fmt_Spec.Align := Right;
      Fmt_Spec.Base_Style := None;
      Parse_Format (Fmt, Fmt_Spec);
      
      --  Handle width from argument
      if Fmt_Spec.Width_From_Arg then
         Replace_Slice
           (Fmt_Copy,
            Fmt_Spec.Width_Asterisk_Pos,
            Fmt_Spec.Width_Asterisk_Pos,
            Trim (Value'Img, Both));
         Fmt_Spec.Width_From_Arg := False;
         Fmt_Spec.Width_Asterisk_Pos := 0;
         return Format_Type (Fmt_Copy);
      end if;
      
      --  Handle precision from argument
      if Fmt_Spec.Precision_From_Arg then
         Replace_Slice
           (Fmt_Copy,
            Fmt_Spec.Precision_Asterisk_Pos,
            Fmt_Spec.Precision_Asterisk_Pos,
            Trim (Value'Img, Both));
         Fmt_Spec.Precision_From_Arg := False;
         Fmt_Spec.Precision_Asterisk_Pos := 0;
         return Format_Type (Fmt_Copy);
      end if;
      
      Replace_Slice
        (Fmt_Copy, Fmt_Spec.Spec_Start, Fmt_Spec.Spec_End,
         Format_Integer (Value, Fmt_Spec));
      return Format_Type (Fmt_Copy);
   end "&";
   
end Formatted_Output.Integer_Output;
