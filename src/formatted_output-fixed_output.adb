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

with Formatted_Output.Utils; use Formatted_Output.Utils;

package body Formatted_Output.Fixed_Output is

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type is
      Fmt_Copy : Unbounded_String;
      Fmt_Spec : Format_Data_Record;
      Img      : Real_Image;
   begin
      Fmt_Copy := Unbounded_String (Fmt);

      Fmt_Spec.Value_Kind := V_Fixed;
      Fmt_Spec.Align := Right;
      --  Fmt_Spec.Base_Style := Ada_Base_Style;
      Fmt_Spec.Notation := Decimal_Notation;
      Parse_Format (Fmt, Fmt_Spec);

      --  Check precision
      if Fmt_Spec.Precision = Undefined then
         Fmt_Spec.Precision := Item_Type'Aft;
      end if;

      Real_To_Text (Img, Long_Long_Float (Value), Fmt_Spec);
      Replace_Slice
        (Fmt_Copy, Fmt_Spec.Spec_Start, Fmt_Spec.Spec_End,
         Format_Real (Img, Fmt_Spec));
      return Format_Type (Fmt_Copy);
   end "&";

end Formatted_Output.Fixed_Output;
