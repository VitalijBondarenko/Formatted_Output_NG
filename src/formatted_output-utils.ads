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

private package Formatted_Output.Utils is

   Ada_Dec_Point_Character : constant String := ".";
   Ada_Sep_Character       : constant String := "_";
   Digit_Symbols           : constant String := "0123456789ABCDEF";
   Undefined               : constant Integer := -1;

   type Digit_Grouping_Kind is (None, Ada_Grouping_Style, NLS_Grouping_Style);

   subtype Base_Type is Integer range 2 .. 16;

   type Base_Style_Kind is (None, C_Base_Style, Ada_Base_Style);

   type Number_Notation is (Decimal_Notation, E_Notation, P_Notation);

   type Signed_Number_Represent is (Sign_Magnitude, Twos_Complement);

   type Value_Type_Kind is
     (V_Unknown,
      V_Signed_Integer, V_Unsigned_Integer, V_Float, V_Fixed, V_Decimal,
      V_Enum, V_Pointer, V_Str, V_Char);

   type Letter_Case_Style is (Capitalized, Lower_Case, Upper_Case, Mixed);

   type Sign_Style_Kind is (Negative_Only, Always_Show, Space_Show);

   type Format_Data_Record is record
      --  Common properties
      Spec_Start             : Integer := 0;
      Spec_End               : Integer := 0;
      Value_Kind             : Value_Type_Kind := V_Unknown;
      Width                  : Natural := 0;
      Precision              : Integer := Undefined;
      Align                  : Alignment := Left;
      Letter_Case            : Letter_Case_Style := Lower_Case;
      Width_From_Arg         : Boolean := False;
      Width_Asterisk_Pos     : Integer := 0;
      Precision_From_Arg     : Boolean := False;
      Precision_Asterisk_Pos : Integer := 0;

      --  Number properties
      Base                   : Base_Type := 10;
      Base_Style             : Base_Style_Kind := None;
      Width_Exp              : Natural := 3;
      Notation               : Number_Notation := Decimal_Notation;
      G_Spec_Used            : Boolean := False;
      Strip_Trailing_Zeroes  : Boolean := False;
      Zero_Padded            : Boolean := False;
      Sign_Style             : Sign_Style_Kind := Negative_Only;
      Digit_Groups           : Digit_Grouping_Kind := None;
   end record;

   procedure Parse_Format
     (Fmt : Format_Type; Fmt_Spec : out Format_Data_Record);

   function Decimal_Point_Character return String;

   function Thousands_Sep_Character return String;

   procedure Separate_Digit_Groups
     (Text_Value : in out Unbounded_String;
      Fmt        : Format_Data_Record);

   procedure Set_Zero_Padded
     (Text_Value : in out Unbounded_String;
      Zero_Count : Integer;
      Fmt        : Format_Data_Record);

   ----------------------------------------------------------------------------
   -- Integer number utils
   ----------------------------------------------------------------------------

   generic
      type Int is range <>;
      type Uns is mod <>;
   procedure Int_To_Text
     (To        : out Unbounded_String;
      Value     : Int;
      Base      : Base_Type;
      Neg_Style : Signed_Number_Represent);

   generic
      type Uns is mod <>;
   procedure Uns_To_Text
     (To        : out Unbounded_String;
      Value     : Uns;
      Base      : Base_Type);

   function Format_Integer
     (Value : Unbounded_String; Fmt_Spec : Format_Data_Record) return String;

   ----------------------------------------------------------------------------
   -- Real number utils
   ----------------------------------------------------------------------------

   type Real_Image is record
      Int_Part    : Unbounded_String;
      Frac_Part   : Unbounded_String;
      Is_Negative : Boolean := False;
      Exp         : Integer := 0;
   end record;

   procedure Real_To_Text
     (To    : out Real_Image;
      Value : Long_Long_Float;
      Fmt   : in out Format_Data_Record);

   function Format_Real
     (Value    : Real_Image;
      Fmt_Spec : Format_Data_Record) return String;

end Formatted_Output.Utils;
