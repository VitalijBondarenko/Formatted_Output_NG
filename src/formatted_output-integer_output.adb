------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2016-2023 Vitalii Bondarenko <vibondare@gmail.com>         --
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
with Ada.Unchecked_Conversion;
with Interfaces;              use Interfaces;
with System;

package body Formatted_Output.Integer_Output is

   package Item_Type_IO is new Ada.Text_IO.Integer_IO (Item_Type'Base);
   use Item_Type_IO;

   type Neg_Number_Represent is (Sign_Magnitude, Twos_Complement);

   generic
      type Int is range <>;
      type Uns is mod <>;
   procedure Int_To_Str
     (To        : out String;
      Value     : Int;
      Base      : Base_Type;
      Neg_Style : Neg_Number_Represent);
   
   function Separate_Integer_Digit_Groups
     (Text_Value : String;
      Separator  : String;
      Group_Size : Integer) return String;
   
   function Format
     (Value         : Item_Type'Base;
      Initial_Width : Integer;
      Leading_Zero  : Boolean;
      Base          : Base_Type;
      Justification : Alignment;
      Force_Sign    : Boolean;
      Base_Style    : Base_Style_Kind;
      Digit_Groups  : Digit_Grouping) return String;
   
   ----------------
   -- Int_To_Str --
   ----------------
   
   procedure Int_To_Str
     (To        : out String;
      Value     : Int;
      Base      : Base_Type;
      Neg_Style : Neg_Number_Represent)
   is
      function To_Unsigned is new Ada.Unchecked_Conversion (Int, Uns);
   
      Digit_Symbols : constant String := "0123456789ABCDEF";
      Result        : String (1 .. 255) := (others => ' ');
      Index         : Natural := Result'Last;
      Divider       : Int := 0;
      Quotient      : Int := 0;
      Remainder     : Int := 0;
      Uns_Q         : Uns := 0;
      Uns_D         : Uns := 0;
      Uns_R         : Uns := 0;
   begin
      if Value = 0 then
         Result (Index) := '0';
   
      elsif Value > 0 then
         Quotient := Value;
         Divider := Int (Base);
   
         while Quotient /= 0 loop
            Remainder := Quotient rem Divider;
            Quotient := Quotient / Divider;
            Result (Index) := Digit_Symbols (Natural (Remainder) + 1);
            Index := Index - 1;
         end loop;
   
         Index := Index + 1;
   
      else
         case Neg_Style is
            when Sign_Magnitude  =>
               Uns_Q := To_Unsigned (abs (Value + 1)) + 1;
            when Twos_Complement =>
               Uns_Q := To_Unsigned (Value);
         end case;
   
         Uns_D := To_Unsigned (Int (Base));
   
         while Uns_Q /= 0 loop
            Uns_R := Uns_Q rem Uns_D;
            Uns_Q := Uns_Q / Uns_D;
            Result (Index) := Digit_Symbols (Natural (Uns_R) + 1);
            Index := Index - 1;
         end loop;
   
         if Neg_Style = Sign_Magnitude then
            Result (Index) := '-';
         else
            Index := Index + 1;
         end if;
      end if;
   
      if Result'Last - Index + 1 > To'Length then
         raise Layout_Error;
      else
         To (To'First .. To'Last - (Result'Length - Index)) := (others => ' ');
         To (To'Last - (Result'Length - Index) .. To'Last) :=
           Result (Index .. Result'Last);
      end if;
   end Int_To_Str;

   ------------------
   -- Int_xx_To_Str --
   ------------------
   
   procedure Int_8_To_Str is new Int_To_Str (Integer_8, Unsigned_8);
   procedure Int_16_To_Str is new Int_To_Str (Integer_16, Unsigned_16);
   procedure Int_32_To_Str is new Int_To_Str (Integer_32, Unsigned_32);
   procedure Int_64_To_Str is new Int_To_Str (Integer_64, Unsigned_64);
   
   type Longest_Integer is range System.Min_Int .. System.Max_Int;
   type Longest_Unsigned is mod System.Max_Binary_Modulus;
   procedure Int_Max_To_Str is new Int_To_Str (Longest_Integer, Longest_Unsigned);

   -----------------------------------
   -- Separate_Integer_Digit_Groups --
   -----------------------------------
   
   function Separate_Integer_Digit_Groups
     (Text_Value : String;
      Separator  : String;
      Group_Size : Integer) return String
   is
      FD  : Natural := Index_Non_Blank (Text_Value, Forward);
      LD  : Natural := Index_Non_Blank (Text_Value, Backward);
      SP  : Natural := 0;
      Res : Unbounded_String := Null_Unbounded_String;
   begin
      if Separator'Length = 0 then
         return Text_Value;
      end if;
      
      if Text_Value (FD) = '-' or else Text_Value (FD) = '+' then
         SP := FD;
         FD := FD + 1;
      end if;

      if SP > 0 then
         Res := Text_Value (Text_Value'First .. SP) & Res;
      else
         Res := Text_Value (Text_Value'First .. FD - 1) & Res;
      end if;

      Res := Res
        & Separate_Digit_Groups (Text_Value (FD .. LD), Separator, Group_Size)
        & Text_Value (LD + 1 .. Text_Value'Last);
      return To_String (Res);
   end Separate_Integer_Digit_Groups;

   ------------
   -- Format --
   ------------

   function Format
     (Value         : Item_Type'Base;
      Initial_Width : Integer;
      Leading_Zero  : Boolean;
      Base          : Base_Type;
      Justification : Alignment;
      Force_Sign    : Boolean;
      Base_Style    : Base_Style_Kind;
      Digit_Groups  : Digit_Grouping) return String
   is
      Img        : String (1 .. Maximal_Item_Length);
      Width      : Integer;
      Real_Width : Integer;
      Pre_First  : Natural;
      Last       : Natural;
      Neg_Num    : Neg_Number_Represent;
   begin
      if Base_Style = C_Base_Style then
         Neg_Num := Twos_Complement;
      else
         Neg_Num := Sign_Magnitude;
      end if;
      
      if Item_Type'Base'Size > 64 then
         Int_Max_To_Str (Img, Longest_Integer (Value), Base, Neg_Num);
      elsif Item_Type'Base'Size > 32 then
         Int_64_To_Str (Img, Integer_64 (Value), Base, Neg_Num);
      elsif Item_Type'Base'Size > 16 then
         Int_32_To_Str (Img, Integer_32 (Value), Base, Neg_Num);
      elsif Item_Type'Base'Size > 8 then
         Int_16_To_Str (Img, Integer_16 (Value), Base, Neg_Num);
      else
         Int_8_To_Str (Img, Integer_8 (Value), Base, Neg_Num);
      end if;
      
      Last := Img'Last;
      Pre_First := Last;
   
      while Img (Pre_First) /= ' ' loop
         Pre_First := Pre_First - 1;
      end loop;
      
      if Value < 0 and then Img (Pre_First + 1) = '-' then
         Pre_First := Pre_First + 1;
         Img (Pre_First) := ' ';
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
                 (Separate_Integer_Digit_Groups (V, L, G));
            when NLS_Grouping_Style =>
               if Base = 10 then
                  T := To_Unbounded_String
                    (Separate_Integer_Digit_Groups (V, L, G));
               else
                  T := To_Unbounded_String (V);
               end if;
         end case;

         case Base_Style is
            when None           =>
               if Value < 0 then --  and Base = 10 then
                  T := "-" & T;
               elsif Value > 0 and then Force_Sign then
                  T := "+" & T;
               end if;
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
               
               if Value < 0 and Base /= 10 then
                  T := "-" & T;
               elsif Value > 0 and then Force_Sign then
                  T := "+" & T;
               end if;
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
      Force_Sign            : Boolean := False;
      Force_Base            : Boolean := False;
      Base_Style            : Base_Style_Kind := None;
      Digit_Groups          : Digit_Grouping := None;
      Fmt_Copy              : Unbounded_String;
      After_Point_Ignore    : Boolean := False;
   begin
      if Command_Start /= 0 then
         Fmt_Copy := Unbounded_String (Fmt);
         
         for I in Command_Start + 1 .. Length (Fmt_Copy) loop
            case Element (Fmt_Copy, I) is
               when 'd'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format
                       (Value, Width, Leading_Zero, 10,
                        Justification, Force_Sign, None, Digit_Groups));
                  return Format_Type (Fmt_Copy);
                  
               when 'x'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     To_Lower (
                       Format
                         (Value, Width, Leading_Zero, 16, Justification,
                          Force_Sign, Base_Style, Digit_Groups)));
                  return Format_Type (Fmt_Copy);
                  
               when 'X'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     To_Upper (
                       Format
                         (Value, Width, Leading_Zero, 16, Justification,
                          Force_Sign, Base_Style, Digit_Groups)));
                  return Format_Type (Fmt_Copy);
                  
               when 'o'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format
                       (Value, Width, Leading_Zero, 8,
                        Justification, Force_Sign, Base_Style, Digit_Groups));
                  return Format_Type (Fmt_Copy);
                  
               when 'b'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format
                       (Value, Width, Leading_Zero, 2,
                        Justification, Force_Sign, Base_Style, Digit_Groups));
                  return Format_Type (Fmt_Copy);
                  
               when '_'        =>
                  Digit_Groups := Ada_Grouping_Style;
                  
               when '''        =>
                  Digit_Groups := NLS_Grouping_Style;
                  
               when '+'        =>
                  Force_Sign := True;
                  
               when '#'        =>
                  Base_Style := C_Base_Style;
                  
               when '~'        =>
                  Base_Style := Ada_Base_Style;
                  
               when '-' | '<'
                  | '>' | '^'  =>
                  if Justification_Changed or else Digit_Occured then
                     raise Format_Error;
                  end if;
                  
                  Justification_Changed := True;
                  
                  case Element (Fmt_Copy, I) is
                     when '-' | '<' => Justification := Left;
                     when '>'       => Justification := Right;
                     when '^'       => Justification := Center;
                     when others    => null;
                  end case;
                  
               when '*'        =>
                  Replace_Slice (Fmt_Copy, I, I, Trim (Value'Img, Both));
                  return Format_Type (Fmt_Copy);

               when '.'        =>
                  if Element (Fmt_Copy, I + 1) = '*' then
                     Replace_Slice
                       (Fmt_Copy, I + 1, I + 1, Trim (Value'Img, Both));
                     return Format_Type (Fmt_Copy);
                  else
                     After_Point_Ignore := True;
                  end if;
                  
               when '0' .. '9' =>
                  if not After_Point_Ignore then
                     Digit_Occured := True;
                  
                     if Width = 0 and then Element (Fmt_Copy, I) = '0' then
                        Leading_Zero := True;
                     else
                        Width := Width * 10
                          + Character'Pos (Element (Fmt_Copy, I))
                          - Character'Pos ('0');
                     end if;
                  end if;
                  
               when others     =>
                  raise Format_Error;
            end case;
         end loop;
      end if;
      
      raise Format_Error;
   end "&";

end Formatted_Output.Integer_Output;
