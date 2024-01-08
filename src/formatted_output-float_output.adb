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

package body Formatted_Output.Float_Output is

   package Item_Type_IO is new Ada.Text_IO.Float_IO (Item_Type'Base);
   use Item_Type_IO;

   Maximal_Float_Item_Length : constant := 5200;

   type Hex_Float_Notation is (Decimal_Notation, E_Notation, P_Notation);

   function Dec_To_Hex
     (Number   : Item_Type;
      Notation : Hex_Float_Notation := Decimal_Notation) return String;

   function Integer_Part (Number : Item_Type) return Item_Type'Base;
   --  Returns the integer part of a real Number.

   function Fraction_Part (Number : Item_Type) return Item_Type'Base;
   --  Returns the fractional part of a real Number.

   function Int_Part_To_Str
     (Value : Item_Type'Base; Base : Base_Type) return String;

   function Frac_Part_To_Str
     (Value : Item_Type'Base; Base : Base_Type) return String;

   function Set_Valid_Decimal_Point
     (Text_Value : String;
      Dec_Point  : String) return String;

   function Set_Float_Leading_Zero
     (Text_Value : String;
      Separator  : String;
      Dec_Point  : String;
      Group_Size : Integer) return String;

   function Separate_Float_Digit_Groups
     (Text_Value : String;
      Separator  : String;
      Dec_Point  : String;
      Group_Size : Integer) return String;

   function Format
     (Value                 : Item_Type'Base;
      Initial_Width         : Integer;
      Initial_Width_After   : Integer;
      Strip_Trailing_Zeroes : Boolean;
      Leading_Zero          : Boolean;
      Width_Exp             : Integer;
      Base                  : Base_Type;
      Justification         : Alignment;
      Force_Sign            : Boolean;
      Base_Style            : Base_Style_Kind;
      Digit_Groups          : Digit_Grouping) return String;

   ----------------
   -- Dec_To_Hex --
   ----------------

   function Dec_To_Hex
     (Number   : Item_Type;
      Notation : Hex_Float_Notation := Decimal_Notation) return String
   is
      Base : constant Base_Type := 16;
      S    : Unbounded_String :=
        To_Unbounded_String (if Number < 0.0 then "-" else "");
      Exp  : Natural := 0;
      X    : Item_Type'Base := 0.0;
      B    : constant Item_Type := Item_Type (Base);
      --  High : Natural := 0;

      procedure Make_Decimal_Notation;
      procedure Make_E_Notation;
      procedure Make_P_Notation;

      ---------------------------
      -- Make_Decimal_Notation --
      ---------------------------

      procedure Make_Decimal_Notation is
      begin
         S := S
           & "16#"
           & Int_Part_To_Str (Integer_Part (Number), Base)
           & "."
           & Frac_Part_To_Str (Fraction_Part (Number), Base)
           & "#";
      end Make_Decimal_Notation;

      ---------------------
      -- Make_E_Notation --
      ---------------------

      procedure Make_E_Notation is
      begin
         X := abs Number;

         if X = 0.0 then
            S := S & "16#0.0#E+00";
         elsif X >= 1.0 then
            while X >= B loop
               X := X / B;
               Exp := Exp + 1;
            end loop;

            S := S & "16#"
              & Int_Part_To_Str (Integer_Part (X), Base)
              & "."
              & Frac_Part_To_Str (Fraction_Part (X), Base)
              & "#E+" & Exp'Img (2 .. Exp'Img'Last);
         else
            while X <= 1.0 loop
               X := X * B;
               Exp := Exp + 1;
            end loop;

            S := S & "16#"
              & Int_Part_To_Str (Integer_Part (X), Base)
              & "."
              & Frac_Part_To_Str (Fraction_Part (X), Base)
              & "#E-" & Exp'Img (2 .. Exp'Img'Last);
         end if;
      end Make_E_Notation;

      ------------
      -- Make_P --
      ------------

      procedure Make_P_Notation is
      begin
         X := abs Number;

         if X = 0.0 then
            S := S & "0X0P+0";
         elsif X >= 1.0 then
            while X >= 2.0 loop
               X := X / 2.0;
               Exp := Exp + 1;
            end loop;

            S := S & "0X"
              & Int_Part_To_Str (Integer_Part (X), Base)
              & "."
              & Frac_Part_To_Str (Fraction_Part (X), Base)
              & "P+" & Exp'Img (2 .. Exp'Img'Last);
         else
            while X <= 1.0 loop
               X := X * 2.0;
               Exp := Exp + 1;
            end loop;

            S := S & "0X"
              & Int_Part_To_Str (Integer_Part (X), Base)
              & "."
              & Frac_Part_To_Str (Fraction_Part (X), Base)
              & "P-" & Exp'Img (2 .. Exp'Img'Last);
         end if;
      end Make_P_Notation;

   begin  --  Dec_To_Hex
      case Notation is
         when Decimal_Notation =>
            Make_Decimal_Notation;
         when E_Notation =>
            Make_E_Notation;
         when P_Notation =>
            Make_P_Notation;
      end case;

      return To_String (S);
   end Dec_To_Hex;

   ------------------
   -- Integer_Part --
   ------------------

   function Integer_Part (Number : Item_Type) return Item_Type'Base is
      Result : constant Item_Type := Item_Type'Truncation (Number);
   begin
      if Number >= 0.0 then
         return Result;
      else
         return Result - 1.0;
      end if;
   end Integer_Part;

   -------------------
   -- Fraction_Part --
   -------------------

   function Fraction_Part (Number : Item_Type) return Item_Type'Base is
     (Number - Integer_Part (Number));

   ---------------------
   -- Int_Part_To_Str --
   ---------------------

   function Int_Part_To_Str
     (Value : Item_Type'Base; Base : Base_Type) return String
   is
      Digit_Symbols : constant String := "0123456789ABCDEF";
      Result        : Unbounded_String;
      Divider       : Item_Type'Base;
      Quotient      : Item_Type'Base;
      Remainder     : Item_Type'Base;
      X             : Item_Type'Base;
   begin
      if Value = 0.0 then
         return "0";
      else
         X := abs Value;
         Quotient := abs Value;
         Divider := Item_Type (Base);

         while Quotient /= 0.0 loop
            Quotient := Item_Type'Truncation (X / Divider);
            Remainder := X - Quotient * Divider ;
            Result := Digit_Symbols (Natural (Remainder) + 1) & Result;
            X := Quotient;
         end loop;
      end if;

      return To_String (Result);
   end Int_Part_To_Str;

   ----------------------
   -- Frac_Part_To_Str --
   ----------------------

   function Frac_Part_To_Str
     (Value : Item_Type'Base; Base : Base_Type) return String
   is
      Digit_Symbols : constant String := "0123456789ABCDEF";
      Result        : Unbounded_String;
      X             : Item_Type'Base;
      Product       : Item_Type'Base;
      Multiplier    : Item_Type'Base;
   begin
      if Value = 0.0 then
         return "0";
      else
         X := abs Value;
         Product := abs Value;
         Multiplier := Item_Type (Base);

         while X /= 0.0 loop
            Product := X * Multiplier;
            Result :=
              Result & Digit_Symbols (Natural (Integer_Part (Product)) + 1);
            X := Fraction_Part (Product);
         end loop;
      end if;

      return To_String (Result);
   end Frac_Part_To_Str;

   -----------------------------
   -- Set_Valid_Decimal_Point --
   -----------------------------

   function Set_Valid_Decimal_Point
     (Text_Value : String;
      Dec_Point  : String) return String
   is
      DP : Natural := 0;
   begin
      DP := Index (Text_Value, Ada_Dec_Point_Character, Text_Value'First);

      if DP > 0 then
         return
           Text_Value (Text_Value'First .. DP - 1)
           & Dec_Point
           & Text_Value (DP + 1 .. Text_Value'Last);
      end if;

      return Text_Value;
   end Set_Valid_Decimal_Point;

   ----------------------------
   -- Set_Float_Leading_Zero --
   ----------------------------

   function Set_Float_Leading_Zero
     (Text_Value : String;
      Separator  : String;
      Dec_Point  : String;
      Group_Size : Integer) return String
   is
      DP : Natural := 0;
   begin
      DP := Index (Text_Value, Dec_Point, Text_Value'First);

      if DP > 0 then
         return
           Set_Leading_Zero
             (Text_Value (Text_Value'First .. DP - 1), Separator, Group_Size)
             & Text_Value (DP .. Text_Value'Last);
      else
         return Set_Leading_Zero (Text_Value, Separator, Group_Size);
      end if;
   end Set_Float_Leading_Zero;

   ---------------------------------
   -- Separate_Float_Digit_Groups --
   ---------------------------------

   function Separate_Float_Digit_Groups
     (Text_Value : String;
      Separator  : String;
      Dec_Point  : String;
      Group_Size : Integer) return String
   is
      FD  : Natural := Index_Non_Blank (Text_Value, Forward);
      LD  : Natural := Index_Non_Blank (Text_Value, Backward);
      SP  : Natural := 0;
      DP  : Natural := 0;
      EP  : Natural := 0;
      EL  : Natural := Index (Text_Value, "e", Text_Value'Last, Backward);
      EU  : Natural := Index (Text_Value, "E", Text_Value'Last, Backward);
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
         Res := To_Unbounded_String (Text_Value (Text_Value'First .. SP));
      else
         Res := To_Unbounded_String (Text_Value (Text_Value'First .. FD - 1));
      end if;

      DP := Index (Text_Value, Ada_Dec_Point_Character, FD);

      if EL > 0 then
         EP := EL;
      elsif EU > 0 then
         EP := EU;
      else
         EP := LD;
      end if;

      declare
         E  : String :=
           (if EL + EU > 0 then Text_Value (EP .. LD) else "");
         I  : String :=
           (if DP > 0 then Text_Value (FD .. DP - 1)
            else Text_Value (FD .. EP));
         F  : String :=
           (if DP > 0 then
              (if EL + EU > 0 then Text_Value (DP + 1 .. EP - 1)
               else Text_Value (DP + 1 .. EP))
            else "");
         --  FS : String :=
         --    (if Separator /= Ada_Sep_Character then "" else Separator);
      begin
         Res := Res
           & Separate_Digit_Groups (I, Separator, Group_Size)
           & (if DP > 0 then
              --  Dec_Point & Separate_Digit_Groups (F, FS, Group_Size)
                 Dec_Point & F
              else "")
           & E;
      end;

      return To_String (Res);
   end Separate_Float_Digit_Groups;

   ------------
   -- Format --
   ------------

   function Format
     (Value                 : Item_Type'Base;
      Initial_Width         : Integer;
      Initial_Width_After   : Integer;
      Strip_Trailing_Zeroes : Boolean;
      Leading_Zero          : Boolean;
      Width_Exp             : Integer;
      Base                  : Base_Type;
      Justification         : Alignment;
      Force_Sign            : Boolean;
      Base_Style            : Base_Style_Kind;
      Digit_Groups          : Digit_Grouping) return String
   is
      Img          : String (1 .. Maximal_Float_Item_Length);
      Width        : Integer;
      Width_After  : Integer;
      Real_Width   : Integer;
      Pre_First    : Natural := Img'Last;
      Last         : Natural := Img'Last;
      Item         : Item_Type'Base := Value;
      Hex_Notation : Hex_Float_Notation := Decimal_Notation;
   begin
      if Initial_Width_After = 0 then
         if Strip_Trailing_Zeroes then
            Item := Item_Type'Base'Rounding (Value);
         end if;

         Width_After := Item_Type_IO.Default_Aft;
      else
         Width_After := Initial_Width_After;
      end if;

      if Base = 10 then
         Put
           (To   => Img,
            Item => Item,
            Aft  => Field (Width_After),
            Exp  => Field (Width_Exp));
      else
         case Base_Style is
            when None           => Hex_Notation := Decimal_Notation;
            when C_Base_Style   => Hex_Notation := P_Notation;
            when Ada_Base_Style => Hex_Notation := E_Notation;
         end case;

         Move
           (Source  => Dec_To_Hex (Value, Hex_Notation),
            Target  => Img,
            Drop    => Error,
            Justify => Right,
            Pad     => ' ');
      end if;

      while Img (Pre_First) /= ' ' loop
         Pre_First := Pre_First - 1;
      end loop;

      if Strip_Trailing_Zeroes then
         while Img (Last) = '0' loop
            Last := Last - 1;
         end loop;

         if Img (Last) = '.' then
            Last := Last - 1;
         end if;
      end if;

      if Item > 0.0 and then Force_Sign then
         Img (Pre_First) := '+';
         Pre_First := Pre_First - 1;
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
         P : String :=
           (if Digit_Groups = Ada_Grouping_Style then Ada_Dec_Point_Character
            else Decimal_Point_Character);
         G : Integer := 3;
         --             (if Digit_Groups = NLS_Style or Base = 10 then 3 else 4);
      begin
         case Digit_Groups is
            when None               =>
               T := To_Unbounded_String (Set_Valid_Decimal_Point (V, P));
            when Ada_Grouping_Style
               | NLS_Grouping_Style =>
               T := To_Unbounded_String
                 (Separate_Float_Digit_Groups (V, L, P, G));
         end case;

         if Length (T) > Width then
            return To_String (T);
         else
            Move
              (Source  => To_String (T),
               Target  => S,
               Drop    => Error,
               Justify => Justification,
               Pad     => Filler);

            if Leading_Zero then
               S := Set_Float_Leading_Zero (S, L, P, G);
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
      After_Point           : Boolean := False;
      Width                 : Integer := 0;
      Width_After           : Integer := 0;
      Digit_Occured         : Boolean := False;
      Justification_Changed : Boolean := False;
      Justification         : Alignment := Right;
      Force_Sign            : Boolean := False;
      Digit_Groups          : Digit_Grouping := None;
      Fmt_Copy              : Unbounded_String;
      Base_Style            : Base_Style_Kind := None;
   begin
      if Command_Start /= 0 then
         Fmt_Copy := Unbounded_String (Fmt);

         for I in Command_Start + 1 .. Length (Fmt_Copy) loop
            case Element (Fmt_Copy, I) is
               when 'e'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     To_Lower (
                       Format
                         (Value, Width, Width_After, False, Leading_Zero,
                          Default_Exp, 10, Justification, Force_Sign,
                          None, Digit_Groups)));
                  return Format_Type (Fmt_Copy);

               when 'E'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     To_Upper (
                       Format
                         (Value, Width, Width_After, False, Leading_Zero,
                          Default_Exp, 10, Justification, Force_Sign,
                          None, Digit_Groups)));
                  return Format_Type (Fmt_Copy);

               when 'f'        =>
                  if After_Point and then Width_After = 0 then
                     Replace_Slice
                       (Fmt_Copy, Command_Start, I,
                        Format
                          (Value, Width, Width_After, True, Leading_Zero,
                           0, 10, Justification, Force_Sign, None,
                           Digit_Groups));
                  else
                     Replace_Slice
                       (Fmt_Copy, Command_Start, I,
                        Format
                          (Value, Width, Width_After, False, Leading_Zero,
                           0, 10, Justification, Force_Sign, None,
                           Digit_Groups));
                  end if;

                  return Format_Type (Fmt_Copy);

               when 'g'        =>
                  if After_Point and then Width_After = 0 then
                     Replace_Slice
                       (Fmt_Copy, Command_Start, I,
                        Format
                          (Value, Width, 1, True, Leading_Zero,
                           0, 10, Justification, Force_Sign, None,
                           Digit_Groups));
                  elsif not After_Point then
                     Replace_Slice
                       (Fmt_Copy, Command_Start, I,
                        Format
                          (Value, Width, Default_Aft, True, Leading_Zero,
                           0, 10, Justification, Force_Sign, None,
                           Digit_Groups));
                  else
                     Replace_Slice
                       (Fmt_Copy, Command_Start, I,
                        Format
                          (Value, Width, Width_After, True, Leading_Zero,
                           0, 10, Justification, Force_Sign, None,
                           Digit_Groups));
                  end if;

                  return Format_Type (Fmt_Copy);

               when 'a'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     To_Lower (
                       Format
                         (Value, Width, Width_After, False, Leading_Zero,
                          Default_Exp, 16, Justification, Force_Sign,
                          Base_Style, Digit_Groups)));
                  return Format_Type (Fmt_Copy);

               when 'A'        =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     To_Upper (
                       Format
                         (Value, Width, Width_After, False, Leading_Zero,
                          Default_Exp, 16, Justification, Force_Sign,
                          Base_Style, Digit_Groups)));
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

               when '.'        =>
                  Digit_Occured := True;

                  if After_Point then
                     raise Format_Error;
                  else
                     After_Point := True;
                  end if;

               when '0' .. '9' =>
                  Digit_Occured := True;

                  if After_Point then
                     Width_After := Width_After * 10
                       + Character'Pos (Element (Fmt_Copy, I))
                       - Character'Pos ('0');
                  else
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

end Formatted_Output.Float_Output;
