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

with Interfaces.C.Strings;     use Interfaces.C.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

with L10n.Localeinfo;          use L10n.Localeinfo;

with Formatted_Output_Integer; use Formatted_Output_Integer;

package body Formatted_Output.Utils is

   ------------------
   -- Parse_Format --
   ------------------

   procedure Parse_Format
     (Fmt : Format_Type; Fmt_Spec : out Format_Data_Record)
   is
      I : Natural := 1;
      J : Natural := 0;
   begin
      --  Search percent sign
      while I < Length (Fmt) loop
         if Element (Fmt, I) = '%' then
            if Element (Fmt, I + 1) /= '%' then
               Fmt_Spec.Spec_Start := I;
               exit;
            else
               I := I + 1;
            end if;
         end if;

         I := I + 1;
      end loop;

      if I >= Length (Fmt) then
         Raise_Format_Error (Fmt);
      end if;

      I := I + 1;

      --  Parse flags
      while I < Length (Fmt) loop
         case Element (Fmt, I) is
            when '<' | '-' => --  left alignment
               Fmt_Spec.Align := Left;
            when '^'       => --  center alignment
               Fmt_Spec.Align := Center;
            when '>'       => --  right alignment
               Fmt_Spec.Align := Right;
            when ' '       => --  space before positive number
               if Fmt_Spec.Sign_Style /= Always_Show then
                  Fmt_Spec.Sign_Style := Space_Show;
               end if;
            when '+'       => --  always place sign before number
               Fmt_Spec.Sign_Style := Always_Show;
            when '#'       => --  C based number style
               Fmt_Spec.Base_Style := C_Base_Style;
               Fmt_Spec.Notation := P_Notation;
               Fmt_Spec.Width_Exp := 2;
            when '~'       => --  Ada based number style
               Fmt_Spec.Base_Style := Ada_Base_Style;
               Fmt_Spec.Notation := E_Notation;
               Fmt_Spec.Width_Exp := 3;
            when '''       => --  current locale based digit groups
               Fmt_Spec.Digit_Groups := NLS_Grouping_Style;
            when '_'       => --  Ada based digit groups by '_' char
               Fmt_Spec.Digit_Groups := Ada_Grouping_Style;
            when '0'       => --  zero pad number
               Fmt_Spec.Zero_Padded := True;
            when others    =>
               exit;
         end case;

         I := I + 1;
      end loop;

      --  Get width
      if Element (Fmt, I) in '0' .. '9' then
         J := I;

         while
           I < Length (Fmt) and then Element (Fmt, I + 1) in '0' .. '9'
         loop
            I := I + 1;
         end loop;

         Fmt_Spec.Width := Natural'Value (Slice (Fmt, J, I));
         I := I + 1;
      elsif Element (Fmt, I) = '*' then
         Fmt_Spec.Width_From_Arg := True;
         Fmt_Spec.Width_Asterisk_Pos := I;
         I := I + 1;
      end if;

      -- Get precision
      if Element (Fmt, I) = '.' then
         I := I + 1;

         if Element (Fmt, I) in '0' .. '9' then
            J := I;

            while
              I < Length (Fmt) and then Element (Fmt, I + 1) in '0' .. '9'
            loop
               I := I + 1;
            end loop;

            Fmt_Spec.Precision := Natural'Value (Slice (Fmt, J, I));
            I := I + 1;
         elsif Element (Fmt, I) = '*' then
            Fmt_Spec.Precision_From_Arg := True;
            Fmt_Spec.Precision_Asterisk_Pos := I;
            I := I + 1;
         end if;
      end if;

      if I > Length (Fmt) then
         Raise_Format_Error (Fmt);
      end if;

      --  Check conversion specifier
      case Element (Fmt, I) is
         --  String
         when 's'       =>
            null;

            --  Character
         when 'c'       =>
            null;

            --  Integer number
         when 'd' | 'i' =>
            Fmt_Spec.Base := 10;
         when 'b'       =>
            Fmt_Spec.Base := 2;
         when 'o'       =>
            Fmt_Spec.Base := 8;
         when 'x' | 'X'  =>
            Fmt_Spec.Base := 16;
            if Element (Fmt, I) = 'x' then
               Fmt_Spec.Letter_Case := Lower_Case;
            else
               Fmt_Spec.Letter_Case := Upper_Case;
            end if;
         when 'u'       =>
            Fmt_Spec.Base := 10;
            Fmt_Spec.Sign_Style := Negative_Only;

            --  Real number
         when 'f' | 'F'  =>
            Fmt_Spec.Base := 10;
            Fmt_Spec.Width_Exp := 0;
            Fmt_Spec.Notation := Decimal_Notation;
            if Element (Fmt, I) = 'f' then
               Fmt_Spec.Letter_Case := Lower_Case;
            else
               Fmt_Spec.Letter_Case := Upper_Case;
            end if;
         when 'g' | 'G' =>
            Fmt_Spec.Base := 10;
            Fmt_Spec.Width_Exp := 0;
            Fmt_Spec.Notation := Decimal_Notation;
            Fmt_Spec.G_Spec_Used := True;
            Fmt_Spec.Strip_Trailing_Zeroes := True;
            if Element (Fmt, I) = 'g' then
               Fmt_Spec.Letter_Case := Lower_Case;
            else
               Fmt_Spec.Letter_Case := Upper_Case;
            end if;
         when 'e' | 'E'  =>
            Fmt_Spec.Base := 10;
            Fmt_Spec.Base_Style := None;
            Fmt_Spec.Width_Exp := 3;
            Fmt_Spec.Notation := E_Notation;
            if Element (Fmt, I) = 'e' then
               Fmt_Spec.Letter_Case := Lower_Case;
            else
               Fmt_Spec.Letter_Case := Upper_Case;
            end if;
         when 'a' | 'A'  =>
            Fmt_Spec.Base := 16;
            Fmt_Spec.Strip_Trailing_Zeroes := True;
            if Element (Fmt, I) = 'a' then
               Fmt_Spec.Letter_Case := Lower_Case;
            else
               Fmt_Spec.Letter_Case := Upper_Case;
            end if;

            --  System.Address
         when 'p'       =>
            Fmt_Spec.Letter_Case := Lower_Case;
         when 'P'       =>
            Fmt_Spec.Letter_Case := Upper_Case;

            --  Enumeration
         when 't'       =>
            Fmt_Spec.Letter_Case := Capitalized;
         when 'l'       =>
            Fmt_Spec.Letter_Case := Lower_Case;
         when 'U'       =>
            Fmt_Spec.Letter_Case := Upper_Case;
         when 'm'       =>
            Fmt_Spec.Letter_Case := Mixed;

            --  Others
         when others    =>
            Raise_Format_Error (Fmt);
      end case;

      Fmt_Spec.Spec_End := I;
   end Parse_Format;

   -----------------------------
   -- Decimal_Point_Character --
   -----------------------------

   function Decimal_Point_Character return String is
      Lconv : constant C_Lconv_Access := C_Localeconv;
   begin
      if Lconv.Decimal_Point = Null_Ptr then
         return Ada_Dec_Point_Character;
      else
         return Value (Lconv.Decimal_Point);
      end if;
   exception
      when others => return Ada_Dec_Point_Character;
   end Decimal_Point_Character;

   -----------------------------
   -- Thousands_Sep_Character --
   -----------------------------

   function Thousands_Sep_Character return String is
      Lconv : constant C_Lconv_Access := C_Localeconv;
   begin
      if Lconv.Thousands_Sep = Null_Ptr then
         return "";
      else
         declare
            S : constant String := Value (Lconv.Thousands_Sep);
         begin
            if S'Length > 1 then
               return " ";
            else
               return S;
            end if;
         end;
      end if;
   exception
      when others => return "";
   end Thousands_Sep_Character;

   ---------------------------
   -- Separate_Digit_Groups --
   ---------------------------

   procedure Separate_Digit_Groups
     (Text_Value : in out Unbounded_String;
      Fmt        : Format_Data_Record)
   is
      Sep_Char    : String :=
        (if Fmt.Digit_Groups = NLS_Grouping_Style
         then Thousands_Sep_Character
         elsif Fmt.Digit_Groups = Ada_Grouping_Style
         then Ada_Sep_Character
         else "");
      Group_Size  : Integer :=
        (if Fmt.Digit_Groups = NLS_Grouping_Style or Fmt.Base = 10
         then 3 else 4);
      First_Digit : Integer := 0;
      I           : Integer;
      J           : Integer := Length (Text_Value);
   begin
      if Sep_Char'Length = 0 then
         return;
      end if;

      --  Find first digit in Text_Value
      if Element (Text_Value, 1) = '-' or Element (Text_Value, 1) = '+' then
         First_Digit := 2;
      elsif Element (Text_Value, 1) = ' ' and Fmt.Sign_Style = Space_Show then
         First_Digit := 2;
      else
         First_Digit := 1;
      end if;

      while J >= First_Digit loop
         I := J - Group_Size + 1;

         if I <= First_Digit then
            exit;
         else
            Insert (Text_Value, I, Sep_Char);
         end if;

         J := I - 1;
      end loop;
   end Separate_Digit_Groups;

   ---------------------
   -- Set_Zero_Padded --
   ---------------------

   procedure Set_Zero_Padded
     (Text_Value : in out Unbounded_String;
      Zero_Count : Integer;
      Fmt        : Format_Data_Record)
   is
      Sep_Char    : String :=
        (if Fmt.Digit_Groups = NLS_Grouping_Style
         then Thousands_Sep_Character
         elsif Fmt.Digit_Groups = Ada_Grouping_Style
         then Ada_Sep_Character
         else "");
      Group_Size  : Integer :=
        (if Fmt.Digit_Groups = NLS_Grouping_Style or Fmt.Base = 10
         then 3 else 4);
      First_Sep   : Integer := 0;
      I           : Integer := 0;
      First_Digit : Integer := 0;
      Base_Char   : Integer := 0;
   begin
      if Zero_Count <= 0 then
         return;
      end if;

      --  Find base chars
      Base_Char := Index
        (Source => Text_Value,
         Set    => Maps.To_Set ("#xX"),
         From   => 1,
         Test   => Inside,
         Going  => Forward);

      --  Calc position of a first digit
      if Base_Char > 0 then
         First_Digit := Base_Char + 1;
      elsif Element (Text_Value, 1) = '-' or Element (Text_Value, 1) = '+' then
         First_Digit := 2;
      elsif Element (Text_Value, 1) = ' ' and Fmt.Sign_Style = Space_Show then
         First_Digit := 2;
      else
         First_Digit := 1;
      end if;

      Insert (Text_Value, First_Digit, String'(Zero_Count * '0'));

      if Fmt.Digit_Groups /= None then
         if
           Fmt.Digit_Groups = NLS_Grouping_Style and then Fmt.Base /= 10
         then
            return;
         end if;

         First_Sep := Index (Text_Value, Sep_Char);

         if First_Sep = 0 then
            First_Sep := Length (Text_Value) - Group_Size;
         else
            First_Sep := First_Sep - Group_Size - 1;
         end if;

         Replace_Slice
           (Source => Text_Value,
            Low    => First_Sep,
            High   => First_Sep,
            By     => Sep_Char);

         while First_Sep > Group_Size + 1 loop
            I := First_Sep - Group_Size - 1;

            if I <= First_Digit then
               Overwrite
                 (Source   => Text_Value,
                  Position => First_Digit,
                  New_Item => "0"); --"" & Filler);
               exit;
            else
               Overwrite
                 (Source   => Text_Value,
                  Position => I,
                  New_Item => Sep_Char);
            end if;

            First_Sep := I;
         end loop;
      end if;
   end Set_Zero_Padded;

   ----------------------------------------------------------------------------
   -- Integer number utils
   ----------------------------------------------------------------------------

   -----------------
   -- Int_To_Text --
   -----------------

   procedure Int_To_Text
     (To        : out Unbounded_String;
      Value     : Int;
      Base      : Base_Type;
      Neg_Style : Signed_Number_Represent)
   is
      function To_Unsigned is new Ada.Unchecked_Conversion (Int, Uns);
      procedure To_Text is new Uns_To_Text (Uns);
   begin
      if Value >= 0 then
         To_Text (To, To_Unsigned (Value), Base);

      else
         case Neg_Style is
            when Sign_Magnitude  =>
               To_Text (To, To_Unsigned (abs (Value + 1)) + 1, Base);
            when Twos_Complement =>
               To_Text (To, To_Unsigned (Value), Base);
         end case;

         if Neg_Style = Sign_Magnitude then
            Insert (To, 1, "-");
         end if;
      end if;
   end Int_To_Text;

   -----------------
   -- Uns_To_Text --
   -----------------

   procedure Uns_To_Text
     (To        : out Unbounded_String;
      Value     : Uns;
      Base      : Base_Type)
   is
      Divider   : Uns := 0;
      Quotient  : Uns := 0;
      Remainder : Uns := 0;
   begin
      if Value = 0 then
         To := To_Unbounded_String ("0");
      else
         Quotient := Value;
         Divider := Uns ((Base));

         while Quotient /= 0 loop
            Remainder := Quotient rem Divider;
            Quotient := Quotient / Divider;
            Insert (To, 1, "" & Digit_Symbols (Natural (Remainder) + 1));
         end loop;
      end if;
   end Uns_To_Text;

   --------------------
   -- Format_Integer --
   --------------------

   function Format_Integer
     (Value : Unbounded_String; Fmt_Spec : Format_Data_Record) return String
   is
      Img         : Unbounded_String;
      Min_Dig_Num : Integer := 0;
      First_Digit : Integer := 0;
   begin
      Img := Value;

      --  Find first digit in Img
      case Fmt_Spec.Value_Kind is
         when V_Signed_Integer   =>
            if Element (Img, 1) = '-' then
               First_Digit := 2;
            elsif Fmt_Spec.Sign_Style = Always_Show then
               Insert (Img, 1, "+");
               First_Digit := 2;
            elsif Fmt_Spec.Sign_Style = Space_Show then
               Insert (Img, 1, " ");
               First_Digit := 2;
            else
               First_Digit := 1;
            end if;

         when V_Unsigned_Integer =>
            First_Digit := 1;

         when others             =>
            return To_String (Value);
      end case;

      --  Get a minimum number of digits
      if Fmt_Spec.Precision /= Undefined then
         Min_Dig_Num := Fmt_Spec.Precision;
      end if;

      --  Pad value to minimum number of digits
      if Min_Dig_Num > Length (Img) then
         Insert (Img, First_Digit, String'((Min_Dig_Num - Length (Img)) * '0'));
      end if;

      --  Separate groups of digits
      case Fmt_Spec.Digit_Groups is
         when None =>
            null;
         when Ada_Grouping_Style =>
            Separate_Digit_Groups (Img, Fmt_Spec);
         when NLS_Grouping_Style =>
            if Fmt_Spec.Base = 10 then
               Separate_Digit_Groups (Img, Fmt_Spec);
            end if;
      end case;

      --  Put base
      case Fmt_Spec.Base_Style is
         when None           =>
            null;
         when C_Base_Style   =>
            case Fmt_Spec.Base is
               when 8      => Insert (Img, 1, "0");
               when 16     => Insert (Img, 1, "0X");
               when others => null;
            end case;
         when Ada_Base_Style =>
            case Fmt_Spec.Base is
               when 2      =>
                  Insert (Img, First_Digit, "2#");
                  Append (Img, "#");
               when 8      =>
                  Insert (Img, First_Digit, "8#");
                  Append (Img, "#");
               when 16     =>
                  Insert (Img, First_Digit, "16#");
                  Append (Img, "#");
               when others => null;
            end case;
      end case;

      --  Put leading zeroes
      if Fmt_Spec.Align = Right
        and then Fmt_Spec.Zero_Padded
        and then Fmt_Spec.Precision <= 0
      then
         Set_Zero_Padded (Img, Fmt_Spec.Width - Length (Img), Fmt_Spec);
      end if;

      --  Build a result string
      declare
         T      : String := To_String (Img);
         Buffer : String (1 .. Fmt_Spec.Width);
      begin
         if Fmt_Spec.Letter_Case = Lower_Case then
            T := To_Lower (T);
         end if;

         if T'Length >= Fmt_Spec.Width then
            return T;
         else
            Move
              (Source  => T,
               Target  => Buffer,
               Drop    => Error,
               Justify => Fmt_Spec.Align,
               Pad     => Filler);
            return Buffer;
         end if;
      end;
   end Format_Integer;

   ----------------------------------------------------------------------------
   -- Real number utils
   ----------------------------------------------------------------------------

   function Integer_Part (Number : Long_Long_Float) return Long_Long_Float;
   --  Returns the integer part of a real Number.

   function Fraction_Part (Number : Long_Long_Float) return Long_Long_Float;
   --  Returns the fractional part of a real Number.

   function Int_Part_To_Str
     (Value : Long_Long_Float; Base : Base_Type) return Unbounded_String;

   function Frac_Part_To_Str
     (Value : Long_Long_Float; Base : Base_Type) return Unbounded_String;

   procedure Round_Real_Number
     (Int_Part  : in out Unbounded_String;
      Frac_Part : in out Unbounded_String;
      Precision : Integer;
      Base      : Base_Type);

   ------------------
   -- Integer_Part --
   ------------------

   function Integer_Part (Number : Long_Long_Float) return Long_Long_Float is
   begin
      return Long_Long_Float'Truncation (Number);
   end Integer_Part;

   -------------------
   -- Fraction_Part --
   -------------------

   function Fraction_Part (Number : Long_Long_Float) return Long_Long_Float is
     (Number - Integer_Part (Number));

   ---------------------
   -- Int_Part_To_Str --
   ---------------------

   function Int_Part_To_Str
     (Value : Long_Long_Float; Base : Base_Type) return Unbounded_String
   is
      Result    : Unbounded_String;
      Divider   : Long_Long_Float;
      Quotient  : Long_Long_Float;
      Remainder : Long_Long_Float;
      X         : Long_Long_Float;
   begin
      X := abs Integer_Part (Value);

      if X = 0.0 then
         Result := To_Unbounded_String ("0");
      else
         Quotient := X;
         Divider := Long_Long_Float (Base);

         while Quotient > 0.0 loop
            Quotient := Long_Long_Float'Truncation (X / Divider);
            Remainder := X - Quotient * Divider;
            Result := Digit_Symbols (Natural (Remainder) + 1) & Result;
            X := Quotient;
         end loop;
      end if;

      return Result;
   end Int_Part_To_Str;

   ----------------------
   -- Frac_Part_To_Str --
   ----------------------

   function Frac_Part_To_Str
     (Value : Long_Long_Float; Base : Base_Type) return Unbounded_String
   is
      Result     : Unbounded_String;
      X          : Long_Long_Float;
      Product    : Long_Long_Float;
      Multiplier : Long_Long_Float;
   begin
      X := abs Fraction_Part (Value);

      if X = 0.0 then
         Result := To_Unbounded_String ("0");
      else
         Product := X;
         Multiplier := Long_Long_Float (Base);

         while X /= 0.0 loop
            Product := X * Multiplier;
            Result :=
              Result & Digit_Symbols (Natural (Integer_Part (Product)) + 1);
            X := Fraction_Part (Product);
         end loop;
      end if;

      return Result;
   end Frac_Part_To_Str;

   -----------------------
   -- Round_Real_Number --
   -----------------------

   procedure Round_Real_Number
     (Int_Part  : in out Unbounded_String;
      Frac_Part : in out Unbounded_String;
      Precision : Integer;
      Base      : Base_Type)
   is
      ----------------
      -- Next_Digit --
      ----------------

      function Next_Digit (D : Character) return Character is
         P : Natural;
      begin
         P := Index (Digit_Symbols, Maps.To_Set (D));

         if P = 0 or P >= Base then
            raise Program_Error;
         end if;

         return Digit_Symbols (P + 1);
      end Next_Digit;

      Int_L    : Natural := Length (Int_Part);
      Buffer   : String (1 .. Int_L + Length (Frac_Part) + 1) :=
        (others => ' ');
      Mid_Dig  : constant Character := Digit_Symbols (Base / 2 + 1);
      Last_Dig : constant Character := Digit_Symbols (Base);
      Cursor   : Natural := Int_L + Precision + 1;
      D        : Character;
      Done     : Boolean := False;
      FD       : Natural := Buffer'First + 1;
      LD       : Natural := Buffer'Last;
   begin
      Buffer := ' ' & To_String (Int_Part) & To_String (Frac_Part);

      if Cursor >= LD then
         return;
      elsif Cursor < FD then
         if Precision = 1 and then Buffer (Buffer'First + 1) >= Mid_Dig then
            Buffer (Buffer'First) := '1';
         else
            Buffer (Buffer'First) := '0';
         end if;

         FD := 1;
      else
         LD := Cursor;

         if Buffer (Cursor + 1) >= Mid_Dig then
            while Buffer (Cursor) = Last_Dig loop
               Buffer (Cursor) := '0';
               Cursor := Cursor - 1;
            end loop;

            if Buffer (Cursor) >= '0' and Buffer (Cursor) < Last_Dig then
               Buffer (Cursor) := Next_Digit (Buffer (Cursor));
            else
               Buffer (Cursor) := '1';
               FD := 1;
            end if;
         end if;
      end if;

      Int_Part := To_Unbounded_String (Buffer (FD .. Int_L + 1));
      Frac_Part := To_Unbounded_String
        (if Precision > 0 then Buffer (Int_L + 2 .. LD) else "");
   end Round_Real_Number;

   ------------------
   -- Real_To_Text --
   ------------------

   procedure Real_To_Text
     (To    : out Real_Image;
      Value : Long_Long_Float;
      Fmt   : in out Format_Data_Record)
   is
      package F_IO is new Ada.Text_IO.Float_IO (Long_Long_Float);

      X   : Long_Long_Float := 0.0;
      B   : Long_Long_Float := Long_Long_Float (Fmt.Base);
      Exp : Integer := 0;
      Prc : Integer := 0;
   begin
      if Value = 0.0 then
         To := (To_Unbounded_String ("0"),
                To_Unbounded_String ("0"),
                False,
                0);
      else
         X := abs (Value);

         --  Handle format for g, G specifiers
         if Fmt.G_Spec_Used then
            --  Compute exponent
            declare
               S     : String (1 .. 50);
               E_Pos : Natural := 0;
            begin
               F_IO.Put (To => S, Item => X, Aft => 1, Exp => 1);
               E_Pos := Ada.Strings.Fixed.Index (S, "E");

               if E_Pos = 0 then
                  Exp := 0;
               else
                  Exp := Integer'Value (S (E_Pos + 1 .. S'Last));
               end if;
            end;

            --  Compute precision and notation
            if Fmt.Precision = Undefined then
               Prc := 6;
            elsif Fmt.Precision = 0 then
               Prc := 1;
            else
               Prc := Fmt.Precision;
            end if;

            if Prc > Exp and Exp >= -4 then
               Fmt.Precision := Prc - 1 - Exp;
               Fmt.Width_Exp := 0;
               Fmt.Notation := Decimal_Notation;
            else
               Fmt.Precision := Prc - 1;
               Fmt.Width_Exp := 3;
               Fmt.Notation := E_Notation;
            end if;
         end if;

         if Fmt.Base = 10 then
            declare
               Img   : String (1 .. 5200);
               F_Dig : Natural := 0;
               L_Dig : Natural := 0;
               D_Pos : Natural := 0;
               E_Pos : Natural := 0;
            begin
               F_IO.Put
                 (To   => Img,
                  Item => X,
                  Aft  => (if Fmt.Precision = Undefined then F_IO.Default_Aft
                           else Fmt.Precision),
                  Exp  => Fmt.Width_Exp);
               E_Pos := Ada.Strings.Fixed.Index (Img, "E");

               if E_Pos = 0 then
                  Exp := 0;
                  L_Dig := Img'Last;
               else
                  Exp := Integer'Value (Img (E_Pos + 1 .. Img'Last));
                  L_Dig := E_Pos - 1;
               end if;

               F_Dig := Ada.Strings.Fixed.Index_Non_Blank (Img);

               if Img (F_Dig) = '-' then
                  F_Dig := F_Dig + 1;
               end if;

               D_Pos := Ada.Strings.Fixed.Index (Img, ".");
               To.Int_Part := To_Unbounded_String (Img (F_Dig .. D_Pos - 1));
               To.Frac_Part := To_Unbounded_String (Img (D_Pos + 1 .. L_Dig));
            end;
         else
            if Fmt.Notation in E_Notation .. P_Notation then
               Exp := 0;
               --  Set Base as 2 for C style P_Notation
               if Fmt.Base = 16 and then Fmt.Notation = P_Notation then
                  B := 2.0;
               end if;

               if X >= 1.0 then
                  while X >= B loop
                     X := X / B;
                     Exp := Exp + 1;
                  end loop;
               else
                  while X <= 1.0 loop
                     X := X * B;
                     Exp := Exp - 1;
                  end loop;
               end if;
            end if;

            To.Int_Part := Int_Part_To_Str (X, Fmt.Base);
            To.Frac_Part := Frac_Part_To_Str (X, Fmt.Base);
         end if;

         To.Is_Negative := Value < 0.0;
         To.Exp := Exp;
      end if;
   end Real_To_Text;

   -----------------
   -- Format_Real --
   -----------------

   function Format_Real
     (Value    : Real_Image;
      Fmt_Spec : Format_Data_Record) return String
   is
      Real_Width  : Integer := 0;
      Width_After : Integer := Fmt_Spec.Precision;
      Dec_Point   : String :=
        (if Fmt_Spec.Digit_Groups = Ada_Grouping_Style
         then Ada_Dec_Point_Character
         else Decimal_Point_Character);
      Int_Part    : Unbounded_String := Value.Int_Part;
      Frac_Part   : Unbounded_String := Value.Frac_Part;
   begin
      --  Value rounding if need
      if Length (Frac_Part) > Width_After then
         Round_Real_Number
           (Int_Part  => Int_Part,
            Frac_Part => Frac_Part,
            Precision => Width_After,
            Base      => Fmt_Spec.Base);
      end if;

      --  Separate groups of digits
      if Fmt_Spec.Digit_Groups /= None then
         Separate_Digit_Groups (Int_Part, Fmt_Spec);
      end if;

      --  Put/strip trailing zeroes
      if
        Width_After > Length (Frac_Part)
        and then not Fmt_Spec.Strip_Trailing_Zeroes
      then
         Append
           (Frac_Part, String'((Width_After - Length (Frac_Part)) * '0'));
      elsif Width_After = 0 then
         Frac_Part := Null_Unbounded_String;
      elsif Fmt_Spec.Strip_Trailing_Zeroes then
         declare
            L : Natural := Length (Frac_Part);
         begin
            while L > 0 and then Element (Frac_Part, L) = '0' loop
               L := L - 1;
            end loop;

            if L < Length (Frac_Part) then
               Delete (Frac_Part, L + 1, Length (Frac_Part));
            end if;
         end;
      end if;

      --  Put base
      case Fmt_Spec.Base_Style is
         when None           =>
            if Fmt_Spec.Notation = E_Notation then
               Append
                 (Frac_Part, -(+"E%+0*d" & Fmt_Spec.Width_Exp & Value.Exp));
            end if;
         when C_Base_Style   =>
            if Fmt_Spec.Base = 16 then
               Insert (Int_Part, 1, "0X");
               Append (Frac_Part, -(+"P%+d" & Value.Exp));
            end if;
         when Ada_Base_Style =>
            if Fmt_Spec.Base = 16 then
               Insert (Int_Part, 1, "16#");

               if Fmt_Spec.Notation = Decimal_Notation then
                  Append (Frac_Part, "#");
               else
                  Append
                    (Frac_Part, -(+"#E%+0*d" & Fmt_Spec.Width_Exp & Value.Exp));
               end if;
            end if;
      end case;

      --  Put sign
      if Value.Is_Negative then
         Insert (Int_Part, 1, "-");
      else
         if Fmt_Spec.Sign_Style = Always_Show then
            Insert (Int_Part, 1, "+");
         elsif Fmt_Spec.Sign_Style = Space_Show then
            Insert (Int_Part, 1, " ");
         end if;
      end if;

      --  Put leading zeroes
      if Fmt_Spec.Align = Right and then Fmt_Spec.Zero_Padded then
         Set_Zero_Padded
           (Int_Part,
            Fmt_Spec.Width - Length (Int_Part) - Length (Frac_Part)
            - Dec_Point'Length,
            Fmt_Spec);
      end if;

      --  Set decimal point
      if Width_After > 0 and then Length (Frac_Part) > 0 then
         Insert (Frac_Part, 1, Dec_Point);
      end if;

      --  Get a length of the result string
      Real_Width := Integer'Max
        (Fmt_Spec.Width, Length (Int_Part) + Length (Frac_Part));

      --  Build a result string
      declare
         T      : String := To_String (Int_Part & Frac_Part);
         Buffer : String (1 .. Real_Width);
      begin
         if Fmt_Spec.Letter_Case = Lower_Case then
            T := To_Lower (T);
         end if;

         if T'Length > Real_Width then
            return T;
         else
            Move
              (Source  => T,
               Target  => Buffer,
               Drop    => Error,
               Justify => Fmt_Spec.Align,
               Pad     => Filler);
            return Buffer;
         end if;
      end;
   end Format_Real;

end Formatted_Output.Utils;
