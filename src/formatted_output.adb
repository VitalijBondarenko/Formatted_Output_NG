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

with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;    use Ada.Float_Text_IO;
with Ada.Strings;          use Ada.Strings;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
with Ada.Strings.Maps;     use Ada.Strings.Maps;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with L10n.Localeinfo;      use L10n.Localeinfo;

package body Formatted_Output is

   ---------------
   -- To_Format --
   ---------------

   function To_Format (Fmt_String : String) return Format_Type is
   begin
      return To_Unbounded_String (Fmt_String);
   end To_Format;

   ---------------
   -- To_String --
   ---------------

   function To_String (Fmt : Format_Type) return String is
      CR       : constant String (1 .. 1) := (1 => ASCII.CR);
      LF       : constant String (1 .. 1) := (1 => ASCII.LF);
      BS       : constant String (1 .. 1) := (1 => ASCII.BS);
      HT       : constant String (1 .. 1) := (1 => ASCII.HT);
      FF       : constant String (1 .. 1) := (1 => ASCII.FF);
      I        : Integer := 1;
      Fmt_Copy : Unbounded_String := Unbounded_String (Fmt);
   begin
      while I < Length (Fmt_Copy) loop
         if Element (Fmt_Copy, I) = '\' then
            case Element (Fmt_Copy, I + 1) is
               when 'n'    => 
                  Replace_Slice (Fmt_Copy, I, I + 1, LF);
                  --  I := I + 1;
                  --  Uncomment line above, if your system using two-byte
                  --  representation of the next line character. Example of
                  --  such system is EZ2LOAD.
               when 'r'    => 
                  Replace_Slice (Fmt_Copy, I, I + 1, CR);
               when 'b'    => 
                  Replace_Slice (Fmt_Copy, I, I + 1, BS);
               when 't'    => 
                  Replace_Slice (Fmt_Copy, I, I + 1, HT);
               when 'f'    => 
                  Replace_Slice (Fmt_Copy, I, I + 1, FF);
               when '\'    => 
                  Delete (Fmt_Copy, I, I);
               when others => 
                  null;
            end case;
         elsif Element (Fmt_Copy, I) = '%' then
            case Element (Fmt_Copy, I + 1) is
               when '%'    =>
                  Delete (Fmt_Copy, I, I);
               when others =>
                  raise Format_Error;
            end case;
         end if;
         
         I := I + 1;
      end loop;
      
      return To_String (Fmt_Copy);
   end To_String;

   -------------------
   -- Format_String --
   -------------------

   function Format_String
     (Value         : String;
      Initial_Width : Integer;
      Justification : Alignment) return String
   is
      Width : Integer;
   begin
      if Initial_Width < Value'Length then
         Width := Value'Length;
      else
         Width := Initial_Width;
      end if;
      
      declare
         S : String (1 .. Width);
      begin
         Move (Value, S, Justify => Justification, Pad => Filler);
         return S;
      end;
   end Format_String;

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : String) return Format_Type is
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
               when 's'             =>
                  Replace_Slice
                    (Fmt_Copy, Command_Start, I,
                     Format_String (Value, Width, Justification));
                  return Format_Type (Fmt_Copy);
                  
               when '-' | '+' | '*' =>
                  if Justification_Changed or else Digit_Occured then
                     raise Format_Error;
                  end if;
                  
                  Justification_Changed := True;
                  
                  case Element (Fmt_Copy, I) is
                     when '-'    => 
                        Justification := Left;
                     when '+'    => 
                        Justification := Right;
                     when '*'    => 
                        Justification := Center;
                     when others => 
                        null;
                  end case;
                  
               when '0' .. '9'      =>
                  Digit_Occured := True;
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

   ---------
   -- Put --
   ---------

   procedure Put (Fmt : Format_Type) is
   begin
      Put (To_String (Fmt));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (File : File_Type; Fmt : Format_Type) is
   begin
      Put (File, To_String (Fmt));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Fmt : Format_Type) is
   begin
      Put_Line (To_String (Fmt));
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : File_Type; Fmt : Format_Type) is
   begin
      Put_Line (File, To_String (Fmt));
   end Put_Line;

   --------------------------
   -- Scan_To_Percent_Sign --
   --------------------------

   function Scan_To_Percent_Sign (Fmt : Format_Type) return Integer is
      I : Natural := 1;
   begin
      while I < Length (Fmt) loop
         if Element (Fmt, I) = '%' then
            if Element (Fmt, I + 1) /= '%' then
               return I;
            else
               I := I + 1;
            end if;
         end if;
         
         I := I + 1;
      end loop;
      
      return 0;
   end Scan_To_Percent_Sign;

   -----------------------------
   -- Decimal_Point_Character --
   -----------------------------

   function Decimal_Point_Character return String is
      Lconv : C_Lconv_Access := C_Localeconv;
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
      Lconv : C_Lconv_Access := C_Localeconv;
   begin
      if Lconv.Thousands_Sep = Null_Ptr then
         return "";
      else
         declare
            S : String := Value (Lconv.Thousands_Sep);
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
   
   function Separate_Digit_Groups
     (Text_Value : String;
      Separator  : String;
      Group_Size : Integer) return String
   is
      Tmp : Unbounded_String := Null_Unbounded_String;
      I   : Integer;
      J   : Integer := Text_Value'Last;
   begin
      if Separator'Length = 0 then
         return Text_Value;
      end if;

      while J >= Text_Value'First loop
         I := J - Group_Size + 1;
         
         if I <= Text_Value'First then
            Insert (Tmp, 1, Text_Value (Text_Value'First .. J));
            exit;
         else
            Insert (Tmp, 1, Separator & Text_Value (I .. J));
         end if;

         J := J - Group_Size;
      end loop;
      
      return To_String (Tmp);

   exception
      when others => return "";
   end Separate_Digit_Groups;

   --  ---------------------------------
   --  -- Separate_Based_Digit_Groups --
   --  ---------------------------------
   --  
   --  function Separate_Based_Digit_Groups
   --    (Text_Value : String;
   --     Separator  : String;
   --     Group_Size : Integer) return String
   --  is
   --     Tmp : Unbounded_String := Null_Unbounded_String;
   --     NS1 : Natural := Index (Text_Value, "#", Text_Value'First);
   --     NS2 : Natural;
   --  begin
   --     if NS1 > 0 then
   --        NS2 := Index (Text_Value, "#", NS1 + 1);
   --     else
   --        return "";
   --     end if;
   --  
   --     declare
   --        TS : String := Text_Value (NS1 + 1 .. NS2 - 1);
   --        I  : Integer;
   --        J  : Integer := TS'Last;
   --     begin
   --        while J >= TS'First loop
   --           I := J - Group_Size + 1;
   --  
   --           if I <= TS'First then
   --              Insert (Tmp, 1, TS (TS'First .. J));
   --              exit;
   --           end if;
   --  
   --           Insert (Tmp, 1, Separator & TS (I .. J));
   --           J := J - Group_Size;
   --        end loop;
   --     end;
   --  
   --     Tmp := Text_Value (Text_Value'First .. NS1) & Tmp;
   --     Tmp := Tmp & Text_Value (NS2 .. Text_Value'Last);
   --     return To_String (Tmp);
   --  
   --  exception
   --     when others => return "";
   --  end Separate_Based_Digit_Groups;

   ----------------------
   -- Set_Leading_Zero --
   ----------------------
   
   function Set_Leading_Zero (Img : String) return String is
      S   : String := Img;
      FD  : Natural := 0;
      Cur : Natural := 0;
      NS1 : Natural := 0;
   begin
      FD := Index_Non_Blank (Img, Forward);

      if Img (FD) = '-' or else Img (FD) = '+' then
         S (1) := Img (FD);
         Cur := 1;
         FD := FD + 1;
      end if;

      --  check using base
      NS1 := Index
        (Source => Img,
         Set    => To_Set ("#xX"),
         From   => FD,
         Test   => Inside,
         Going  => Forward);
      
      if NS1 > 0 then
         for I in FD .. NS1 loop
            Cur := Cur + 1;
            S (Cur) := Img (I);
         end loop;

         for I in Cur + 1 .. NS1 loop
            S (I) := '0';
         end loop;
      else
         for I in Cur + 1 .. FD - 1 loop
            S (I) := '0';
         end loop;
      end if;
      
      return S;
   end Set_Leading_Zero;

   ----------------------
   -- Set_Leading_Zero --
   ----------------------
   
   function Set_Leading_Zero
     (Img : String; Separator : String; Group_Size : Integer) return String
   is
      S   : String := Img;
      FD  : Natural := Index_Non_Blank (Img, Forward);
      Cur : Natural := 0;
      NS1 : Natural := 0;
      NS2 : Natural := 0;
      I   : Integer := 0;
      J   : Integer := 0;
   begin
      if Separator'Length = 0 then
         return Set_Leading_Zero (Img);
      end if;
      
      FD := Index_Non_Blank (Img, Forward);
      
      if Img (FD) = '-' or else Img (FD) = '+' then
         S (1) := Img (FD);
         Cur := 1;
         FD := FD + 1;
      end if;

      --  check using base
      NS1 := Index
        (Source => Img,
         Set    => To_Set ("#xX"),
         From   => FD,
         Test   => Inside,
         Going  => Forward);
      NS2 := Index (Img, Separator, FD);

      if NS1 > 0 then
         for I in FD .. NS1 loop
            Cur := Cur + 1;
            S (Cur) := Img (I);
         end loop;
         
         I := Cur + 1;
         J := NS1;
         
         if NS2 = 0 then
            NS2 := Img'Length - Group_Size - 1;
         end if;
      else
         I := Cur + 1;
         J := FD - 1;
         
         if NS2 = 0 then
            NS2 := Img'Length - Group_Size;
         end if;
      end if;
      
      for P in I .. J loop
         S (P) := '0';
      end loop;
      
      S (NS2) := Separator (Separator'First);
      J := NS2;
      
      while J >= Cur + 1 loop
         I := J - Group_Size - 1;
         
         if I >= Cur + 1 then
            S (I) := Separator (Separator'First);
         end if;
         
         J := I;
      end loop;

      if NS1 = 0 and then S (Cur + 1) = Separator (Separator'First) then
         if Cur = 0 then
            S (1) := Filler;
         else         
            S (Cur + 1) := S (Cur);
            S (Cur) := Filler;
         end if;
      elsif NS1 > 0 and then S (Cur + 1) = Separator (Separator'First) then
         for P in reverse 2 .. Cur + 1 loop
            S (P) := S (P - 1);
         end loop;
         
         S (1) := Filler;
      end if;
      
      return S;
   end Set_Leading_Zero;

end Formatted_Output;
