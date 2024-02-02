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

with Ada.Text_IO;
with Ada.Calendar;
with Interfaces;                           use Interfaces;
with System;

with Formatted_Output;                     use Formatted_Output;
with Formatted_Output.Integer_Output;
with Formatted_Output.Decimal_Output;
with Formatted_Output.Modular_Output;
with Formatted_Output_Integer;             use Formatted_Output_Integer;
with Formatted_Output_Long_Integer;        use Formatted_Output_Long_Integer;
with Formatted_Output_Long_Long_Integer;   use Formatted_Output_Long_Long_Integer;
with Formatted_Output_Float;               use Formatted_Output_Float;
with Formatted_Output_Long_Float;          use Formatted_Output_Long_Float;
with Formatted_Output_Long_Long_Float;     use Formatted_Output_Long_Long_Float;
with Formatted_Output_Short_Integer;       use Formatted_Output_Short_Integer;
with Formatted_Output_Short_Short_Integer; use Formatted_Output_Short_Short_Integer;
with Formatted_Output.Enumeration_Output;
with Formatted_Output.Time_Output;         use Formatted_Output.Time_Output;
with Formatted_Output.Address_Output;      use Formatted_Output.Address_Output;

with L10n; use L10n;

procedure Formatted_Output_Demo is

   procedure Show_Integer_Ranges;
   procedure Show_Unsigned_Ranges;
   procedure Show_Float_Ranges;
   procedure Show_Decimal_Ranges;
   procedure Show_Enum_Style;

   -------------------------
   -- Show_Integer_Ranges --
   -------------------------

   procedure Show_Integer_Ranges is
      type Longest_Integer is range System.Min_Int .. System.Max_Int;
      package LI_FO is new Formatted_Output.Integer_Output (Longest_Integer);
      use LI_FO;
   begin
      Put_Line (+"%^80s" & "-- Integer type ranges --");
      Put_Line
        (+"Short_Short_Integer type range:\n\t%+_30d .. %-+_30d"
         & Short_Short_Integer'First & Short_Short_Integer'Last);
      Put_Line
        (+"Short_Integer type range:\n\t%+_30d .. %-+_30d"
         & Short_Integer'First & Short_Integer'Last);
      Put_Line
        (+"Integer type range:\n\t%+_30d .. %-+_30d"
         & Integer'First & Integer'Last);
      Put_Line
        (+"Long_Integer type range:\n\t%+_30d .. %-+_30d"
         & Long_Integer'First & Long_Integer'Last);
      Put_Line
        (+"Long_Long_Integer type range:\n\t%+_30d .. %-+_30d"
         & Long_Long_Integer'First & Long_Long_Integer'Last);
      Put_Line
        (+"Natural type range:\n\t%+_30d .. %-+_30d"
         & Natural'First & Natural'Last);
      Put_Line
        (+"Positive type range:\n\t%+_30d .. %-+_30d"
         & Positive'First & Positive'Last);
      Put_Line
        (+"Widest integer type range for your system:\n\t%+_30d .. %-+_30d"
         & Longest_Integer'First & Longest_Integer'Last);
   end Show_Integer_Ranges;

   --------------------------
   -- Show_Unsigned_Ranges --
   --------------------------

   procedure Show_Unsigned_Ranges is
      package Unsigned_Output is
        new Formatted_Output.Modular_Output (Interfaces.Unsigned_64);
      use Unsigned_Output;

      type Longest_Unsigned is mod System.Max_Binary_Modulus;
      package LU_FO is new Formatted_Output.Modular_Output (Longest_Unsigned);
      use LU_FO;
   begin
      Put_Line (+"%^80s" & "-- Unsigned type example --");
      Put_Line
        (+"Unsigned_64 type range:\n\t%_30u .. %-_30u"
         & Unsigned_64'First & Unsigned_64'Last);
      Put_Line
        (+"Widest unsigned type range for your system:\n\t%_30u .. %-_30u"
         & Longest_Unsigned'First & Longest_Unsigned'Last);
   end Show_Unsigned_Ranges;

   -----------------------
   -- Show_Float_Ranges --
   -----------------------

   procedure Show_Float_Ranges is
   begin
      Put_Line (+"%^80s" & "-- Float type ranges --");
      Put_Line
        (+"Float type range:\n\t%+_35.20e .. %-+_35.20e"
         & Float'First & Float'Last);
      Put_Line
        (+"Long_Float type range:\n\t%+_35.20e .. %-+_35.20e"
         & Long_Float'First & Long_Float'Last);
      Put_Line
        (+"Long_Long_Float type range:\n\t%+_35.20e .. %-+_35.20e"
         & Long_Long_Float'First & Long_Long_Float'Last);
   end Show_Float_Ranges;

   -------------------------
   -- Show_Decimal_Ranges --
   -------------------------

   procedure Show_Decimal_Ranges is
      type Money is delta 0.01 digits 17;
      package Money_Output is new Formatted_Output.Decimal_Output (Money);
      use Money_Output;

      L : constant String := Get_Locale;
      D : constant Integer := Money'Digits;
      S : constant Money := 1.0 / (10 ** Money'Scale);
   begin
      Put_Line (+"%^80s" & "-- Decimal type example --");
      Set_Locale (Locale => "POSIX");
      Put_Line
        (+"Money type range (type Money is delta %f digits %d):\n\t%+_30f .. %-+_30f"
         & S & D & Money'First & Money'Last);
      Set_Locale (Locale => L);
   end Show_Decimal_Ranges;
   ---------------------
   -- Show_Enum_Style --
   ---------------------

   procedure Show_Enum_Style is
      package File_Mode_Output is
        new Formatted_Output.Enumeration_Output (Ada.Text_IO.File_Mode);
      use File_Mode_Output;
      use Ada.Text_IO;
   begin
      Put_Line (+"%^80s" & "-- Enumeration type example --");
      Put_Line
        (+"%s"
         & "Ada.Text_IO.File_Mode enumeration type (in different styles):");
      Put_Line
        (+"%11s => %t, %t, %t" & "Capitalized" & In_File & Out_File & Append_File);
      Put_Line
        (+"%11s => %l, %l, %l" & "Lower Case " & In_File & Out_File & Append_File);
      Put_Line
        (+"%11s => %U, %U, %U" & "Upper Case " & In_File & Out_File & Append_File);
      Put_Line
        (+"%11s => %m, %m, %m" & "Mixed Case " & In_File & Out_File & Append_File);
   end Show_Enum_Style;

   Str : constant String := "Hello, world!";
   T   : constant Ada.Calendar.Time := Ada.Calendar.Clock;

begin
   Set_Locale;

   Put_Line (+"%^80s" & "-- String and Character example --");
   Put_Line (+"String is ""%s""" & Str);
   Put_Line (+"First char is '%c'" & Str (Str'First));
   Ada.Text_IO.New_Line;

   Show_Integer_Ranges;
   Ada.Text_IO.New_Line;

   Show_Unsigned_Ranges;
   Ada.Text_IO.New_Line;

   Show_Float_Ranges;
   Ada.Text_IO.New_Line;

   Show_Decimal_Ranges;
   Ada.Text_IO.New_Line;

   Show_Enum_Style;
   Ada.Text_IO.New_Line;

   Put_Line (+"%^80s" & "-- Address type example --");
   Put_Line (+"%p" & T'Address);
   Put_Line (+"%~_p" & T'Address);
   Put_Line (+"%#p" & T'Address);
   Ada.Text_IO.New_Line;

   Put_Line (+"%^80s" & "-- Time output examples --");
   Set_Locale;
   Put_Line
     (+"Current Time (Locale: %10s) = %s"
      & Get_Locale & Format_Time ("'%c'", T));
   Set_Locale (Locale => "POSIX");
   Put_Line
     (+"Current Time (Locale: %10s) = %s"
      & Get_Locale & Format_Time ("'%c'", T));
   Set_Locale (Locale => "uk_UA.utf8");
   Put_Line
     (+"Current Time (Locale: %10s) = %s"
      & Get_Locale & Format_Time ("'%c'", T));
end Formatted_Output_Demo;
