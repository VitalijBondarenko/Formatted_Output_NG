------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2016 Vitalij Bondarenko <vibondare@gmail.com>              --
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
--
--  This is a fork of the GNAT.Calendar.Time_IO package with modifications for
--  NLS support using.

with GNAT.Calendar;           use GNAT.Calendar;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling;

with L10n.Langinfo;           use L10n.Langinfo;

package body Formatted_Output.Time_Output is

   Day_Abbr_Names : constant array (0 .. 6) of Locale_Item :=
     (ABDAY_1, ABDAY_2, ABDAY_3, ABDAY_4, ABDAY_5, ABDAY_6, ABDAY_7);

   Day_Full_Names : constant array (0 .. 6) of Locale_Item :=
     (DAY_1, DAY_2, DAY_3, DAY_4, DAY_5, DAY_6, DAY_7);

   Month_Abbr_Names : constant array (Month_Number) of Locale_Item :=
     (ABMON_1, ABMON_2, ABMON_3, ABMON_4, ABMON_5, ABMON_6,
      ABMON_7, ABMON_8, ABMON_9, ABMON_10, ABMON_11, ABMON_12);

   Month_Full_Names : constant array (Month_Number) of Locale_Item :=
     (MON_1, MON_2, MON_3, MON_4, MON_5, MON_6,
      MON_7, MON_8, MON_9, MON_10, MON_11, MON_12);

   type Padding_Mode is (None, Zero, Space);

   type Sec_Number is mod 2 ** 64;
   --  Type used to compute the number of seconds since 1970-01-01.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Am_Pm (H : Natural) return String;
   --  Return AM or PM depending on the hour H or empty string if not defined
   --  in locale.

   function Hour_12 (H : Natural) return Positive;
   --  Convert a 1-24h format to a 0-12 hour format.

   function Image (Str : String; Length : Natural := 0) return String;
   --  Return Str capitalized and cut to length number of characters. If
   --  length is 0, then no cut operation is performed.

   function Image
     (N       : Sec_Number;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String;
   --  Return image of N. This number is eventually padded with zeros or spaces
   --  depending of the length required. If length is 0 then no padding occurs.

   function Image
     (N       : Natural;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String;
   --  As above with N provided in Integer format

   function Julian_Day
     (Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number) return Integer;
   --  Finding Julian day number

   function Julian_Day (Date : Time) return Integer;
   --  Finding Julian day number

   function Day_Abbr_Name (Date : Time) return String;
   --  Returns locale's abbreviated weekday name.

   function Day_Full_Name (Date : Time) return String;
   --  Returns locale's full weekday name.

   function Mon_Abbr_Name (Month : Month_Number) return String;
   function Mon_Abbr_Name (Date : Time) return String;
   --  Returns locale's abbreviated month name.

   function Mon_Full_Name (Month : Month_Number) return String;
   function Mon_Full_Name (Date : Time) return String;
   --  Returns locale's full month name.

   -----------
   -- Am_Pm --
   -----------

   function Am_Pm (H : Natural) return String is
   begin
      if H = 0 or else H > 12 then
         return Nl_Langinfo (PM_STR);
      else
         return Nl_Langinfo (AM_STR);
      end if;
   end Am_Pm;

   -------------
   -- Hour_12 --
   -------------

   function Hour_12 (H : Natural) return Positive is
   begin
      if H = 0 then
         return 12;
      elsif H <= 12 then
         return H;
      else
         return H - 12;
      end if;
   end Hour_12;

   -----------
   -- Image --
   -----------

   function Image
     (Str    : String;
      Length : Natural := 0) return String
   is
      Local : constant String :=
        Ada.Characters.Handling.To_Upper (Str (Str'First)) &
        Ada.Characters.Handling.To_Lower (Str (Str'First + 1 .. Str'Last));
   begin
      if Length = 0 then
         return Local;
      else
         return Local (1 .. Length);
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (N       : Natural;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String
   is
   begin
      return Image (Sec_Number (N), Padding, Length);
   end Image;

   function Image
     (N       : Sec_Number;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String
   is
      function Pad_Char return String is
      begin
         case Padding is
            when None  => return "";
            when Zero  => return "00";
            when Space => return "  ";
         end case;
      end Pad_Char;

      --  Local Declarations
      NI  : constant String := Sec_Number'Image (N);
      NIP : constant String := Pad_Char & NI (2 .. NI'Last);
   begin
      if Length = 0 or else Padding = None then
         return NI (2 .. NI'Last);
      else
         return NIP (NIP'Last - Length + 1 .. NIP'Last);
      end if;
   end Image;

   ----------------
   -- Julian_Day --
   ----------------

   function Julian_Day
     (Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number) return Integer
   is
      IY : Integer := Integer (Year);
      IM : Integer := Integer (Month);
      ID : Integer := Integer (Day);
      A  : Integer := (14 - IM) / 12;
      Y  : Integer := IY + 4800 - A;
      M  : Integer := IM + 12 * A - 3;
   begin
      return
        ID + (153 * M + 2) / 5 + 365 * Y + Y / 4 - Y / 100 + Y / 400 - 32045;
   end Julian_Day;

   function Julian_Day (Date : Time) return Integer is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Sec   : Day_Duration;
      pragma Unreferenced (Sec);
   begin
      Split (Date, Year, Month, Day, Sec);
      return Julian_Day (Year, Month, Day);
   end Julian_Day;

   -------------------
   -- Day_Abbr_Name --
   -------------------

   function Day_Abbr_Name (Date : Time) return String is
   begin
      return Nl_Langinfo (Day_Abbr_Names ((Julian_Day (Date) + 1) mod 7));
   end Day_Abbr_Name;

   -------------------
   -- Day_Full_Name --
   -------------------

   function Day_Full_Name (Date : Time) return String is
   begin
      return Nl_Langinfo (Day_Full_Names ((Julian_Day (Date) + 1) mod 7));
   end Day_Full_Name;

   -------------------
   -- Mon_Abbr_Name --
   -------------------

   function Mon_Abbr_Name (Month : Month_Number) return String is
   begin
      return Nl_Langinfo (Month_Abbr_Names (Month));
   end Mon_Abbr_Name;

   function Mon_Abbr_Name (Date : Time) return String is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Sec   : Day_Duration;
      pragma Unreferenced (Year, Day, Sec);
   begin
      Split (Date, Year, Month, Day, Sec);
      return Mon_Abbr_Name (Month);
   end Mon_Abbr_Name;

   -------------------
   -- Mon_Full_Name --
   -------------------

   function Mon_Full_Name (Month : Month_Number) return String is
   begin
      return Nl_Langinfo (Month_Full_Names (Month));
   end Mon_Full_Name;

   function Mon_Full_Name (Date : Time) return String is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Sec   : Day_Duration;
      pragma Unreferenced (Year, Day, Sec);
   begin
      Split (Date, Year, Month, Day, Sec);
      return Mon_Full_Name (Month);
   end Mon_Full_Name;

   ------------
   -- Format --
   ------------

   function Format
     (Picture : String;
      Date    : Ada.Calendar.Time) return String
   is
      Padding    : Padding_Mode := Zero;
      --  Padding is set for one directive
      Result     : Unbounded_String;
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;
      P          : Positive;
   begin
      --  Get current time in split format
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);

      --  Null picture string is error
      if Picture = "" then
         raise Time_Picture_Error with "null picture string";
      end if;

      --  Loop through characters of picture string, building result
      Result := Null_Unbounded_String;
      P := Picture'First;

      while P <= Picture'Last loop

         --  A directive has the following format "%[-_]."
         if Picture (P) = '%' then
            Padding := Zero;

            if P = Picture'Last then
               raise Time_Picture_Error with "picture string ends with '%";
            end if;

            --  Check for GNU extension to change the padding
            if Picture (P + 1) = '-' then
               Padding := None;
               P := P + 1;
            elsif Picture (P + 1) = '_' then
               Padding := Space;
               P := P + 1;
            end if;

            if P = Picture'Last then
               raise Time_Picture_Error
                 with "picture string ends with '- or '_";
            end if;

            case Picture (P + 1) is

               --  Literal %
               when '%' =>
                  Result := Result & '%';

                  --  A newline
               when 'n' =>
                  Result := Result & ASCII.LF;

                  --  A horizontal tab
               when 't' =>
                  Result := Result & ASCII.HT;

                  --  Hour (00..23)
               when 'H' =>
                  Result := Result & Image (Hour, Padding, 2);

                  --  Hour (01..12)
               when 'I' =>
                  Result := Result & Image (Hour_12 (Hour), Padding, 2);

                  --  Hour ( 0..23)
               when 'k' =>
                  Result := Result & Image (Hour, Space, 2);

                  --  Hour ( 1..12)
               when 'l' =>
                  Result := Result & Image (Hour_12 (Hour), Space, 2);

                  --  Minute (00..59)
               when 'M' =>
                  Result := Result & Image (Minute, Padding, 2);

                  --  AM/PM
               when 'p' =>
                  Result := Result & Am_Pm (Hour);

                  --  am/pm lower case
               when 'P' =>
                  Result := Result &
                    Ada.Characters.Handling.To_Lower (Am_Pm (Hour));

                  --  Time, 12-hour (hh:mm:ss [AP]M)
               when 'r' =>
                  begin
                     Result := Result &
                       Format (Nl_Langinfo (T_FMT_AMPM), Date);
                  exception
                     when others =>
                        Result := Result & Format ("%I:%M:%S", Date);
                  end;

                  --  Time, 24-hour (hh:mm)
               when 'R' =>
                  Result := Result & Format ("%H:%M", Date);

                  --   Seconds since 1970-01-01  00:00:00 UTC
                  --   (a nonstandard extension)
               when 's' =>
                  declare
                     --  Compute the number of seconds using Ada.Calendar.Time
                     --  values rather than Julian days to account for Daylight
                     --  Savings Time.
                     Neg : Boolean  := False;
                     Sec : Duration := Date - Time_Of (1970, 1, 1, 0.0);
                  begin
                     --  Avoid rounding errors and perform special processing
                     --  for dates earlier than the Unix Epoc.
                     if Sec > 0.0 then
                        Sec := Sec - 0.5;
                     elsif Sec < 0.0 then
                        Neg := True;
                        Sec := abs (Sec + 0.5);
                     end if;

                     --  Prepend a minus sign to the result since Sec_Number
                     --  cannot handle negative numbers.
                     if Neg then
                        Result := Result & "-" & Image (Sec_Number (Sec), None);
                     else
                        Result := Result & Image (Sec_Number (Sec), None);
                     end if;
                  end;

                  --  Second (00..59)
               when 'S' =>
                  Result := Result & Image (Second, Padding, 2);

               --  Milliseconds (3 digits)
               --  Microseconds (6 digits)
               --  Nanoseconds  (9 digits)
               when 'i' | 'o' | 'N' =>
                  declare
                     Sub_Sec : constant Long_Integer :=
                                 Long_Integer (Sub_Second * 1_000_000_000);

                     Img1  : constant String := Sub_Sec'Img;
                     Img2  : constant String :=
                               "00000000" & Img1 (Img1'First + 1 .. Img1'Last);
                     Nanos : constant String :=
                               Img2 (Img2'Last - 8 .. Img2'Last);

                  begin
                     case Picture (P + 1) is
                        when 'i' =>
                           Result := Result &
                             Nanos (Nanos'First .. Nanos'First + 2);

                        when 'o' =>
                           Result := Result &
                             Nanos (Nanos'First .. Nanos'First + 5);

                        when 'N' =>
                           Result := Result & Nanos;

                        when others =>
                           null;
                     end case;
                  end;

                  --  Time, 24-hour (hh:mm:ss)
               when 'T' =>
                  Result := Result &
                    Image (Hour, Padding, 2) & ':' &
                    Image (Minute, Padding, 2) & ':' &
                    Image (Second, Padding, 2);

                  --  Locale's abbreviated weekday name (Sun..Sat)
               when 'a' =>
                  Result := Result & Day_Abbr_Name (Date);

                  --  Locale's full weekday name, variable length
                  --  (Sunday..Saturday)
               when 'A' =>
                  Result := Result & Day_Full_Name (Date);

                  --  Locale's abbreviated month name (Jan..Dec)
               when 'b' | 'h' =>
                  Result := Result & Mon_Abbr_Name (Month);

                  --  Locale's full month name, variable length
                  --  (January..December).
               when 'B' =>
                  Result := Result & Mon_Full_Name (Month);

                  --  Locale's date and time (Sat Nov 04 12:02:33 EST 1989)
               when 'c' =>
                  Result := Result & Format (Nl_Langinfo (D_T_FMT), Date);

                  --   Day of month (01..31)
               when 'd' =>
                  Result := Result & Image (Day, Padding, 2);

                  --   Day of month ( 1..31)
               when 'e' =>
                  Result := Result & Image (Day, Space, 2);

                  --  Date (mm/dd/yy)
               when 'D' =>
                  Result := Result &
                    Image (Month, Padding, 2) & '/' &
                    Image (Day, Padding, 2) & '/' &
                    Image (Year, Padding, 2);

                  --  Locale's date
               when 'x' =>
                  Result := Result & Format (Nl_Langinfo (D_FMT), Date);

                  --  Locale's time
               when 'X' =>
                  Result := Result & Format (Nl_Langinfo (T_FMT), Date);

                  --  Numeric timezone
               when 'z' =>
                  declare
                     TZ   : constant Integer :=
                       Integer (UTC_Time_Offset (Date));
                     TZH  : constant String :=
                       Image (abs (TZ / 60), Zero, 2);
                     TZM  : constant String :=
                       Image (abs (TZ mod 60), Zero, 2);
                     Sing : constant String := (if TZ < 0 then "-" else "+");
                  begin
                     Result := Result & Sing & TZH & TZM;
                  end;

                  --  Alphabetic time zone abbreviation
               when 'Z' =>
                  Result := Result & "";

                  --  Day of year (001..366)
               when 'j' =>
                  Result := Result & Image (Day_In_Year (Date), Padding, 3);

                  --  Month (01..12)
               when 'm' =>
                  Result := Result & Image (Month, Padding, 2);

                  --  Day of week (1..7) with 1 corresponding to Monday
               when 'u' =>
                  declare
                     DOW : constant Natural range 1 .. 7 :=
                       Day_Name'Pos (Day_Of_Week (Date)) + 1;
                  begin
                     Result := Result & Image (DOW, Length => 1);
                  end;

                  --  Week number of year with Sunday as first day of week
                  --  (00..53)
               when 'U' =>
                  declare
                     Offset : constant Natural :=
                       (Julian_Day (Year, 1, 1) + 1) mod 7;
                     Week   : constant Natural :=
                       1 + ((Day_In_Year (Date) - 1) + Offset) / 7;
                  begin
                     Result := Result & Image (Week, Padding, 2);
                  end;

                  --  Day of week (0..6) with 0 corresponding to Sunday
               when 'w' =>
                  declare
                     DOW : constant Natural range 0 .. 6 :=
                       (if Day_Of_Week (Date) = Sunday then 0
                        else Day_Name'Pos (Day_Of_Week (Date)) + 1);
                  begin
                     Result := Result & Image (DOW, Length => 1);
                  end;

                  --  Week number of year with Monday as first day of week
                  --  (00..53)
               when 'W' =>
                  Result := Result & Image (Week_In_Year (Date), Padding, 2);

                  --  Last two digits of year (00..99)
               when 'y' =>
                  declare
                     Y : constant Natural := Year - (Year / 100) * 100;
                  begin
                     Result := Result & Image (Y, Padding, 2);
                  end;

                  --   Year (1970...)
               when 'Y' =>
                  Result := Result & Image (Year, None, 4);

                  --   Century
               when 'C' =>
                  Result := Result & Image (Year / 100, None);

               when others =>
                  raise Time_Picture_Error
                    with "unknown format character in picture string";

            end case;

            --  Skip past % and format character
            P := P + 2;

            --  Character other than % is copied into the result
         else
            Result := Result & Picture (P);
            P := P + 1;
         end if;
      end loop;

      return To_String (Result);
   end Format;

   -----------------
   -- Format_Time --
   -----------------

   function Format_Time
     (Picture : String; Value : Ada.Calendar.Time) return String
   is
   begin
      return Format (Picture, Value);
   end Format_Time;

end Formatted_Output.Time_Output;
