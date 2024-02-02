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
--
--  This is a fork of the GNAT.Calendar.Time_IO package with modifications for
--  NLS support using.

with GNAT.Calendar;           use GNAT.Calendar;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
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

   Alt_Month_Full_Names : constant array (Month_Number) of Locale_Item :=
     (ALTMON_1, ALTMON_2, ALTMON_3, ALTMON_4, ALTMON_5, ALTMON_6,
      ALTMON_7, ALTMON_8, ALTMON_9, ALTMON_10, ALTMON_11, ALTMON_12);

   type Padding_Mode is (Unset, None, Zero, Space);

   type Sec_Number is mod 2 ** 64;
   --  Type used to compute the number of seconds since 1970-01-01.

   ---------------------------------------------------------------------------
   -- Local Subprograms
   ---------------------------------------------------------------------------

   function Format
     (Picture : String; Date : Ada.Calendar.Time) return String;

   function Am_Pm (H : Natural) return String;
   --  Return AM or PM depending on the hour H or empty string if not defined
   --  in locale.

   function Hour_12 (H : Natural) return Positive;
   --  Convert a 1-24h format to a 0-12 hour format.

   function Image
     (Str     : String;
      Padding : Padding_Mode := Space;
      Length  : Natural := 0) return String;
   --  Return image of Str. This string is eventually padded with zeros or spaces
   --  depending of the length required. If length is 0 then no padding occurs.

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

   function Alt_Mon_Full_Name (Month : Month_Number) return String;
   --  Returns locale's full month name.

   function Alt_Era_Format (Item : Locale_Item) return String;
   --  Returns alternative date/time format.
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
     (Str     : String;
      Padding : Padding_Mode := Space;
      Length  : Natural := 0) return String
   is
      Res_Len  : constant Natural := Natural'Max (Length, Str'Length);
      Pad_Char : Character := ' ';
   begin
      if Length = 0 or else Padding = None then
         return Str;
      elsif Padding = Zero then
         Pad_Char := '0';
      end if;

      return String'((Res_Len - Str'Length) * Pad_Char) & Str;
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

   -----------
   -- Image --
   -----------

   function Image
     (N       : Sec_Number;
      Padding : Padding_Mode := Zero;
      Length  : Natural := 0) return String
   is
      NI       : constant String := Sec_Number'Image (N);
      Res_Len  : constant Natural := Natural'Max (Length, NI'Length - 1);
      Pad_Char : Character := '0';
   begin
      if Length = 0 or else Padding = None then
         return NI (2 .. NI'Last);
      elsif Padding = Space then
         Pad_Char := ' ';
      end if;

      return String'((Res_Len - NI'Length + 1) * Pad_Char) & NI (2 .. NI'Last);
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

   ----------------
   -- Julian_Day --
   ----------------

   function Julian_Day (Date : Time) return Integer is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Sec   : Day_Duration;
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

   -------------------
   -- Mon_Abbr_Name --
   -------------------

   function Mon_Abbr_Name (Date : Time) return String is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Sec   : Day_Duration;
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

   -------------------
   -- Mon_Full_Name --
   -------------------

   function Mon_Full_Name (Date : Time) return String is
      Year  : Year_Number;
      Month : Month_Number;
      Day   : Day_Number;
      Sec   : Day_Duration;
   begin
      Split (Date, Year, Month, Day, Sec);
      return Mon_Full_Name (Month);
   end Mon_Full_Name;

   -----------------------
   -- Alt_Mon_Full_Name --
   -----------------------

   function Alt_Mon_Full_Name (Month : Month_Number) return String is
      R : String := Nl_Langinfo (Alt_Month_Full_Names (Month));
   begin
      if R'Length = 0 then
         return Nl_Langinfo (Month_Full_Names (Month));
      else
         return R;
      end if;
   end Alt_Mon_Full_Name;

   --------------------
   -- Alt_Era_Format --
   --------------------

   function Alt_Era_Format (Item : Locale_Item) return String is
      R : String := Nl_Langinfo (Item);
   begin
      if R'Length = 0 then
         case Item is
            when ERA_D_FMT => return Nl_Langinfo (D_FMT);
            when ERA_D_T_FMT => return Nl_Langinfo (D_T_FMT);
            when ERA_T_FMT => return Nl_Langinfo (T_FMT);
            when others => return "";
         end case;
      else
         return R;
      end if;
   end Alt_Era_Format;

   ------------
   -- Format --
   ------------

   function Format
     (Picture : String; Date : Ada.Calendar.Time) return String
   is
      Result          : Unbounded_String;
      Year            : Year_Number;
      Month           : Month_Number;
      Day             : Day_Number;
      Hour            : Hour_Number;
      Minute          : Minute_Number;
      Second          : Second_Number;
      Sub_Second      : Second_Duration;
      TZ              : Integer;
      Use_E_Modifier  : Boolean := False;
      Use_O_Modifier  : Boolean := False;
      P               : Positive;
      S               : Positive;
      Width           : Natural;
      Space_Pad_Specs : constant String := "aAbBcDeFhklpPrRTzZxX";
      Zero_Pad_Specs  : constant String := "CdHIjmMsSuUwWyYioN";
      Default_Padding : Padding_Mode := Unset;
      Padding         : Padding_Mode := Unset;

      function Real_Length (Default : Natural) return Natural is
        (Natural'Max (Default, Width));

   begin
      --  Null picture string is error
      if Picture = "" then
         raise Time_Picture_Error with "null picture string";
      end if;

      --  Get current time in split format
      Split (Date, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      TZ := Integer (UTC_Time_Offset (Date));

      --  Loop through characters of picture string, building result
      Result := Null_Unbounded_String;
      P := Picture'First;

      while P <= Picture'Last loop

         --  A directive has the following format "%[-_0]."
         if Picture (P) = '%' then
            Default_Padding := Unset;
            Padding := Unset;
            Width := 0;

            if P = Picture'Last then
               raise Time_Picture_Error with "picture string ends with '%";
            end if;

            --  Check for the padding, 'O' and 'E' modifier
            case Picture (P + 1) is
               when '-' =>
                  Padding := None;
                  P := P + 1;
               when '_' =>
                  Padding := Space;
                  P := P + 1;
               when '0' =>
                  Padding := Zero;
                  P := P + 1;
               when 'E' =>
                  Use_E_Modifier := True;
                  P := P + 1;
               when 'O' =>
                  Use_O_Modifier := True;
                  P := P + 1;
               when others =>
                  null;
            end case;

            if P = Picture'Last then
               raise Time_Picture_Error
                 with "picture string ends with '-', '_', '0', 'E', 'O'";
            end if;

            --  Check field width
            if Picture (P + 1) in '0' .. '9' then
               S := P + 1;

               while
                 P + 1 < Picture'Last and then Picture (P + 2) in '0' .. '9'
               loop
                  P := P + 1;
               end loop;

               Width := Natural'Value (Picture (S .. P + 1));
               P := P + 1;
            end if;

            --  Set default padding mode
            if Index (Space_Pad_Specs, String'(1 => Picture (P + 1))) > 0 then
               Default_Padding := Space;
            elsif Index (Zero_Pad_Specs, String'(1 => Picture (P + 1))) > 0 then
               Default_Padding := Zero;
            else
               Default_Padding := None;
            end if;

            --  Check padding mode
            if Padding = Unset then
               Padding := Default_Padding;
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

                  ------------------------------------------------------------
                  --  Date & time fields
                  ------------------------------------------------------------

                  --  Locale's abbreviated weekday name (Sun .. Sat)
               when 'a' =>
                  Result := Result & Image
                    (Day_Abbr_Name (Date), Padding, Real_Length (0));

                  --  Locale's full weekday name, variable length
                  --  (Sunday .. Saturday)
               when 'A' =>
                  Result := Result & Image
                    (Day_Full_Name (Date), Padding, Real_Length (0));

                  --  Locale's abbreviated month name (Jan .. Dec)
               when 'b' | 'h' =>
                  Result := Result & Image
                    (Mon_Abbr_Name (Month), Padding, Real_Length (0));

                  --  Locale's full month name, variable length
                  --  (January .. December).
               when 'B' =>
                  if Use_O_Modifier then
                     Result := Result & Image
                       (Alt_Mon_Full_Name (Month), Padding, Real_Length (0));
                  else
                     Result := Result & Image
                       (Mon_Full_Name (Month), Padding, Real_Length (0));
                  end if;

                  --  Locale's date and time (Sat Nov 04 12:02:33 EST 1989)
               when 'c' =>
                  if Use_E_Modifier then
                     Result := Result & Image
                       (Format (Alt_Era_Format (ERA_D_T_FMT), Date),
                        Padding,
                        Real_Length (0));
                  else
                     Result := Result & Image
                       (Format (Alt_Era_Format (D_T_FMT), Date),
                        Padding,
                        Real_Length (0));
                  end if;

                  --  Century
               when 'C' =>
                  Result := Result &
                    Image (Year / 100, Padding, Real_Length (2));

                  --  Day of month (01 .. 31)
               when 'd' =>
                  Result := Result & Image (Day, Padding, Real_Length (2));

                  --  Date (mm/dd/yy)
               when 'D' =>
                  Result := Result & Image
                    (Image (Month, Zero, 2) & '/'
                     & Image (Day, Zero, 2) & '/'
                     & Image (Year, Zero, 2),
                     Padding,
                     Real_Length (0));

                  --  Day of month ( 1..31)
               when 'e' =>
                  Result := Result & Image (Day, Padding, Real_Length (2));

                  --  Date (yyyy-mm-dd)
               when 'F' =>
                  Result := Result & Image
                    (Image (Year, Zero, 4) & '-'
                     & Image (Month, Zero, 2) & '-'
                     & Image (Day, Zero, 2),
                     Padding,
                     Real_Length (0));

                  --  Hour (00 .. 23)
               when 'H' =>
                  Result := Result & Image (Hour, Padding, Real_Length (2));

                  --  Hour (01 .. 12)
               when 'I' =>
                  Result := Result &
                    Image (Hour_12 (Hour), Padding, Real_Length (2));

                  --  Day of year (001 .. 366)
               when 'j' =>
                  Result := Result &
                    Image (Day_In_Year (Date), Padding, Real_Length (3));

                  --  Hour ( 0 .. 23)
               when 'k' =>
                  Result := Result & Image (Hour, Padding, Real_Length (2));

                  --  Hour ( 1 .. 12)
               when 'l' =>
                  Result := Result &
                    Image (Hour_12 (Hour), Space, Real_Length (2));

                  --  Month (01 .. 12)
               when 'm' =>
                  Result := Result & Image (Month, Padding, Real_Length (2));

                  --  Minute (00 .. 59)
               when 'M' =>
                  Result := Result & Image (Minute, Padding, Real_Length (2));

                  --  AM/PM
               when 'p' =>
                  Result := Result &
                    Image (Am_Pm (Hour), Padding, Real_Length (0));

                  --  am/pm lower case
               when 'P' =>
                  Result := Result &
                    Ada.Characters.Handling.To_Lower
                    (Image (Am_Pm (Hour), Padding, Real_Length (0)));

                  --  Time, 12-hour (hh:mm:ss [AP]M)
               when 'r' =>
                  begin
                     Result := Result & Image
                       (Format (Nl_Langinfo (T_FMT_AMPM), Date),
                        Padding,
                        Real_Length (0));
                  exception
                     when others =>
                        Result := Result & Image
                          (Image (Hour_12 (Hour), Zero, 2) & ":"
                           & Image (Minute, Zero, 2) & ":"
                           & Image (Second, Zero, 2),
                           Padding,
                           Real_Length (0));
                  end;

                  --  Time, 24-hour (hh:mm)
               when 'R' =>
                  Result := Result & Image
                    (Image (Hour, Zero, 2) & ":" & Image (Minute, Zero, 2),
                     Padding,
                     Real_Length (0));

                  --  Seconds since 1970-01-01 00:00:00 UTC
                  --  (a GNU extension)
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
                     Result := Result
                       & (if Neg then "-" else "")
                       & Image (Sec_Number (Sec), Padding, Real_Length (0));

                  end;

                  --  Second (00 .. 59)
               when 'S' =>
                  Result := Result & Image (Second, Padding, Real_Length (2));

                  --  Time, 24-hour (hh:mm:ss)
               when 'T' =>
                  Result := Result & Image
                    (Image (Hour, Zero, 2) & ':'
                     & Image (Minute, Zero, 2) & ':'
                     & Image (Second, Zero, 2),
                     Padding,
                     Real_Length (0));

                  --  Day of week (1 .. 7) with 1 corresponding to Monday
               when 'u' =>
                  declare
                     DOW : constant Natural range 1 .. 7 :=
                       Day_Name'Pos (Day_Of_Week (Date)) + 1;
                  begin
                     Result := Result & Image (DOW, Padding, Real_Length (1));
                  end;

                  --  Week number of year with Sunday as first day of week
                  --  (00 .. 53)
               when 'U' =>
                  declare
                     Offset : constant Natural :=
                       (Julian_Day (Year, 1, 1) + 1) mod 7;
                     Week   : constant Natural :=
                       1 + ((Day_In_Year (Date) - 1) + Offset) / 7;
                  begin
                     Result := Result & Image (Week, Padding, Real_Length (2));
                  end;

                  --  Day of week (0 .. 6) with 0 corresponding to Sunday
               when 'w' =>
                  declare
                     DOW : constant Natural range 0 .. 6 :=
                       (if Day_Of_Week (Date) = Sunday then 0
                        else Day_Name'Pos (Day_Of_Week (Date)) + 1);
                  begin
                     Result := Result & Image (DOW, Padding, Real_Length (1));
                  end;

                  --  Week number of year with Monday as first day of week
                  --  (00 .. 53)
               when 'W' =>
                  Result := Result &
                    Image (Week_In_Year (Date), Padding, Real_Length (2));

                  --  Locale's date
               when 'x' =>
                  if Use_E_Modifier then
                     Result := Result & Image
                       (Format (Alt_Era_Format (ERA_D_FMT), Date),
                        Padding,
                        Real_Length (0));
                  else
                     Result := Result & Image
                       (Format (Nl_Langinfo (D_FMT), Date),
                        Padding,
                        Real_Length (0));
                  end if;

                  --  Locale's time
               when 'X' =>
                  if Use_E_Modifier then
                     Result := Result & Image
                       (Format (Alt_Era_Format (ERA_T_FMT), Date),
                        Padding,
                        Real_Length (0));
                  else
                     Result := Result & Image
                       (Format (Nl_Langinfo (T_FMT), Date),
                        Padding,
                        Real_Length (0));
                  end if;

                  --  Last two digits of year (00 .. 99)
               when 'y' =>
                  declare
                     Y : constant Natural := Year - (Year / 100) * 100;
                  begin
                     Result := Result & Image (Y, Padding, Real_Length (2));
                  end;

                  --  Year including the century (1970)
               when 'Y' =>
                  Result := Result & Image (Year, Padding, Real_Length (4));

                  --  z  Numeric timezone
                  --  Z  Alphabetic time zone abbreviation
               when 'z' | 'Z' =>
                  declare
                     TZH  : constant String := Image (abs (TZ / 60), Zero, 2);
                     TZM  : constant String := Image (abs (TZ mod 60), Zero, 2);
                     Sing : constant String := (if TZ < 0 then "-" else "+");
                  begin
                     case Picture (P + 1) is
                        when 'z' =>
                           Result := Result & Image
                             (Sing & TZH & TZM, Padding, Real_Length (0));
                        when 'Z' =>
                           Result := Result & Image
                             ("UTC" & Sing & TZH & ":" & TZM,
                              Padding,
                              Real_Length (0));
                        when others =>
                           null;
                     end case;
                  end;

                  ------------------------------------------------------------
                  --  Additional time fields
                  ------------------------------------------------------------

                  --  i  Milliseconds (3 digits)
                  --  o  Microseconds (6 digits)
                  --  N  Nanoseconds  (9 digits)
               when 'i' | 'o' | 'N' =>
                  declare
                     Sub_Sec : constant Long_Integer :=
                       Long_Integer (Sub_Second * 1_000_000_000);
                     Img1    : constant String := Sub_Sec'Img;
                     Img2    : constant String :=
                       "00000000" & Img1 (Img1'First + 1 .. Img1'Last);
                     Nanos   : constant String :=
                       Img2 (Img2'Last - 8 .. Img2'Last);
                  begin
                     case Picture (P + 1) is
                        when 'i'    => --  milliseconds
                           Result := Result & Image
                             (Nanos (Nanos'First .. Nanos'First + 2),
                              Padding,
                              Real_Length (0));
                        when 'o'    => --  microseconds
                           Result := Result & Image
                             (Nanos (Nanos'First .. Nanos'First + 5),
                              Padding,
                              Real_Length (0));
                        when 'N'    => --  nanoseconds
                           Result := Result & Image
                             (Nanos, Padding, Real_Length (0));
                        when others =>
                           null;
                     end case;
                  end;

                  --  Unknown character
               when others =>
                  raise Time_Picture_Error
                    with "unknown format character in picture string";
            end case;

            --  Skip past % and format character
            P := P + 2;

            --  Reset modifier flag
            Use_O_Modifier := False;

            --  Escape sequences in format strings
         elsif Picture (P) = '\' then
            case Picture (P + 1) is
               when 'n'    => --  A newline
                  Result := Result & ASCII.LF;
                  P := P + 2;
               when 't'    => --  A horizontal tab
                  Result := Result & ASCII.HT;
                  P := P + 2;
               when '\'    => --  A backlash
                  Result := Result & "\";
                  P := P + 2;
               when others =>
                  Result := Result & "\";
                  P := P + 1;
            end case;

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
