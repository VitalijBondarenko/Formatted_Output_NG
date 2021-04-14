------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2016-2021 Vitalii Bondarenko <vibondare@gmail.com>         --
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

with Ada.Calendar;

package Formatted_Output.Time_Output is

   Time_Picture_Error : exception;
   --  Exception raised for incorrect picture

   function Format_Time
     (Picture : String; Value : Ada.Calendar.Time) return String;
   --  Return Date, as interpreted in the current local time zone, as a string
   --  with format Picture. Raise Time_Picture_Error if picture string is null
   --  or has an incorrect format.
   --
   --  Picture is a string to describe date and time output format. The string
   --  is a set of standard character and special tag that are replaced by the
   --  corresponding values. It follows the GNU Date specification. Here are
   --  the recognized directives :
   --
   --     %%   a literal %
   --     \n   a newline
   --     \t   a horizontal tab
   --
   --  Time fields:
   --
   --     %H   hour (00 .. 23)
   --     %I   hour (01 .. 12)
   --     %k   hour ( 0 .. 23)
   --     %l   hour ( 1 .. 12)
   --     %M   minute (00 .. 59)
   --     %N   nanoseconds (000000000 .. 999999999)
   --     %p   locale's AM or PM; blank if not known
   --     %P   like %p, but lower case
   --     %r   locale's time, 12-hour (hh:mm:ss [AP]M)
   --     %R   time, 24-hour (hh:mm)
   --     %s   seconds since 1970-01-01 00:00:00 UTC
   --     %S   second (00..59)
   --     %T   time, 24-hour (hh:mm:ss)
   --
   --  Date fields:
   --
   --     %a   locale's abbreviated weekday name (Sun..Sat)
   --     %A   locale's full weekday name, variable length (Sunday..Saturday)
   --     %b   locale's abbreviated month name (Jan..Dec)
   --     %B   locale's full month name (January..December)
   --     %c   locale's date and time (Sat Nov 04 12:02:33 EST 1989)
   --     %C   century; like %Y, except omit last two digits (e.g., 20)
   --     %d   day of month (01..31)
   --     %D   date (mm/dd/yy)
   --     %e   like %d, but a leading zero is replaced by a space ( 1..31)
   --     %h   same as %b
   --     %j   day of year (001..366)
   --     %m   month (01..12)
   --     %u   day of week (1..7) with 1 corresponding to Monday
   --     %U   week number of year with Sunday as first day of week (00..53)
   --     %w   day of week (0..6) with 0 corresponding to Sunday
   --     %W   week number of year with Monday as first day  of week (00..53)
   --     %x   locale's date representation
   --     %X   locale's time representation
   --     %y   last two digits of year (00..99)
   --     %Y   year (1970...)
   --     %z   +hhmm numeric time zone (e.g., -0400)
   --     %Z   alphabetic time zone abbreviation (e.g., EDT)
   --
   --  By default, date pads numeric fields with zeroes. GNU date recognizes
   --  the following optional numeric modifiers:
   --
   --      -   (hyphen) do not pad the field
   --      _   (underscore) pad the field with spaces
   --
   --  Here are some extensions to the GNU Date specification:
   --
   --     %i   milliseconds (3 digits)
   --     %o   microseconds (6 digits)

end Formatted_Output.Time_Output;
