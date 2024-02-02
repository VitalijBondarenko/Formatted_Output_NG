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

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

package Formatted_Output is

   type Format_Type is private;

   function To_String (Fmt : Format_Type) return String;
   --  Convert formatted string to fixed-length string.
   --  Example:
   --     Send_To_Terminal (To_String (+"Hello %s\n" & "world"));

   function "-" (Fmt : Format_Type) return String
                 renames Formatted_Output.To_String;
   --  Convert formatted string to fixed-length string.

   function To_Format (Fmt_String : String) return Format_Type;
   --  Converts String to Format_Type.

   function "+" (Fmt_String : String) return Format_Type
                 renames Formatted_Output.To_Format;
   --  Converts String to Format_Type.

   function "&" (Fmt : Format_Type; Value : String) return Format_Type;
   --  Replaces leftmost formatting sequence in Fmt with formatted Value
   --  string, then returns Fmt. Raises exception Format_Error when invalid
   --  string formatting sequence is found or no formatting sequence found
   --  at all.
   --
   --  Escape sequences in format strings:
   --     \n   line feed
   --     \r   carriage return
   --     \b   backspace
   --     \t   horizontal tab
   --     \f   form feed
   --
   --  Format sequences for strings:
   --
   --  %[flags][width][.precision]s
   --  
   --  Flag characters can be:
   --     >      The converted value is to be right adjusted on the field boundary.
   --            (The default is left justification.)
   --     <, -   The converted value is to be left adjusted on the field boundary.
   --            (This is default.)
   --     ^      The converted value is to be center adjusted on the field boundary.
   --            (The default is left justification.)
   --  
   --  Width:
   --     number   Integer number specifying minimal field width.
   --     *        The width is not specified in the format string, but as an
   --              additional integer value argument preceding the argument
   --              that has to be formatted.
   --  
   --  Precision:
   --     .number  Integer number specifying the maximum number of characters
   --              to be printed.
   --     .*       The precision is not specified in the format string, but as
   --              an additional integer value argument preceding the argument
   --              that has to be formatted.

   function "&" (Fmt : Format_Type; Value : Character) return Format_Type;
   --  Replaces leftmost formatting sequence in Fmt with formatted Value
   --  string, then returns Fmt. Raises exception Format_Error when invalid
   --  string formatting sequence is found or no formatting sequence found
   --  at all.
   --
   --  Format sequences for character:
   --
   --  %[flags][width]c
   --  
   --  Flag characters can be:
   --     >      The converted value is to be right adjusted on the field boundary.
   --            (The default is left justification.)
   --     <, -   The converted value is to be left adjusted on the field boundary.
   --            (This is default.)
   --     ^      The converted value is to be center adjusted on the field boundary.
   --            (The default is left justification.)
   --  
   --  Width:
   --     number   Integer number specifying minimal field width.
   --     *        The width is not specified in the format string, but as an
   --              additional integer value argument preceding the argument
   --              that has to be formatted.

   procedure Put (Fmt : Format_Type);
   --  Puts formatted string to console using Ada.Text_IO
   --  Defined as Ada.Text_IO.Put (To_String (Fmt));
   --  Example:
   --     Put (+"%s %s %s\n" & "Just" & "a" & "test");

   procedure Put (File : File_Type; Fmt : Format_Type);
   --  Puts formatted string to file using Ada.Text_IO
   --  Defined as Ada.Text_IO.Put (File, To_String (Fmt));

   procedure Put_Line (Fmt : Format_Type);
   --  Puts formatted string to console using Ada.Text_IO

   procedure Put_Line (File : File_Type; Fmt : Format_Type);
   --  Puts formatted string to file using Ada.Text_IO

   Format_Error : exception;
   --  Format_Error exception raised when bad format detected

   Filler : constant Character := ' ';
   --  Filling character, used when padding strings

   function Get_Image (Fmt : Format_Type) return String;
   --  Returns a string representing the value Fmt in display form

private

   type Format_Type is new Ada.Strings.Unbounded.Unbounded_String;

   procedure Raise_Format_Error (Format : Format_Type) with No_Return;
   --  Raise the Format_Error exception which information about the context

end Formatted_Output;
