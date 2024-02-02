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

generic
   type Item_Type is (<>);

package Formatted_Output.Enumeration_Output is

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type;
   --  Replaces leftmost formatting sequence in Fmt with formatted Value image,
   --  then returns Fmt. Raises exception Format_Error when invalid formatting
   --  sequence is found or no formatting sequence found at all.
   --
   --  Format sequences for enumeration types:
   --
   --  %[flags][width](t|l|U|m)
   --
   --  Flag characters can be:
   --     <, -   The converted value is to be left adjusted on the field boundary.
   --            (The default is right justification.)
   --     >      The converted value is to be right adjusted on the field boundary.
   --            (This is default.)
   --     ^      The converted value is to be center adjusted on the field boundary.
   --            (The default is right justification.)
   --
   --  Width:
   --     number   Integer number specifying minimal field width.
   --     *        The width is not specified in the format string, but as an
   --              additional integer value argument preceding the argument
   --              that has to be formatted.
   --
   --  Format specifier can be:
   --     t   convert capitalized.
   --     l   convert in lower case.
   --     U   convert in upper case.
   --     m   convert in mixed case.

end Formatted_Output.Enumeration_Output;
