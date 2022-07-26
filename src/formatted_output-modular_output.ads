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

generic
   type Item_Type is mod <>;

package Formatted_Output.Modular_Output is

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type;
   --  Replaces leftmost formatting sequence in Fmt with formatted Value image,
   --  then returns Fmt. Raises exception Format_Error when invalid formatting
   --  sequence is found or no formatting sequence found at all.
   --
   --  Format sequences for modular types:
   --
   --  %[flags][<width>](d|x|X|o|b)
   --
   --  Flag characters can be:
   --     -   The converted value is to be left adjusted on the field boundary.
   --         (The default is right justification.)
   --     *   The converted value is to be center adjusted on the field boundary.
   --         (The default is right justification.)
   --     +   A sign (+ or -) should always be placed before a number produced by a
   --         signed conversion. By default, a sign is used only for negative numbers.
   --     #   Used with o, x or X specifiers the value is use with Base in C style.
   --     ~   As above, but using Ada style based <base>#<number>#
   --     0   The value should be zero padded.
   --     _   The output is to be grouped with grouping character '_'.
   --         Group size is 3 for Base = 10 and 4 for other.
   --     '   For decimal conversion (d) the output is to be grouped with thousands'
   --         grouping characters if the locale information indicates any.
   --
   --  <width> is decimal number specifying minimum field width.
   --
   --  Format specifier can be:
   --     d   Convert as decimal.
   --     x   Convert as hexadecimal (lowercase).
   --     X   Convert as hexadecimal (uppercase).
   --     o   Convert as octal.
   --     b   Convert as binary.

end Formatted_Output.Modular_Output;
