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

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Strings.Maps;    use Ada.Strings.Maps;

with Formatted_Output.Utils; use Formatted_Output.Utils;

package body Formatted_Output is

   function Format_String
     (Value : String; Fmt_Spec : Format_Data_Record) return String;

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
                  Raise_Format_Error
                    (Unbounded_Slice (Fmt, I, Length (Fmt)));
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
     (Value : String; Fmt_Spec : Format_Data_Record) return String
   is
      S : String (1 .. Integer'Max (Fmt_Spec.Width, Value'Length));
   begin
      Move
        (Source  => Value,
         Target  => S,
         Drop    => Error,
         Justify => Fmt_Spec.Align,
         Pad     => Filler);
      return S;
   end Format_String;

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : String) return Format_Type is
      Fmt_Spec : Format_Data_Record;
      Fmt_Copy : Unbounded_String;
   begin
      Fmt_Spec.Value_Kind := V_Str;
      Fmt_Spec.Align := Left;
      Parse_Format (Fmt, Fmt_Spec);
      Fmt_Copy := Unbounded_String (Fmt);

      if Fmt_Spec.Precision = Undefined then
         Replace_Slice
           (Fmt_Copy, Fmt_Spec.Spec_Start, Fmt_Spec.Spec_End,
            Format_String (Value, Fmt_Spec));
      else
         Replace_Slice
           (Fmt_Copy, Fmt_Spec.Spec_Start, Fmt_Spec.Spec_End,
            Format_String
              (Value (Value'First .. Value'First + Fmt_Spec.Precision - 1),
               Fmt_Spec));
      end if;
      
      return Format_Type (Fmt_Copy);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Fmt : Format_Type; Value : Character) return Format_Type is
      Fmt_Spec : Format_Data_Record;
      Fmt_Copy : Unbounded_String;
   begin
      Fmt_Spec.Value_Kind := V_Char;
      Parse_Format (Fmt, Fmt_Spec);
      Fmt_Copy := Unbounded_String (Fmt);
      
      Replace_Slice
        (Fmt_Copy, Fmt_Spec.Spec_Start, Fmt_Spec.Spec_End,
         Format_String (String'(1 => Value), Fmt_Spec));
      return Format_Type (Fmt_Copy);
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

   ---------------
   -- Get_Image --
   ---------------
   
   function Get_Image (Fmt : Format_Type) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Unbounded_String (Fmt));
   end Get_Image;
   
   ----------------------------------------------------------------------------
   --  Private functions
   ----------------------------------------------------------------------------
   
   ------------------------
   -- Raise_Format_Error --
   ------------------------

   procedure Raise_Format_Error (Format : Format_Type) is
   begin
      raise Format_Error with
        "wrong format specified: " & Get_Image (Format);
   end Raise_Format_Error;

end Formatted_Output;
