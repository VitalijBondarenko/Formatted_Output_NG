with Interfaces;                      use Interfaces;
with Ada.Calendar;                    use Ada.Calendar;
with Ada.Text_IO;

with Formatted_Output;                use Formatted_Output;
with Formatted_Output_Integer;        use Formatted_Output_Integer;
with Formatted_Output.Address_Output; use Formatted_Output.Address_Output;
with Formatted_Output_Long_Float;     use Formatted_Output_Long_Float;
with Formatted_Output.Modular_Output;
with Formatted_Output.Time_Output;    use Formatted_Output.Time_Output;
with Formatted_Output.Enumeration_Output;

with L10n;                            use L10n;

procedure Test is
   S   : constant String := "Hello, world!";
   T   : Time := Clock;
   Fmt : Format_Type := +"\tLocale %10s:\t%s";
begin
   Put_Line (+"String:");
   Put_Line (+"\t[%20s]" & S);
   Put_Line (+"\t[%^20s]" & S);
   Put_Line (+"\t[%>*s]" & 20 & S);
   Put_Line (+"\t[%>20.*s]" & 4 & S);
   Put_Line (+"\t[%*.*s]" & 20 & 4 & S);

   Put_Line (+"Character:\n\t%c %%" & 'A');

   Put_Line (+"Integer:");
   Put_Line (+"\tDecimal:    \t%i %d %.6i %i %.0i %+i %i"
             & 1 & 2 & 3 & 0 & 0 & 4 & (-4));
   Put_Line (+"\tHexadecimal:\t%x %x %X %#x" & 5 & 10 & 10 & 6);
   Put_Line (+"\tOctal:      \t%o %#o %#o" & 10 & 10 & 4);

   declare
      package Uns_FO is new Formatted_Output.Modular_Output (Unsigned_64);
      use Uns_FO;
   begin
      Put_Line (+"\tLargest 64-bit value: %_u (%~_X)"
                & Unsigned_64'Last & Unsigned_64'Last);
   end;

   Put_Line (+"Floating point:");
   Put_Line (+"\tRounding:\t%f %.0f %.32f" & 1.5 & 1.5 & 1.3);
   Put_Line (+"\tPadding:\t%05.2f %.2f %5.2f" & 1.5 & 1.5 & 1.5);
   Put_Line (+"\tScientific:\t%E %e" & 1.5 & 1.5);
   Put_Line (+"\tHexadecimal:\t%a %A" & 1.5 & 1.5);

   Put_Line (+"Time:");
   Set_Locale;
   Put_Line (Fmt & Get_Locale & Format_Time ("%c", T));
   Set_Locale (Locale => "C");
   Put_Line (Fmt & Get_Locale & Format_Time ("%c", T));
   Set_Locale (Locale => "uk_UA.utf8");
   Put_Line (Fmt & Get_Locale & Format_Time ("%c", T));
   Put_Line (+"\tISO 8601 format:\t%s" & Format_Time ("%FT%TZ", T));

   Put_Line (+"Pointer address:");
   Put_Line (+"\t%p" & S'Address);
   Put_Line (+"\t%~_p" & T'Address);
   Put_Line (+"\t%#p" & Fmt'Address);

   declare
      package File_Mode_FO is
        new Formatted_Output.Enumeration_Output (Ada.Text_IO.File_Mode);
      use File_Mode_FO;
      use Ada.Text_IO;
   begin
      Put_Line (+"Enumeration:");
      Put_Line (+"\t%11s: %t, %t, %t"
                & "Capitalized" & In_File & Out_File & Append_File);
      Put_Line (+"\t%11s: %l, %l, %l"
                & "Lower Case " & In_File & Out_File & Append_File);
      Put_Line (+"\t%11s: %U, %U, %U"
                & "Upper Case " & In_File & Out_File & Append_File);
      Put_Line (+"\t%11s: %m, %m, %m"
                & "Mixed Case " & In_File & Out_File & Append_File);
   end;
end Test;
