with Ada.Text_IO;
with Ada.Calendar;
with Interfaces;                           use Interfaces;

with Formatted_Output;                     use Formatted_Output;
with Formatted_Output_Integer;             use Formatted_Output_Integer;
with Formatted_Output_Long_Integer;        use Formatted_Output_Long_Integer;
with Formatted_Output_Long_Long_Integer;   use Formatted_Output_Long_Long_Integer;
with Formatted_Output_Float;               use Formatted_Output_Float;
with Formatted_Output_Long_Float;          use Formatted_Output_Long_Float;
with Formatted_Output_Long_Long_Float;     use Formatted_Output_Long_Long_Float;
with Formatted_Output.Decimal_Output;
with Formatted_Output.Modular_Output;
with Formatted_Output_Short_Integer;       use Formatted_Output_Short_Integer;
with Formatted_Output_Short_Short_Integer; use Formatted_Output_Short_Short_Integer;
with Formatted_Output.Enumeration_Output;
with Formatted_Output.Time_Output;         use Formatted_Output.Time_Output;

with L10n; use L10n;

procedure Formatted_Output_Demo is

   procedure Show_Integer_Ranges is
   begin
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
   end Show_Integer_Ranges;

   procedure Show_Float_Ranges is
   begin
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

   package Unsigned_Output is
     new Formatted_Output.Modular_Output (Interfaces.Unsigned_64);
   use Unsigned_Output;

   type Money is delta 0.01 digits 18;

   package Money_Output is new Formatted_Output.Decimal_Output (Money);
   use Money_Output;

   package Character_Output is
     new Formatted_Output.Enumeration_Output (Character);
   use Character_Output;

   Str : String := "Hello, world!";
   T   : Ada.Calendar.Time := Ada.Calendar.Clock;

begin
   Set_Locale;

   Formatted_Output.Put_Line (+"String = %s" & Str);
   Formatted_Output.Put_Line (+"Char = %c" & Str (Str'First));
   Ada.Text_IO.New_Line;

   Show_Integer_Ranges;
   Ada.Text_IO.New_Line;

   Show_Float_Ranges;
   Ada.Text_IO.New_Line;

   Formatted_Output.Put_Line
     (+"Unsigned_64 type range:\n\t%_35d .. %-_35d"
      & Unsigned_64'First & Unsigned_64'Last);
   Ada.Text_IO.New_Line;

   Formatted_Output.Put_Line
     (+"Money type range (%s):\n\t%+_35e .. %-+_35e"
      & "type Money is delta 0.01 digits 18"
      & Money'First & Money'Last);
   Ada.Text_IO.New_Line;

   Formatted_Output.Put_Line
     (+"Current Time (Locale: %s) = %s"
      & Get_Locale & Format_Time ("'%c'", T));
   Set_Locale (Locale => "POSIX");
   Formatted_Output.Put_Line
     (+"Current Time (Locale: %s) = %s"
      & Get_Locale & Format_Time ("'%c'", T));
   Set_Locale (Locale => "uk_UA.UTF-8");
   Formatted_Output.Put_Line
     (+"Current Time (Locale: %s) = %s"
      & Get_Locale & Format_Time ("'%c'", T));
end Formatted_Output_Demo;
