--  with Ada.Text_IO;
with Interfaces;                           use Interfaces;

with Formatted_Output;                     use Formatted_Output;
with Formatted_Output_Integer;             use Formatted_Output_Integer;
with Formatted_Output_Long_Integer;        use Formatted_Output_Long_Integer;
with Formatted_Output_Long_Long_Integer;   use Formatted_Output_Long_Long_Integer;
with Formatted_Output_Short_Integer;       use Formatted_Output_Short_Integer;
with Formatted_Output_Short_Short_Integer; use Formatted_Output_Short_Short_Integer;
with Formatted_Output.Modular_Output;

with L10n;                                 use L10n;

procedure Integer_Output_Demo is

--     package TIO renames Ada.Text_IO;

   package Unsigned_Output is
     new Formatted_Output.Modular_Output (Unsigned_64);
   use Unsigned_Output;

begin
   Formatted_Output.Put_Line
     (+"Short_Short_Integer type range:\n\t%+_30d .. %-+_30d"
      & Short_Short_Integer'First & Short_Short_Integer'Last);
   Formatted_Output.Put_Line
     (+"Short_Integer type range:\n\t%+_30d .. %-+_30d"
      & Short_Integer'First & Short_Integer'Last);
   Formatted_Output.Put_Line
     (+"Integer type range:\n\t%+_30d .. %-+_30d"
      & Integer'First & Integer'Last);
   Formatted_Output.Put_Line
     (+"Long_Integer type range:\n\t%+_30d .. %-+_30d"
      & Long_Integer'First & Long_Integer'Last);
   Formatted_Output.Put_Line
     (+"Long_Long_Integer type range:\n\t%+_30d .. %-+_30d"
      & Long_Long_Integer'First & Long_Long_Integer'Last);
   Formatted_Output.Put_Line
     (+"Natural type range:\n\t%+_30d .. %-+_30d"
      & Natural'First & Natural'Last);
   Formatted_Output.Put_Line
     (+"Positive type range:\n\t%+_30d .. %-+_30d"
      & Positive'First & Positive'Last);
   Formatted_Output.Put_Line
     (+"Unsigned_64 type range:\n\t%_30d .. %-_30d"
      & Unsigned_64'First & Unsigned_64'Last);
end Integer_Output_Demo;
