with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Calendar;

with L10n;                               use L10n;

with Formatted_Output;                   use Formatted_Output;
with Formatted_Output.Decimal_Output;
with Formatted_Output_Integer;           use Formatted_Output_Integer;
with Formatted_Output_Long_Integer;      use Formatted_Output_Long_Integer;
with Formatted_Output_Long_Long_Integer; use Formatted_Output_Long_Long_Integer;
with Formatted_Output_Float;             use Formatted_Output_Float;
with Formatted_Output_Long_Float;        use Formatted_Output_Long_Float;
with Formatted_Output_Long_Long_Float;   use Formatted_Output_Long_Long_Float;
with Formatted_Output.Time_Output;       use Formatted_Output.Time_Output;
with Formatted_Output.Enumeration_Output;
with Formatted_Output.Character_Output;  use Formatted_Output.Character_Output;
with Formatted_Output.Modular_Output;

with Interfaces; use Interfaces;

procedure Money_Test is

   type Money is delta 0.01 digits 18;

   package Money_Output is new Formatted_Output.Decimal_Output (Money);
   use Money_Output;

   type Byte is mod 2 ** 8;
   package Modular_Output is new Formatted_Output.Modular_Output (Interfaces.Unsigned_64);
   use Modular_Output;

   Tf     : Money := 25.09;
   Cf     : Money := 0.6;
   D      : Integer := 2140;
   Charge : Money := Tf * Cf * D;
   Val    : Long_Float := 23.894945;
   T      : Ada.Calendar.Time := Ada.Calendar.Clock;

begin
   Set_Locale;
--     Formatted_Output.Put_Line (+"MAX_Int           = %22d (%x)" & Integer'Last & Integer'Last);
--     Formatted_Output.Put_Line (+"MAX_Long_Int      = %22d (%x)" & Long_Integer'Last & Long_Integer'Last);
--     Formatted_Output.Put_Line (+"MAX_Long_Long_Int = %22d (%x)" & Long_Long_Integer'Last & Long_Long_Integer'Last);
--     New_Line;
--     Formatted_Output.Put_Line (+"MAX_Float           = %f" & Float'Last);
--     Formatted_Output.Put_Line (+"MAX_Long_Float      = %f" & Long_Float'Last);
--     Formatted_Output.Put_Line (+"MAX_Long_Long_Float = %e" & Long_Long_Float'Last);
--     New_Line;
--     Formatted_Output.Put_Line (+"MIN_Money = %25f" & Money'First);
--     Formatted_Output.Put_Line (+"MAX_Money = %25f" & Money'Last);
--     Formatted_Output.Put_Line (+"Mn = %f" & Tf);
--     New_Line;

   Formatted_Output.Put_Line
     (+"Current Time (%*25s) = %s" & Get_Locale & Format_Time ("'%c'", T));
   Formatted_Output.Put_Line (+"Long_Float = %f %+44g" & Val & Val);
   Set_Locale (Locale => "POSIX");
   Formatted_Output.Put_Line
     (+"Current Time (%*25s) = %s" & Get_Locale & Format_Time ("'%c'", T));
   Formatted_Output.Put_Line (+"Long_Float = %f %+44g" & Val & Val);
   Set_Locale (Locale => "uk_UA.UTF-8");
   Formatted_Output.Put_Line
     (+"Current Time (%*25s) = %s" & Get_Locale & Format_Time ("'%c'", T));
   Formatted_Output.Put_Line (+"Long_Float = %f %+44g" & Val & Val);
--     Formatted_Output.Put_Line (+"Long_Int = %'10d" & D);


--     Formatted_Output.Put_Line (+"Mn = %18f" & Tf);
--     Formatted_Output.Put_Line (+"Bool = %m" & True );

--     Formatted_Output.Put_Line (+"Char = %c" & 'L');

--     declare
--  --        I : Float := 2#11111_1111_111.0#;Float'Last;
--        I : Float := Float'Last;
--        S : String (1 .. 128) := (others => ' ');
--        U : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
--     begin
--        Formatted_Output.Put_Line
--          (+"Floats'%%10e' '%%10f' '%%10g': '%10e' '%10f' '%10g'" & I & I & I);
--        Formatted_Output.Put_Line (+"'%d'\n'%x'" & U & U);
--     end;
end Money_Test;
