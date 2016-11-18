------------------------------------------------------------------------------
--                                                                          --
--  File:                                                                   --
--     build_formatted_output.adb                                           --
--                                                                          --
--  Description:                                                            --
--     Build program for all Formatted_Output package hierarchy             --
--                                                                          --
--  Author:                                                                 --
--     Eugene Nonko, cm@liceum.secna.ru                                     --
--                                                                          --
--  Revision history:                                                       --
--     16/03/99 - original                                                  --
--                                                                          --
------------------------------------------------------------------------------

with Formatted_Output,
     Formatted_Output.Float_Output,
     Formatted_Output.Integer_Output,
     Formatted_Output.Enumeration_Output,
     Formatted_Output.Modular_Output;

procedure Build_Formatted_Output is
begin -- Build_Formatted_Output
   null;
end Build_Formatted_Output;
