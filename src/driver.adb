with Instruction;
use Instruction;
with Machine;
use Machine;
with Debug; use Debug;

procedure Driver with SPARK_Mode is
   Prog : Program := (others => (Op => NOP));
   Code : ReturnCode;
   Result : Integer;
   HasInvalidBehaviour : Boolean;
begin
   -- initialise the random number generators used to generate
   -- random instructions. Commenting this out may yield predictable
   -- (i.e. non-random) output
   Instruction.Init;
      
   -- generate a random program
   Put_Line("Generating Random Program...");
   for I in Prog'Range loop
      GenerateRandomInstr(Prog(I));
   end loop;
   
   Put_Line("Analysing Program for Invalid Behaviour...");
   HasInvalidBehaviour := DetectInvalidBehaviour(Prog,MAX_PROGRAM_LENGTH);
   Put("Analysis Result: ");
   Put(HasInvalidBehaviour'Image); New_Line;

   
   -- run the program
   Put_Line("Executing program...");
   ExecuteProgram(Prog,MAX_PROGRAM_LENGTH,Code,Result);
   Put("Return Code: ");
   Put(Code'Image);
   if Code = Success then
      Put(" Result: "); Put(Result);
   end if;
   New_Line;   

end Driver;
