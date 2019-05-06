with Instruction; use Instruction;

-- specification of the VM implementation
package Machine with SPARK_Mode is 
   
   -- the VM returns either 
   -- Success: when the program executes successfully and returns a value
   -- IllegalProgram: when the program executes an invalid behaviour
   -- CyclesExhausted: when the program has not returned a value before
   --                  the cycle count was exhausted (see the Cycles
   --                  argument to the ExecuteProgram procedure below)
   type ReturnCode is (Success,CyclesExhausted,IllegalProgram);
   
   -- each program has exactly this many instructions
   MAX_PROGRAM_LENGTH : constant Integer := 65536;
   
   -- type of values that the program counter (pc) holds
   -- note that the first instruction in the program is at position 1
   type ProgramCounter is range 1 .. MAX_PROGRAM_LENGTH;
   
   -- a program is just an array of instructions, indexed by the pc
   type Program is array (ProgramCounter) of Instruction.Instr;
   
   -- called to execute the given program.
   -- at most Cycles instructions will be executed.
   -- if Cycles instructions have been executed without encountering a 
   -- RET instruction and without encountering invalid behaviour, 
   -- then Ret is set to CyclesExhausted.
   -- Otherwise, if no invalid behaviour is encountered, then Ret is set
   -- Success and Result will contain the value returned by the RET
   -- instruction encountered.
   -- Otherwise, if an invalid behaviour is encountered, then Ret is set
   -- to IllegalProgram.
   -- If Ret is CyclesExhausted or IllegalProgram, Result is meaningless
   procedure ExecuteProgram(Prog : in Program; 
                            Cycles : in Integer;
                            Ret : out ReturnCode;
                            Result : out Integer);


   -- analyse the given program to determine whether it might encounter
   -- invalid behaviour while executing for Cycles instructions
   -- from an initial state with unknown register and memory contents.
   -- returns True if the program might encounter invalid
   -- behaviour. Otherwise, returns False if the program is guaranteed
   -- not to encounter invalid behaviour while Cycles instructions are executed
   function DetectInvalidBehaviour(Prog : in Program; 
                                   Cycles : in Integer) return Boolean;

end Machine;
