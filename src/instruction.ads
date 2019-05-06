-- This package defines the instructions for the VM
package Instruction with SPARK_Mode is
   
   -- instruction opcodes, where NOP represents e.g. blank lines
   type OpCode is (NOP,ADD,SUB,MUL,DIV,RET,LDR,STR,MOV,JMP,JZ);
   
   -- number of registers in the VM
   NUM_REGS : constant Integer := 32;
   
   -- the type of register names: register Rn is represented by the
   -- number n, so e.g. R3 we represent as the number 3
   type Reg is range 0 .. (NUM_REGS - 1);
   
   -- the size of the memory 
   MEMORY_SIZE : constant Integer := 65536;
   
   -- the type of memory addresses. Note we constrain it so that we can
   -- never represent an out-of-bounds memory address
   type Addr is range 0 .. (MEMORY_SIZE - 1);
   
   -- valid offsets are in the range -M to M where M is the maximum address
   type Offset is range -Addr'Last .. Addr'Last;
   
   -- the type that represents individual instructions
   type Instr(Op : OpCode := NOP) is record
      case Op is
         when NOP =>
            null; -- no-op; has no other data
         when ADD =>
            AddRd : Reg;
            AddRs1 : Reg;
            AddRs2 : Reg;
         when SUB =>
            SubRd : Reg;
            SubRs1 : Reg;
            SubRs2 : Reg;
         when MUL =>
            MulRd : Reg;
            MulRs1 : Reg;
            MulRs2 : Reg;
         when DIV =>
            DivRd : Reg;
            DivRs1 : Reg;
            DivRs2 : Reg;
         when RET =>
            RetRs : Reg;
         when LDR =>
            LdrRd : Reg;
            LdrRs : Reg;
            LdrOffs : Offset;
         when STR =>
            StrRa : Reg;
            StrOffs : Offset;
            StrRb : Reg;
         when MOV =>
            MovRd : Reg;
            MovOffs : Offset;
         when JMP =>
            JmpOffs : Offset;
         when JZ =>
            JzRa : Reg;
            JzOffs : Offset;
         when others =>
            null;
      end case;
   end record;
   
   -- call this to initialise the random number generators
   -- if you don't call this you will get predicatable output
   procedure Init with
     Global => null;
   
   -- generate a random instruction
   procedure GenerateRandomInstr(Inst : out Instr) with
     Global => null;
   
   -- for debugging to print out instructions
   procedure DebugPrintInstr(Inst : in Instr) with
     Global => null;
   
end Instruction;
