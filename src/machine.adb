with Instruction;
use Instruction;
with Debug; use Debug;

-- used so we can print TAB character
with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode is
   -- data values are 32-bit integers
   -- this is the type of words used in the virtual machine
   type DataVal is range -(2**31) .. +(2**31 - 1);

   -- the registers
   Regs : array (Reg) of DataVal := (others => 0);

   -- the memory
   Memory : array (Addr) of DataVal := (others => 0);

   -- the program counter
   PC : ProgramCounter := ProgramCounter'First;

   procedure IncPC(Ret : out ReturnCode; Offs : in Offset) is
   begin
      PC := ProgramCounter(Integer(PC) + Integer(Offs));
      Ret := Success;
   end IncPC;

   procedure DoAdd(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) + Regs(Rs2);
      Ret := Success;
   end DoAdd;

   procedure DoSub(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) - Regs(Rs2);
      Ret := Success;
   end DoSub;

   procedure DoMul(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) * Regs(Rs2);
      Ret := Success;
   end DoMul;

   procedure DoDiv(Rd : in Reg;
                   Rs1 : in Reg;
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := Regs(Rs1) / Regs(Rs2);
      Ret := Success;
   end DoDiv;

   procedure DoLdr(Rd : in Reg;
                   Rs : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
      A : Addr := Addr(Regs(Rs) + DataVal(Offs));
   begin
      Regs(Rd) := Memory(A);
      Ret := Success;
   end DoLdr;

   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg;
                   Ret : out ReturnCode) is
      A : Addr := Addr(Regs(Ra) + DataVal(Offs));
   begin
      Memory(A) := Regs(Rb);
      Ret := Success;
   end DoStr;

   procedure DoMov(Rd : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) is
   begin
      Regs(Rd) := DataVal(Offs);
      Ret := Success;
   end DoMov;

   procedure ExecuteProgram(Prog : in Program;
                            Cycles : in Integer;
                            Ret : out ReturnCode;
                            Result : out Integer)
   is
      CycleCount : Integer := 0;
      Inst : Instr;
   begin
      Ret := Success;
      PC := ProgramCounter'First;
      Result := 0;
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(PC);

         -- debug print pc and current instruction
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;

         case Inst.Op is
            when ADD =>
               DoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2,Ret);
               IncPC(Ret,1);
            when SUB =>
               DoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2,Ret);
               IncPC(Ret,1);
            when MUL =>
               DoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2,Ret);
               IncPC(Ret,1);
            when DIV =>
               DoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2,Ret);
               IncPC(Ret,1);
            when LDR =>
               DoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs,Ret);
               IncPC(Ret,1);
            when STR =>
               DoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb,Ret);
               IncPC(Ret,1);
            when MOV =>
               DoMov(Inst.MovRd,Inst.MovOffs,Ret);
               IncPC(Ret,1);
            when Instruction.RET =>
               Result := Integer(Regs(Inst.RetRs));
               Ret := Success;
               return;
            when JMP =>
               IncPC(Ret,Inst.JmpOffs);
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  IncPC(Ret,Inst.JzOffs);
               else
                  IncPc(Ret,1);
               end if;
            when NOP =>
               IncPC(Ret,1);
         end case;
         CycleCount := CycleCount + 1;
      end loop;
      if Ret = Success then
         -- Cycles instructions executed without a RET or invalid behaviour
         Ret := CyclesExhausted;
      end if;
   end ExecuteProgram;

   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      Result : Integer := 0;
      Ret : ReturnCode := Success;
      -- the registers
      Regs : array (Reg) of DataVal;
      -- the memory
      Memory : array (Addr) of DataVal;
      -- the program counter
      PC : ProgramCounter;
      function DetectIncPC(Offs : in Offset) return ReturnCode is
      begin
         -- PC := ProgramCounter(Integer(PC) + Integer(Offs));
         return Success;
      end DetectIncPC;
      function DetectDoAdd(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg) return ReturnCode is
      begin
         -- Regs(Rd) := Regs(Rs1) + Regs(Rs2);
         return Success;
      end DetectDoAdd;
      function DetectDoSub(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg) return ReturnCode is
      begin
         -- Regs(Rd) := Regs(Rs1) - Regs(Rs2);
         return Success;
      end DetectDoSub;
      function DetectDoMul(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg) return ReturnCode is
      begin
         --Regs(Rd) := Regs(Rs1) * Regs(Rs2);
         return Success;
      end DetectDoMul;
      function DetectDoDiv(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg) return ReturnCode is
      begin
         -- Regs(Rd) := Regs(Rs1) / Regs(Rs2);
         return Success;
      end DetectDoDiv;
      function DetectDoLdr(Rd : in Reg; Rs : in Reg; Offs : in Offset) return ReturnCode is
         -- A : Addr := Addr(Regs(Rs) + DataVal(Offs));
      begin
         -- Regs(Rd) := Memory(A);
         return Success;
      end DetectDoLdr;
      function DetectDoStr(Ra : in Reg; Offs : in Offset; Rb : in Reg) return ReturnCode is
         -- A : Addr := Addr(Regs(Ra) + DataVal(Offs));
      begin
         -- Memory(A) := Regs(Rb);
         return Success;
      end DetectDoStr;
      function DetectDoMov(Rd : in Reg; Offs : in Offset) return ReturnCode is
      begin
         -- Regs(Rd) := DataVal(Offs);
         return Success;
      end DetectDoMov;
   begin
      Regs := (others => 0);
      Memory := (others => 0);
      PC := ProgramCounter'First;
      -- Static Analysis
      -- examine program code to detect invalid behaviour
      -- TODO: static analysis

      -- Dynamic Analysis
      -- run dynamic analysis after static analysis does not detect invalid behaviour
      PC := ProgramCounter'First;
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(PC);

         -- debug print pc and current instruction
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;

         case Inst.Op is
            when ADD =>
               Ret := DetectDoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2);
               Ret := DetectIncPC(1);
            when SUB =>
               Ret := DetectDoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2);
               Ret := DetectIncPC(1);
            when MUL =>
               Ret := DetectDoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2);
               Ret := DetectIncPC(1);
            when DIV =>
               -- raised CONSTRAINT_ERROR : machine.adb divide by zero
               if Regs(Inst.DivRs2) = 0 then
                  return True;
               end if;
               Ret := DetectDoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2);
               Ret := DetectIncPC(1);
            when LDR =>
               -- Rd = Ra, Rs = Rb
               -- raised CONSTRAINT_ERROR : machine.adb range check failed
               if (Integer(Inst.LdrRs) + Integer(Inst.LdrOffs)) < 0 then
                  return True;
               elsif (Integer(Inst.LdrRs) + Integer(Inst.LdrOffs)) > 65535 then
                  return True;
               end if;
               Ret := DetectDoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs);
               Ret := DetectIncPC(1);
            when STR =>
               -- is Ra and Rb swapped? Ra = Rb, Rb = Ra
               -- raised CONSTRAINT_ERROR : machine.adb range check failed
               if (Integer(Inst.StrRa) + Integer(Inst.StrOffs)) < 0 then
                  return True;
               elsif (Integer(Inst.StrRa) + Integer(Inst.StrOffs)) > 65535 then
                  return True;
               end if;
               Ret := DetectDoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb);
               Ret := DetectIncPC(1);
            when MOV =>
               Ret := DetectDoMov(Inst.MovRd,Inst.MovOffs);
               Ret := DetectIncPC(1);
            when Instruction.RET =>
               Result := Integer(Regs(Inst.RetRs));
               Ret := Success;
               return False;
            when JMP =>
               -- raised CONSTRAINT_ERROR : machine.adb range check failed
               if (Integer(PC) + Integer(Inst.JmpOffs)) < 0 then
                  return True;
               elsif (Integer(PC) + Integer(Inst.JmpOffs)) > 65535 then
                  return True;
               end if;
               Ret := DetectIncPC(Inst.JmpOffs);
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  -- raised CONSTRAINT_ERROR : machine.adb range check failed
                  if (Integer(PC) + Integer(Inst.JzOffs)) < 0 then
                     return True;
                  elsif (Integer(PC) + Integer(Inst.JzOffs)) > 65535 then
                     return True;
                  end if;
                  Ret := DetectIncPC(Inst.JzOffs);
               else
                  Ret := DetectIncPC(1);
               end if;
            when NOP =>
               Ret := DetectIncPC(1);
         end case;
         CycleCount := CycleCount + 1;
       end loop;
       if Ret = Success then
          -- Cycles instructions executed without a RET or invalid behaviour
          Ret := CyclesExhausted;
       end if;
      return True;
   end DetectInvalidBehaviour;

end Machine;
