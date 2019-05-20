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

   function DetectInvalidBehaviour(Prog : in Program; Cycles : in Integer) return Boolean is
      -- data values are 32-bit integers
      -- this is the type of words used in the virtual machine
      type DataVal is range -(2**31) .. +(2**31 - 1);
      type Register is array (Reg) of DataVal;
      type Memory is array (Addr) of DataVal;
      -- the registers
      Regs : Register;
      -- the memory
      Mem : Memory;
      -- the program counter
      PC : ProgramCounter;
      CycleCount : Integer := 0;
      Inst : Instr;
      -- Result : Integer;
      Ret : ReturnCode := Success;
      function DetectIncPC(Offs : in Offset; PC : ProgramCounter) return ProgramCounter is
      begin
         return ProgramCounter(Integer(PC) + Integer(Offs));
      end DetectIncPC;
      function DetectDoAdd(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Regs : in Register) return Register is
         NewRegs : Register;
      begin
         NewRegs := Regs;
         NewRegs(Rd) := Regs(Rs1) + Regs(Rs2);
         return NewRegs;
      end DetectDoAdd;
      function DetectDoSub(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Regs : in Register) return Register is
         NewRegs : Register;
      begin
         NewRegs := Regs;
         NewRegs(Rd) := Regs(Rs1) - Regs(Rs2);
         return NewRegs;
      end DetectDoSub;
      function DetectDoMul(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Regs : in Register) return Register is
         NewRegs : Register;
      begin
         NewRegs := Regs;
         NewRegs(Rd) := Regs(Rs1) * Regs(Rs2);
         return NewRegs;
      end DetectDoMul;
      function DetectDoDiv(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Regs : in Register) return Register is
         NewRegs : Register;
      begin
         NewRegs := Regs;
         NewRegs(Rd) := Regs(Rs1) / Regs(Rs2);
         return NewRegs;
      end DetectDoDiv;
      function DetectDoLdr(Rd : in Reg; Rs : in Reg; Offs : in Offset; Regs : in Register; Mem : in Memory) return Register is
         A : Addr := Addr(Regs(Rs) + DataVal(Offs));
         NewRegs : Register;
      begin
         NewRegs := Regs;
         NewRegs(Rd) := Mem(A);
         return NewRegs;
      end DetectDoLdr;
      function DetectDoStr(Ra : in Reg; Offs : in Offset; Rb : in Reg; Regs : in Register; Mem : in Memory) return Memory is
         A : Addr := Addr(Regs(Ra) + DataVal(Offs));
         NewMem : Memory;
      begin
         NewMem := Mem;
         NewMem(A) := Regs(Rb);
         return NewMem;
      end DetectDoStr;
      function DetectDoMov(Rd : in Reg; Offs : in Offset; Regs : in Register) return Register is
         NewRegs : Register;
      begin
         NewRegs := Regs;
         NewRegs(Rd) := DataVal(Offs);
         return NewRegs;
      end DetectDoMov;
   begin
      Regs := (others => 0);
      Mem := (others => 0);
      PC := ProgramCounter'First;
      -- Static Analysis
      -- examine program code to detect invalid behaviour
      -- TODO: static analysis?

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
               if Regs(Inst.AddRs1)+Regs(Inst.AddRs2) > (2**31 - 1) then
                   return True;
               elsif Regs(Inst.AddRs1)+Regs(Inst.AddRs2) < -(2**31) then
                  return True;
               end if;
               Regs := DetectDoAdd(Inst.AddRd,Inst.AddRs1,Inst.AddRs2, Regs);
               PC := DetectIncPC(1, PC);
            when SUB =>
               if Regs(Inst.SubRs1)-Regs(Inst.SubRs2) > (2**31 - 1) then
                  return True;
               elsif Regs(Inst.SubRs1)-Regs(Inst.SubRs2) < -(2**31) then
                  return True;
               end if;
               Regs := DetectDoSub(Inst.SubRd,Inst.SubRs1,Inst.SubRs2, Regs);
               PC := DetectIncPC(1, PC);
            when MUL =>
               if Regs(Inst.MulRs1) * Regs(Inst.MulRs2) > (2**31 - 1) then
                  return True;
               elsif Regs(Inst.MulRs1) * Regs(Inst.MulRs2) < -(2**31) then
                  return True;
               end if;
               Regs := DetectDoMul(Inst.MulRd,Inst.MulRs1,Inst.MulRs2, Regs);
               PC := DetectIncPC(1, PC);
            when DIV =>
               -- raised CONSTRAINT_ERROR : machine.adb divide by zero
               if Regs(Inst.DivRs2) = 0 then
                  return True;
               end if;
               Regs := DetectDoDiv(Inst.DivRd,Inst.DivRs1,Inst.DivRs2, Regs);
               PC := DetectIncPC(1, PC);
            when LDR =>
               -- Rd = Ra, Rs = Rb
               -- raised CONSTRAINT_ERROR : machine.adb range check failed
               if Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs) < 0 then
                  return True;
               elsif Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs) > 65535 then
                  return True;
               end if;
               Regs := DetectDoLdr(Inst.LdrRd,Inst.LdrRs,Inst.LdrOffs, Regs, Mem);
               PC := DetectIncPC(1, PC);
            when STR =>
               -- is Ra and Rb swapped? Ra = Rb, Rb = Ra
               -- raised CONSTRAINT_ERROR : machine.adb range check failed
               if Regs(Inst.StrRa) + DataVal(Inst.StrOffs) < 0 then
                  return True;
               elsif Regs(Inst.StrRa) + DataVal(Inst.StrOffs) > 65535 then
                  return True;
               end if;
               Mem := DetectDoStr(Inst.StrRa,Inst.StrOffs,Inst.StrRb, Regs, Mem);
               PC := DetectIncPC(1, PC);
            when MOV =>
               if Inst.MovOffs < -(2**31)then
                  return True;
               elsif Inst.MovOffs > (2**31 - 1) then
                  return True;
               end if;
               Regs := DetectDoMov(Inst.MovRd,Inst.MovOffs, Regs);
               PC := DetectIncPC(1, PC);
            when Instruction.RET =>
               -- Result := Integer(Regs(Inst.RetRs));
               -- Ret := Success;
               return False;
            when JMP =>
               -- raised CONSTRAINT_ERROR : machine.adb range check failed
               if (Integer(PC) + Integer(Inst.JmpOffs)) < 0 then
                  return True;
               elsif (Integer(PC) + Integer(Inst.JmpOffs)) > 65535 then
                  return True;
               end if;
               PC := DetectIncPC(Inst.JmpOffs, PC);
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  -- raised CONSTRAINT_ERROR : machine.adb range check failed
                  if (Integer(PC) + Integer(Inst.JzOffs)) < 0 then
                     return True;
                  elsif (Integer(PC) + Integer(Inst.JzOffs)) > 65535 then
                     return True;
                  end if;
                  PC := DetectIncPC(Inst.JzOffs, PC);
               else
                  if Integer(PC) + 1 < 0 then
                     return True;
                  elsif Integer(PC) + 1 > 65535 then
                     return True;
                  end if;
                  PC := DetectIncPC(1, PC);
               end if;
            when NOP =>
               PC := DetectIncPC(1, PC);
         end case;
         CycleCount := CycleCount + 1;
       end loop;
       -- if Ret = Success then
          -- Cycles instructions executed without a RET or invalid behaviour
          -- Ret := CyclesExhausted;
       -- end if;
      return True;
   end DetectInvalidBehaviour;

end Machine;
