with Instruction;
use Instruction;
with Debug;
use Debug;

-- used so we can print TAB character
with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode is
   
   -- data values are 32-bit integers
   -- this is the type of words used in the virtual machine
   type DataVal is range -(2**31) .. +(2**31 - 1);
   -- uninitialized data value in virtual machine
   subtype UninitializedDataVal is DataVal;
   -- initialized data value in virtual machine
   subtype InitializedDataVal is DataVal;
   
   -- initialise register as array of uninitialised values
   type Register is array (Reg) of UninitializedDataVal;
   
   -- initialise memory as array of uninitialised values
   type Memory is array (Addr) of UninitializedDataVal;

   procedure IncPC(Ret :in out ReturnCode; Offs : in Offset; PC : in out ProgramCounter) is
   begin
      if Ret = Success then
         if (Integer(PC) <= Integer(ProgramCounter'Last) - Integer(Offs)) and
            (Integer(PC) >= Integer(ProgramCounter'First) - Integer(Offs)) then
            PC := ProgramCounter(Integer(PC) + Integer(Offs));
            Ret := Success;
         else
            Ret := IllegalProgram;
         end if;
      end if;
   end IncPC;
   
   procedure DoAdd(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Ret : out ReturnCode; Regs : in out Register) is
   begin
      if (Regs(Rs2) > 0 and then Regs(Rs1) > DataVal'Last - Regs(Rs2)) or
         (Regs(Rs2) < 0 and then Regs(Rs1) < DataVal'First - Regs(Rs2)) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Regs(Rs1) + Regs(Rs2);
         Ret := Success;
      end if;
   end DoAdd;
   
   procedure DoSub(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Ret : out ReturnCode; Regs : in out Register) is
   begin
      if (Regs(Rs2) < 0 and then Regs(Rs1) > DataVal'Last + Regs(Rs2)) or
         (Regs(Rs2) > 0 and then Regs(Rs1) < DataVal'First + Regs(Rs2)) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Regs(Rs1) - Regs(Rs2);
         Ret := Success;
      end if;
   end DoSub;
   
   procedure DoMul(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Ret : out ReturnCode; Regs : in out Register) is
   begin
      if (Regs(Rs1) < 0 and then Regs(Rs2) < 0 and then Regs(Rs1) < DataVal'Last / Regs(Rs2)) or
         (Regs(Rs1) < 0 and then Regs(Rs2) > 0 and then DataVal'First / Regs(Rs2) > Regs(Rs1)) or
         (Regs(Rs2) /= 0 and then Regs(Rs1) > 0 and then Regs(Rs1) > DataVal'Last / Regs(Rs2)) then
         Ret := IllegalProgram;
      else
        Regs(Rd) := Regs(Rs1) * Regs(Rs2);
        Ret := Success;
      end if;
   end DoMul;
   
   procedure DoDiv(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Ret : out ReturnCode; Regs : in out Register) is
   begin
      if Regs(Rs2) = 0 or (Regs(Rs1) = DataVal'First and Regs(Rs2) = -1) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Regs(Rs1) / Regs(Rs2);
         Ret := Success;
      end if;
   end DoDiv;
   
   procedure DoLdr(Rd : in Reg; Rs : in Reg; Offs : in Offset; Ret : out ReturnCode; Regs : in out Register; Mem : in Memory) is
   begin
      if (Integer(Regs (Rs))  > Integer(Addr'Last) - Integer(Offs)) or
         (Integer(Regs (Rs)) < Integer(Addr'First) - Integer(Offs)) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Mem(Addr(Regs(Rs) + DataVal(Offs)));
         Ret := Success;
      end if;
   end DoLdr;
   
   procedure DoStr(Ra : in Reg; Offs : in Offset; Rb : in Reg; Ret : out ReturnCode; Regs : in Register; Mem : in out Memory) is
   begin
      if (Integer(Regs (Ra) ) > Integer(Addr'Last) - Integer(Offs)) or
         (Integer(Regs (Ra)) < Integer(Addr'First) - Integer(Offs)) then
         Ret := IllegalProgram;
      else
         Mem(Addr(Regs(Ra) + DataVal(Offs))) := Regs(Rb);
         Ret := Success;
      end if;
   end DoStr;
   
   procedure DoMov(Rd : in Reg; Offs : in Offset; Ret : out ReturnCode; Regs : in out Register) with
      Pre => Integer(Offs) >= Integer(DataVal'First) and then Integer(Offs) <= Integer(DataVal'Last) is
   begin
      Regs(Rd) := DataVal(Offs);
      Ret := Success;
   end DoMov;
   
   procedure ExecuteProgram(Prog : in Program; Cycles : in Integer; Ret : out ReturnCode; Result : out Integer) is
      CycleCount : Integer := 0;
      Inst : Instr;
      -- the registers
      Regs : Register := (others => 0);
      -- the memory
      Mem : Memory := (others => 0);
      -- the program counter
      PC : ProgramCounter := ProgramCounter'First;
   begin
      Ret := Success;
      Result := 0;
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(PC);
         
         -- debug print pc and current instruction
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;
         
         case Inst.Op is
            when ADD =>
               DoAdd(Inst.AddRd, Inst.AddRs1, Inst.AddRs2, Ret, Regs);
               IncPC(Ret, 1, PC);
            when SUB =>
               DoSub(Inst.SubRd, Inst.SubRs1, Inst.SubRs2, Ret, Regs);
               IncPC(Ret, 1, PC);
            when MUL =>
               DoMul(Inst.MulRd, Inst.MulRs1, Inst.MulRs2, Ret, Regs);
               IncPC(Ret, 1, PC);
            when DIV =>
               DoDiv(Inst.DivRd, Inst.DivRs1, Inst.DivRs2, Ret, Regs);
               IncPC(Ret, 1, PC);
            when LDR =>
               DoLdr(Inst.LdrRd, Inst.LdrRs, Inst.LdrOffs, Ret, Regs, Mem);
               IncPC(Ret, 1, PC);
            when STR =>
               DoStr(Inst.StrRa, Inst.StrOffs, Inst.StrRb, Ret, Regs, Mem);
               IncPC(Ret, 1, PC);
            when MOV =>
               DoMov(Inst.MovRd, Inst.MovOffs, Ret, Regs);
               IncPC(Ret, 1, PC);
            when Instruction.RET =>
               Result := Integer(Regs(Inst.RetRs));
               Ret := Success;
               return;
            when JMP =>
               IncPC(Ret, Inst.JmpOffs, PC);
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  IncPC(Ret, Inst.JzOffs, PC);
               else
                  IncPC(Ret, 1, PC);
               end if;
            when NOP =>
               IncPC(Ret, 1, PC);
         end case;
         CycleCount := CycleCount + 1;
      end loop;
      if Ret = Success then
         -- Cycles instructions executed without a RET or invalid behaviour
         Ret := CyclesExhausted;
      end if;
   end ExecuteProgram;
   
   function DetectUninitializedVariable(Val : in DataVal) return Boolean is
   begin
      return Val in UninitializedDataVal;
   end DetectUninitializedVariable;

   function DetectInvalidAdd(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.AddRs1))) or
             (DetectUninitializedVariable(Regs(Inst.AddRs2))) or
             (Regs(Inst.AddRs2) > 0 and then Regs(Inst.AddRs1) > DataVal'Last - Regs(Inst.AddRs2)) or
             (Regs(Inst.AddRs2) < 0 and then Regs(Inst.AddRs1) < DataVal'First - Regs(Inst.AddRs2));
   end DetectInvalidAdd;
   
   function DetectInvalidSub(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.SubRs1))) or
             (DetectUninitializedVariable(Regs(Inst.SubRs2))) or
             (Regs(Inst.SubRs2) < 0 and then (Regs(Inst.SubRs1) > DataVal'Last +  Regs(Inst.SubRs2))) or
             (Regs(Inst.SubRs2) > 0 and then (Regs(Inst.SubRs1) < DataVal'First + Regs(Inst.SubRs2)));
   end DetectInvalidSub;
   
   function DetectInvalidMul(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.MulRs1))) or
             (DetectUninitializedVariable(Regs(Inst.MulRs2))) or
             (Regs(Inst.MulRs1) < 0 and then Regs(Inst.MulRs2) < 0 and then Regs(Inst.MulRs1) < DataVal'Last / Regs(Inst.MulRs2)) or
             (Regs(Inst.MulRs1) < 0 and then Regs(Inst.MulRs2) > 0 and then Regs(Inst.MulRs1) < DataVal'First / Regs(Inst.MulRs2)) or
             (Regs(Inst.MulRs2) /= 0 and then Regs(Inst.MulRs1) > 0 and then Regs(Inst.MulRs1) > DataVal'Last / Regs(Inst.MulRs2));
   end DetectInvalidMul;
   
   function DetectInvalidDiv(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.DivRs1))) or
             (DetectUninitializedVariable(Regs(Inst.DivRs2))) or
             (Regs(Inst.DivRs2) = 0) or
             (Regs(Inst.DivRs1) = DataVal'First and Regs(Inst.DivRs2) = -1);
   end DetectInvalidDiv;
   
   function DetectInvalidLdr(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.LdrRs))) or
             (Regs(Inst.LdrRs) < 0 - DataVal(Inst.LdrOffs)) or
             (Regs(Inst.LdrRs) > 65535 - DataVal(Inst.LdrOffs));
   end DetectInvalidLdr;
   
   function DetectInvalidStr(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.StrRa))) or
             (Regs(Inst.StrRa) < 0 - DataVal(Inst.StrOffs)) or
             (Regs(Inst.StrRa) > 65535 - DataVal(Inst.StrOffs));
   end DetectInvalidStr;
   
   function DetectInvalidMov(Inst : in Instr) return Boolean is
   begin
      return (Inst.MovOffs < -(2**31)) or
             (Inst.MovOffs > (2**31 - 1));
   end DetectInvalidMov;
   
   function DetectInvalidRet(Inst : in Instr; Regs : in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.RetRs)));
   end DetectInvalidRet;
   
   function DetectInvalidJmp(Inst : in Instr; PC : in ProgramCounter) return Boolean is
   begin
      return (Integer(Inst.JmpOffs) = 0) or -- infinite loop
             (Integer(PC) + Integer(Inst.JmpOffs) < 0) or
             (Integer(PC) + Integer(Inst.JmpOffs) > 65535);
   end DetectInvalidJmp;
   
   function DetectInvalidJz(Inst : in Instr; PC : in ProgramCounter; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.JzRa))) or
             (Regs(Inst.JzRa) = 0 and then (
                (Integer(Inst.JzOffs) = 0) or -- infinite loop
                (Integer(PC) + Integer(Inst.JzOffs) < 0) or
                (Integer(PC) + Integer(Inst.JzOffs) > 65535))
             ) or
             (Regs(Inst.JzRa) /= 0 and then (
                (Integer(PC) + 1 < 0) or
                (Integer(PC) + 1 > 65535))
             );
   end DetectInvalidJz;
   
   function DetectInvalidCycle(CycleCount : in Integer; Cycles : in Integer) return Boolean is
   begin
      return not (CycleCount < Cycles);
   end DetectInvalidCycle;
   
   function DetectInvalidPC(PC : in ProgramCounter; Offs : in Offset) return Boolean is
   begin
      return (Integer(PC) > Integer(ProgramCounter'Last) - Integer(Offs)) or
             (Integer(PC) < Integer(ProgramCounter'First) - Integer(Offs));
   end DetectInvalidPC;
   
   procedure PerformAdd(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidAdd(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         Regs(Inst.AddRd) := InitializedDataVal(Regs(Inst.AddRs1) + Regs(Inst.AddRs2));
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformAdd;
   
   procedure PerformSub(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidSub(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         Regs(Inst.SubRd) := InitializedDataVal(Regs(Inst.SubRs1) - Regs(Inst.SubRs2));
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformSub;
   
   procedure PerformMul(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidMul(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         Regs(Inst.MulRd) := InitializedDataVal(Regs(Inst.MulRs1) * Regs(Inst.MulRs2));
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformMul;
   
   procedure PerformDiv(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidDiv(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         Regs(Inst.DivRd) := InitializedDataVal(Regs(Inst.DivRs1) / Regs(Inst.DivRs2));
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformDiv;
   
   procedure PerformLdr(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Mem : in Memory; Ret : in out Boolean) is
   begin
      if not (DetectInvalidLdr(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         Regs(Inst.LdrRd) := InitializedDataVal(Mem(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs))));
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformLdr;
   
   procedure PerformStr(Inst : in Instr; PC : in out ProgramCounter; Regs : in Register; Mem : in out Memory; Ret : in out Boolean) is
   begin
      if not (DetectInvalidStr(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         Mem(Addr(Regs(Inst.StrRa) + DataVal(Inst.StrOffs))) := InitializedDataVal(Regs(Inst.StrRb));
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformStr;
   
   procedure PerformMov(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidMov(Inst) or DetectInvalidPC(PC, 1)) then
         Regs(Inst.MovRd) := InitializedDataVal(DataVal(Inst.MovOffs));
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformMov;
   
   procedure PerformRet(Inst : in Instr; Regs : in Register; Ret : in out Boolean) is
   begin
      if not DetectInvalidRet(Inst, Regs) then
         Ret := False;
      end if;
   end PerformRet;
   
   procedure PerformJmp(Inst : in Instr; PC : in out ProgramCounter; Ret : in out Boolean) is
   begin
      if not (DetectInvalidJmp(Inst, PC) or DetectInvalidPC(PC, Inst.JmpOffs)) then
         PC := ProgramCounter(Integer(PC) + Integer(Inst.JmpOffs));
         Ret := False;
      end if; 
   end PerformJmp;
   
   procedure PerformJz(Inst : in Instr; PC : in out ProgramCounter; Regs : in Register; Ret : in out Boolean) is
   begin
      if not DetectInvalidJz(Inst, PC, Regs) then
         if Regs(Inst.JzRa) = 0 then
            if not DetectInvalidPC(PC, Inst.JzOffs) then
               PC := ProgramCounter(Integer(PC) + Integer(Inst.JzOffs));
               Ret := False;
            end if;
         else
            if not DetectInvalidPC(PC, 1) then
               PC := ProgramCounter(Integer(PC) + Integer(1));
               Ret := False;
            end if;
         end if;
      end if;
   end PerformJz;
   
   procedure PerformNop(PC : in out ProgramCounter; Ret : in out Boolean) is
   begin
      if not DetectInvalidPC(PC, 1) then
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformNop;
   
   function DynamicAnalysis(Prog : in Program; Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      PC : ProgramCounter := ProgramCounter'First;
      Regs : Register := (others => 0);
      Mem : Memory := (others => 0);
      Ret : Boolean := True;
   begin
      while not DetectInvalidCycle(CycleCount, Cycles) loop
         Inst := Prog(PC);
         Ret := True;
         
         -- debug print pc and current instruction
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;
         
         -- call respective procedure based on instruction operand
         case Inst.Op is
            when ADD =>
               PerformAdd(Inst, PC, Regs, Ret);
            when SUB =>
               PerformSub(Inst, PC, Regs, Ret);
            when MUL =>
               PerformMul(Inst, PC, Regs, Ret);
            when DIV =>
               PerformDiv(Inst, PC, Regs, Ret);
            when LDR =>
               PerformLdr(Inst, PC, Regs, Mem, Ret);
            when STR =>
               PerformStr(Inst, PC, Regs, Mem, Ret);
            when MOV =>
               PerformMov(Inst, PC, Regs, Ret);
            when Instruction.RET =>
               PerformRet(Inst, Regs, Ret);
               exit;
            when JMP =>
               PerformJmp(Inst, PC, Ret);
            when JZ =>
               PerformJz(Inst, PC, Regs, Ret);
            when NOP =>
               PerformNop(PC, Ret);
         end case;
         
         -- terminate early if invalid behaviour detected
         exit when (Ret = True);
         CycleCount := CycleCount + 1;
      end loop;
      return Ret;
   end DynamicAnalysis;

   function DetectInvalidBehaviour(Prog : in Program; Cycles : in Integer) return Boolean is
   begin     
      return DynamicAnalysis(Prog, Cycles);
   end DetectInvalidBehaviour;
   
end Machine;
