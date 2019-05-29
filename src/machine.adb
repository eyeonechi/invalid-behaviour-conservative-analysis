with Instruction;
use Instruction;
with Debug; use Debug;

-- used so we can print TAB character
with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode is
   -- data values are 32-bit integers
   -- this is the type of words used in the virtual machine
   type DataVal is range -(2**31) .. +(2**31 - 1);
   type Register is array (Reg) of DataVal;
   type Mem is array (Addr) of DataVal;
      
   -- the registers
   Regs : array (Reg) of DataVal := (others => 0);
   
   -- the memory
   Memory : array (Addr) of DataVal := (others => 0);
   
   -- the program counter
   PC : ProgramCounter := ProgramCounter'First;
      
   procedure IncPC(Ret :in out ReturnCode; Offs : in Offset) is 
   begin
      if Ret = Success then
         if (Integer(PC) <= Integer(ProgramCounter'Last) - Integer(Offs)) 
           and ( Integer(PC) >= Integer(ProgramCounter'First) - Integer(Offs)) then
            PC := ProgramCounter(Integer(PC) + Integer(Offs));
            Ret := Success;
         else
            Ret := IllegalProgram;
         end if;
      end if;
   end IncPC;
   
   procedure DoAdd(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      if (Regs(Rs2) > 0 and then Regs(Rs1) > DataVal'Last - Regs(Rs2)) then
         Ret := IllegalProgram;
      elsif (Regs(Rs2) < 0 and then Regs(Rs1) < DataVal'First - Regs(Rs2)) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Regs(Rs1) + Regs(Rs2);
         Ret := Success;
      end if;
   end DoAdd;
   
   procedure DoSub(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      if (Regs(Rs2) < 0 and then Regs(Rs1) > DataVal'Last + Regs(Rs2)) or
          (Regs(Rs2) > 0 and then Regs(Rs1) < DataVal'First + Regs(Rs2)) then
         Ret := IllegalProgram;
      else 
         Regs(Rd) := Regs(Rs1) - Regs(Rs2);
         Ret := Success;
      end if;
   end DoSub;
   
   procedure DoMul(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      if (Regs(Rs1) < 0 and then Regs(Rs2) < 0 and then Regs(Rs1) < DataVal'Last / Regs(Rs2)) then
         Ret := IllegalProgram;
      elsif (Regs(Rs1) < 0 and then Regs(Rs2) > 0 and then DataVal'First / Regs(Rs2) > Regs(Rs1)) then
         Ret := IllegalProgram;
      elsif (Regs(Rs2) /= 0 and then Regs(Rs1) > 0 and then Regs(Rs1) > DataVal'Last / Regs(Rs2)) then
         Ret := IllegalProgram;
      else
        Regs(Rd) := Regs(Rs1) * Regs(Rs2);
        Ret := Success;
      end if;
   end DoMul;
   
   procedure DoDiv(Rd : in Reg; 
                   Rs1 : in Reg; 
                   Rs2 : in Reg;
                   Ret : out ReturnCode) is
   begin
      if Regs(Rs2) = 0 or (Regs(Rs1) = DataVal'First and Regs(Rs2) = -1) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Regs(Rs1) / Regs(Rs2);
         Ret := Success;
      end if;
   end DoDiv;
   
   procedure DoLdr(Rd : in Reg; 
                   Rs : in Reg; 
                   Offs : in Offset;
                   Ret : out ReturnCode) is
   A : Addr;
   begin
      if (Integer(Regs (Rs))  > Integer(Addr'Last) - Integer(Offs)) then
         Ret := IllegalProgram;
      elsif (Integer(Regs (Rs)) < Integer(Addr'First) - Integer(Offs)) then
         Ret := IllegalProgram;  
      else 
         A := Addr(Regs(Rs) + DataVal(Offs));
         Regs(Rd) := Memory(A);
         Ret := Success;
      end if;
   end DoLdr;
   
   procedure DoStr(Ra : in Reg;
                   Offs : in Offset;
                   Rb : in Reg;
                   Ret : out ReturnCode) is 
   A : Addr;
   begin
      if (Integer(Regs (Ra) ) > Integer(Addr'Last) - Integer(Offs)) then
         Ret := IllegalProgram;
      elsif  (Integer(Regs (Ra)) < Integer(Addr'First) - Integer(Offs)) then
         Ret := IllegalProgram;  
      else 
         A := Addr(Regs(Ra) + DataVal(Offs));   
         Memory(A) := Regs(Rb);
         Ret := Success;
      end if;
   end DoStr;
   
   procedure DoMov(Rd : in Reg;
                   Offs : in Offset;
                   Ret : out ReturnCode) with
   Pre => Integer(Offs) >= Integer(DataVal'First) and then Integer(Offs) <= Integer(DataVal'Last)
   is
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

   function DetectInvalidAdd(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (Regs(Inst.AddRs2) > 0 and then Regs(Inst.AddRs1) > DataVal'Last - Regs(Inst.AddRs2)) or
             (Regs(Inst.AddRs2) < 0 and then Regs(Inst.AddRs1) < DataVal'First - Regs(Inst.AddRs2));
   end DetectInvalidAdd;
   
   function DetectInvalidSub(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (Regs(Inst.SubRs2) < 0 and then (Regs(Inst.SubRs1) > DataVal'Last +  Regs(Inst.SubRs2))) or
             (Regs(Inst.SubRs2) > 0 and then (Regs(Inst.SubRs1) < DataVal'First + Regs(Inst.SubRs2)));
   end DetectInvalidSub;
   
   function DetectInvalidMul(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (Regs(Inst.MulRs1) < 0 and then Regs(Inst.MulRs2) < 0 and then Regs(Inst.MulRs1) < DataVal'Last / Regs(Inst.MulRs2)) or
             (Regs(Inst.MulRs1) < 0 and then Regs(Inst.MulRs2) > 0 and then Regs(Inst.MulRs1) < DataVal'First / Regs(Inst.MulRs2)) or
             (Regs(Inst.MulRs2) /= 0 and then Regs(Inst.MulRs1) > 0 and then Regs(Inst.MulRs1) > DataVal'Last / Regs(Inst.MulRs2));
   end DetectInvalidMul;
   
   function DetectInvalidDiv(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (Regs(Inst.DivRs2) = 0) or
             (Regs(Inst.DivRs1) = DataVal'First and Regs(Inst.DivRs2) = -1);
   end DetectInvalidDiv;
   
   function DetectInvalidLdr(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (Regs(Inst.LdrRs) < 0 - DataVal(Inst.LdrOffs)) or
             (Regs(Inst.LdrRs) > 65535 - DataVal(Inst.LdrOffs));
   end DetectInvalidLdr;
   
   function DetectInvalidStr(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (Regs(Inst.StrRa) < 0 - DataVal(Inst.StrOffs)) or
             (Regs(Inst.StrRa) > 65535 - DataVal(Inst.StrOffs));
   end DetectInvalidStr;
   
   function DetectInvalidMov(Inst : in Instr) return Boolean is
   begin
      return (Inst.MovOffs < -(2**31)) or
             (Inst.MovOffs > (2**31 - 1));
   end DetectInvalidMov;
   
   function DetectInvalidJmp(Inst : in Instr; PC : in ProgramCounter) return Boolean is
   begin
      return (Integer(PC) + Integer(Inst.JmpOffs) < 0) or
             (Integer(PC) + Integer(Inst.JmpOffs) > 65535);
   end DetectInvalidJmp;
   
   function DetectInvalidJz(Inst : in Instr; PC : in ProgramCounter; Regs: in Register) return Boolean is
   begin
      return (Regs(Inst.JzRa) = 0 and then (
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
      if not DetectInvalidAdd(Inst, Regs) then
         Regs(Inst.AddRd) := Regs(Inst.AddRs1) + Regs(Inst.AddRs2);
         if not DetectInvalidPC(PC, 1) then
            PC := ProgramCounter(Integer(PC) + Integer(1));
            Ret := False;
         end if;
      end if;
   end PerformAdd;
   
   procedure PerformSub(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not DetectInvalidSub(Inst, Regs) then
         Regs(Inst.SubRd) := Regs(Inst.SubRs1) - Regs(Inst.SubRs2);
         if not DetectInvalidPC(PC, 1) then
            PC := ProgramCounter(Integer(PC) + Integer(1));
            Ret := False;
         end if;
      end if;
   end PerformSub;
   
   procedure PerformMul(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not DetectInvalidMul(Inst, Regs) then
         Regs(Inst.MulRd) := Regs(Inst.MulRs1) * Regs(Inst.MulRs2);
         if not DetectInvalidPC(PC, 1) then
            PC := ProgramCounter(Integer(PC) + Integer(1));
            Ret := False;
         end if;
      end if;
   end PerformMul;
   
   procedure PerformDiv(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not DetectInvalidDiv(Inst, Regs) then
         Regs(Inst.DivRd) := Regs(Inst.DivRs1) / Regs(Inst.DivRs2);
         if not DetectInvalidPC(PC, 1) then
            PC := ProgramCounter(Integer(PC) + Integer(1));
            Ret := False;
         end if;
      end if;
   end PerformDiv;
   
   procedure PerformLdr(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Memory : in Mem; Ret : in out Boolean) is
   begin
      if not DetectInvalidLdr(Inst, Regs) then
         Regs(Inst.LdrRd) := Memory(Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs)));
         if not DetectInvalidPC(PC, 1) then
            PC := ProgramCounter(Integer(PC) + Integer(1));
            Ret := False;
         end if;
      end if;
   end PerformLdr;
   
   procedure PerformStr(Inst : in Instr; PC : in out ProgramCounter; Regs : in Register; Memory : in out Mem; Ret : in out Boolean) is
   begin
      if not DetectInvalidStr(Inst, Regs) then
         Memory(Addr(Regs(Inst.StrRa) + DataVal(Inst.StrOffs))) := Regs(Inst.StrRb);
         if not DetectInvalidPC(PC, 1) then
            PC := ProgramCounter(Integer(PC) + Integer(1));
            Ret := False;
         end if;
      end if;
   end PerformStr;
   
   procedure PerformMov(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not DetectInvalidMov(Inst) then
         Regs(Inst.MovRd) := DataVal(Inst.MovOffs);
         if not DetectInvalidPC(PC, 1) then
            PC := ProgramCounter(Integer(PC) + Integer(1));
            Ret := False;
         end if;
      end if;
   end PerformMov;
   
   procedure PerformJmp(Inst : in Instr; PC : in out ProgramCounter; Ret : in out Boolean) is
   begin
      if not DetectInvalidJmp(Inst, PC) then
         if not DetectInvalidPC(PC, Inst.JmpOffs) then
            PC := ProgramCounter(Integer(PC) + Integer(Inst.JmpOffs));
            Ret := False;
         end if;
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
   
   procedure PerformNop(PC : in out ProgramCounter) is
   begin
      if not DetectInvalidPC(PC, 1) then
         PC := ProgramCounter(Integer(PC) + Integer(1));
      end if;
   end PerformNop;
   
   function DynamicAnalysis(Prog : in Program; Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      PC : ProgramCounter := ProgramCounter'First;
      Regs : Register := (others => 0);
      Memory : Mem := (others => 0);
      R : Boolean := True;
   begin
      while (CycleCount < Cycles) loop
         Inst := Prog(PC);
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;
         R := True;
         case Inst.Op is
            when ADD =>
               PerformAdd(Inst, PC, Regs, R);
            when SUB =>
               PerformSub(Inst, PC, Regs, R);
            when MUL =>
               PerformMul(Inst, PC, Regs, R);
            when DIV =>
               PerformDiv(Inst, PC, Regs, R);
            when LDR =>
               PerformLdr(Inst, PC, Regs, Memory, R);
            when STR =>
               PerformStr(Inst, PC, Regs, Memory, R);
            when MOV =>
               PerformMov(Inst, PC, Regs, R);
            when Instruction.RET =>
               R := DetectInvalidCycle(CycleCount, Cycles);
               exit;
            when JMP =>
               PerformJmp(Inst, PC, R);
            when JZ =>
               PerformJz(Inst, PC, Regs, R);
            when NOP =>
               PerformNop(PC);
         end case;
         CycleCount := CycleCount + 1;
         if R = True then
            exit;
         end if;
      end loop;
      return R;
   end DynamicAnalysis;

   function DetectInvalidBehaviour(
      Prog : in Program;
      Cycles : in Integer
   ) return Boolean is
   begin     
      return DynamicAnalysis(Prog, Cycles);
   end DetectInvalidBehaviour;
   
end Machine;
