-- SWEN90010 High Integrity Systems Engineering
-- Assignment 3
-- --------------------------------------------------------------------------
-- | Name                 | Student ID | Student Email                      |
-- --------------------------------------------------------------------------
-- | Margareta Hardiyanti | 852105     | mhardiyanti@student.unimelb.edu.au |
-- | Ivan Ken Weng Chee   | 736901     | ichee@student.unimelb.edu.au       |
-- --------------------------------------------------------------------------

with Instruction;
use Instruction;
with Debug;
use Debug;

-- used so we can print TAB character
with Ada.Characters.Latin_1;

package body Machine with SPARK_Mode is

   -- integer value
   type IntegerVal is range -(2**31) .. +(2**31 - 1);
   
   -- different states of dataval
   type DataValStates is (Uninitialized, Initialized);
   
   -- data values are 32-bit integers
   -- this is the type of words used in the virtual machine
   type DataVal (State : DataValStates := Uninitialized) is
      record
         case State is
            -- uninitialised DataVal has integer garbage value
            when Uninitialized => Garbage : IntegerVal;
            -- initialised DataVal has integer value
            when Initialized   => Value : IntegerVal;
         end case;
      end record;
   
   -- register which distinguishes initialised and uninitialised values
   type Register is array (Reg) of DataVal;

   -- register of integer values used in executing program
   type IntegerRegister is array (Reg) of IntegerVal;
   
   -- memory which distinguishes initialised and uninitialised values
   type Memory is array (Addr) of DataVal;
   
   -- memory of integer values used in executing program
   type IntegerMemory is array (Addr) of IntegerVal;

   -- increases the program counter by an offset
   procedure IncPC(Ret :in out ReturnCode; Offs : in Offset; PC : in out ProgramCounter) is
   begin
      -- only increase PC if previous return is success
      if Ret = Success then
         -- check for integer overflow
         if (Integer(PC) <= Integer(ProgramCounter'Last) - Integer(Offs)) and
            (Integer(PC) >= Integer(ProgramCounter'First) - Integer(Offs)) then
            PC := ProgramCounter(Integer(PC) + Integer(Offs));
            Ret := Success;
         else
            Ret := IllegalProgram;
         end if;
      end if;
   end IncPC;
   
   -- performs ADD instruction
   procedure DoAdd(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Ret : out ReturnCode; Regs : in out IntegerRegister) is
   begin
      -- check for integer overflow
      if (Regs(Rs2) > 0 and then Regs(Rs1) > IntegerVal'Last - Regs(Rs2)) or
         (Regs(Rs2) < 0 and then Regs(Rs1) < IntegerVal'First - Regs(Rs2)) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Regs(Rs1) + Regs(Rs2);
         Ret := Success;
      end if;
   end DoAdd;
   
   -- performs SUB function
   procedure DoSub(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Ret : out ReturnCode; Regs : in out IntegerRegister) is
   begin
      -- check for integer overflow
      if (Regs(Rs2) < 0 and then Regs(Rs1) > IntegerVal'Last + Regs(Rs2)) or
         (Regs(Rs2) > 0 and then Regs(Rs1) < IntegerVal'First + Regs(Rs2)) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Regs(Rs1) - Regs(Rs2);
         Ret := Success;
      end if;
   end DoSub;
   
   -- performs MUL instruction
   procedure DoMul(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Ret : out ReturnCode; Regs : in out IntegerRegister) is
   begin
      if (Regs(Rs1) < 0 and then Regs(Rs2) < 0 and then Regs(Rs1) < IntegerVal'Last / Regs(Rs2)) or
         (Regs(Rs1) < 0 and then Regs(Rs2) > 0 and then IntegerVal'First / Regs(Rs2) > Regs(Rs1)) or
         (Regs(Rs2) /= 0 and then Regs(Rs1) > 0 and then Regs(Rs1) > IntegerVal'Last / Regs(Rs2)) then
         Ret := IllegalProgram;
      else
        Regs(Rd) := Regs(Rs1) * Regs(Rs2);
        Ret := Success;
      end if;
   end DoMul;
   
   -- performs DIV instruction
   procedure DoDiv(Rd : in Reg; Rs1 : in Reg; Rs2 : in Reg; Ret : out ReturnCode; Regs : in out IntegerRegister) is
   begin
      -- check for dividing by 0 or integer overflow
      if Regs(Rs2) = 0 or (Regs(Rs1) = IntegerVal'First and Regs(Rs2) = -1) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Regs(Rs1) / Regs(Rs2);
         Ret := Success;
      end if;
   end DoDiv;
   
   -- performs LDR instruction
   procedure DoLdr(Rd : in Reg; Rs : in Reg; Offs : in Offset; Ret : out ReturnCode; Regs : in out IntegerRegister; Mem : in IntegerMemory) is
   begin
      -- check for memory access out-of-bounds
      if (Integer(Regs (Rs))  > Integer(Addr'Last) - Integer(Offs)) or
         (Integer(Regs (Rs)) < Integer(Addr'First) - Integer(Offs)) then
         Ret := IllegalProgram;
      else
         Regs(Rd) := Mem(Addr(Regs(Rs) + IntegerVal(Offs)));
         Ret := Success;
      end if;
   end DoLdr;
   
   -- performs STR instruction
   procedure DoStr(Ra : in Reg; Offs : in Offset; Rb : in Reg; Ret : out ReturnCode; Regs : in IntegerRegister; Mem : in out IntegerMemory) is
   begin
      -- check for memory access out-of-bounds
      if (Integer(Regs (Ra) ) > Integer(Addr'Last) - Integer(Offs)) or
         (Integer(Regs (Ra)) < Integer(Addr'First) - Integer(Offs)) then
         Ret := IllegalProgram;
      else
         Mem(Addr(Regs(Ra) + IntegerVal(Offs))) := Regs(Rb);
         Ret := Success;
      end if;
   end DoStr;
   
   -- performs MOV instruction
   procedure DoMov(Rd : in Reg; Offs : in Offset; Ret : out ReturnCode; Regs : in out IntegerRegister) is
   begin
      -- check for offset overflow
      if Integer(Offs) >= Integer(Offset'First) and Integer(Offs) <= Integer(Offset'Last) then
         Regs(Rd) := IntegerVal(Offs);
         Ret := Success;
      else
         Ret := IllegalProgram;
      end if;
   end DoMov;
   
   -- executes the virtual machine
   procedure ExecuteProgram(Prog : in Program; Cycles : in Integer; Ret : out ReturnCode; Result : out Integer) is
      -- the current cycle count
      CycleCount : Integer := 0;
      -- the current instruction
      Inst : Instr;
      -- the registers
      Regs : IntegerRegister := (others => 0);
      -- the memory
      Mem : IntegerMemory := (others => 0);
      -- the program counter
      PC : ProgramCounter := ProgramCounter'First;
   begin
      Ret := Success;
      Result := 0;
      
      -- exit loop if cycles exhausted or success not returned
      while (CycleCount < Cycles and Ret = Success) loop
         Inst := Prog(PC);
         
         -- debug print pc and current instruction
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;
         
         -- call respective procedure based on instruction operand
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
      
      -- Cycles instructions executed without a RET or invalid behaviour
      if Ret = Success then
         Ret := CyclesExhausted;
      end if;
   end ExecuteProgram;
   
   -- detects if a register or memory variable is uninitialised
   function DetectUninitializedVariable(Val : in DataVal) return Boolean is
   begin
      return Val.State = Uninitialized;
   end DetectUninitializedVariable;

   -- detects invalid ADD instruction behaviour
   function DetectInvalidAdd(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return -- {Rb : ?, Rc : ?}
             (DetectUninitializedVariable(Regs(Inst.AddRs1)) and DetectUninitializedVariable(Regs(Inst.AddRs2))) or
             -- {Rb : 0, Rc : ?}
             (Regs(Inst.AddRs1).State = Initialized and then Regs(Inst.AddRs2).State = Uninitialized and then Regs(Inst.AddRs1).Value /= 0) or
             -- {Rb : ?, Rc : 0}
             (Regs(Inst.AddRs1).State = Uninitialized and then Regs(Inst.AddRs2).State = Initialized and then Regs(Inst.AddRs2).Value /= 0) or
             -- {Rb : X, Rc : Y}
             (Regs(Inst.AddRs1).State = Initialized and then Regs(Inst.AddRs2).State = Initialized and then (
                -- {Rb : X, Rc : +Y}
                (Regs(Inst.AddRs2).Value > 0 and then Regs(Inst.AddRs1).Value > IntegerVal'Last - Regs(Inst.AddRs2).Value) or
                -- {Rb : X, Rc : -Y}
                (Regs(Inst.AddRs2).Value < 0 and then Regs(Inst.AddRs1).Value < IntegerVal'First - Regs(Inst.AddRs2).Value)
             ));
   end DetectInvalidAdd;
   
   -- detects invalid SUB instruction behaviour
   function DetectInvalidSub(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return -- {Rb : ?, Rc : ?}
             (DetectUninitializedVariable(Regs(Inst.SubRs1)) and DetectUninitializedVariable(Regs(Inst.SubRs2))) or
             -- {Rb : -1, Rc : ?}
             (Regs(Inst.SubRs1).State = Initialized and then Regs(Inst.SubRs2).State = Uninitialized and then Regs(Inst.SubRs1).Value /= -1) or
             -- {Rb : ?, Rc : 0}
             (Regs(Inst.SubRs1).State = Uninitialized and then Regs(Inst.SubRs2).State = Initialized and then Regs(Inst.SubRs2).Value /= 0) or
             -- {Rb : X, Rc : Y}
             (Regs(Inst.SubRs1).State = Initialized and then Regs(Inst.SubRs2).State = Initialized and then (
                -- {Rb : X, Rc : -Y}
                (Regs(Inst.SubRs2).Value < 0 and then (Regs(Inst.SubRs1).Value > IntegerVal'Last + Regs(Inst.SubRs2).Value)) or
                -- {Rb : X, Rc : +Y}
                (Regs(Inst.SubRs2).Value > 0 and then (Regs(Inst.SubRs1).Value < IntegerVal'First + Regs(Inst.SubRs2).Value))
             ));
   end DetectInvalidSub;
   
   -- detects invalid MUL instruction behaviour
   function DetectInvalidMul(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return -- {Rb : ?, Rc : ?}
             (DetectUninitializedVariable(Regs(Inst.MulRs1)) and DetectUninitializedVariable(Regs(Inst.MulRs2))) or
             -- {Rb : 1, Rc : ?}
             (Regs(Inst.MulRs1).State = Initialized and then Regs(Inst.MulRs2).State = Uninitialized and then Regs(Inst.MulRs1).Value /= 1) or
             -- {Rb : 0, Rc : ?}
             (Regs(Inst.MulRs1).State = Initialized and then Regs(Inst.MulRs2).State = Uninitialized and then Regs(Inst.MulRs1).Value /= 0) or
             -- {Rb : ?, Rc : 1}
             (Regs(Inst.MulRs1).State = Uninitialized and then Regs(Inst.MulRs2).State = Initialized and then Regs(Inst.MulRs2).Value /= 1) or
             -- {Rb : ?, Rc : 0}
             (Regs(Inst.MulRs1).State = Uninitialized and then Regs(Inst.MulRs2).State = Initialized and then Regs(Inst.MulRs2).Value /= 0) or
             -- {Rb : X, Rc : Y}
             (Regs(Inst.MulRs1).State = Initialized and then Regs(Inst.MulRs2).State = Initialized and then (
                -- {Rb : +X, Rc : +Y}
                (Regs(Inst.MulRs1).Value > 0 and then Regs(Inst.MulRs2).Value > 0 and then Regs(Inst.MulRs1).Value > IntegerVal'Last / Regs(Inst.MulRs2).Value) or
                -- {Rb : -X, Rc : -Y}
                (Regs(Inst.MulRs1).Value < 0 and then Regs(Inst.MulRs2).Value < 0 and then Regs(Inst.MulRs1).Value > IntegerVal'Last / Regs(Inst.MulRs2).Value) or
                -- {Rb : -X, Rc : +Y}
                (Regs(Inst.MulRs1).Value < 0 and then Regs(Inst.MulRs2).Value > 0 and then Regs(Inst.MulRs1).Value < IntegerVal'First / Regs(Inst.MulRs2).Value) or
                -- {Rb : +X, Rc : -Y}
                (Regs(Inst.MulRs1).Value > 0 and then Regs(Inst.MulRs2).Value < 0 and then Regs(Inst.MulRs2).Value < IntegerVal'First / Regs(Inst.MulRs1).Value) or
                -- {Rb : 0, Rc : Y}
                not (Regs(Inst.MulRs1).Value = 0 and then Regs(Inst.MulRs2).Value > 0 and then Regs(Inst.MulRs1).Value > IntegerVal'Last / Regs(Inst.MulRs2).Value) or
                -- {Rb : X, Rc : 0}
                not (Regs(Inst.MulRs2).Value = 0 and then Regs(Inst.MulRs1).Value > 0 and then Regs(Inst.MulRs2).Value > IntegerVal'Last / Regs(Inst.MulRs1).Value)
             ));
   end DetectInvalidMul;
   
   -- detects invalid DIV instruction behaviour
   function DetectInvalidDiv(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return -- {Rb : ?, Rc : ?}
             (DetectUninitializedVariable(Regs(Inst.DivRs1)) and DetectUninitializedVariable(Regs(Inst.DivRs2))) or
             -- {Rb : X, Rc : ?}
             (Regs(Inst.DivRs1).State = Initialized and then Regs(Inst.DivRs2).State = Uninitialized) or
             -- {Rb : ?, Rc : 0}
             (Regs(Inst.DivRs1).State = Uninitialized and then Regs(Inst.DivRs2).State = Initialized and then Regs(Inst.DivRs2).Value = 0) or
             -- {Rb : ?, Rc : -1}
             (Regs(Inst.DivRs1).State = Uninitialized and then Regs(Inst.DivRs2).State = Initialized and then Regs(Inst.DivRs2).Value = -1) or
             -- {Rb : X, Rc : Y}
             (Regs(Inst.DivRs1).State = Initialized and then Regs(Inst.DivRs2).State = Initialized and then (
                -- {Rb : X, Rc : 0}
                (Regs(Inst.DivRs2).Value = 0) or
                -- {Rb : -(2**31), Rc : -1}
                (Regs(Inst.DivRs1).Value = IntegerVal'First and Regs(Inst.DivRs2).Value = -1)
             ));
   end DetectInvalidDiv;
   
   -- detects invalid LDR instruction behaviour
   function DetectInvalidLdr(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.LdrRs))) or
             (Regs(Inst.LdrRs).State = Initialized and then (
                (Regs(Inst.LdrRs).Value < IntegerVal(Addr'First) - IntegerVal(Inst.LdrOffs)) or
                (Regs(Inst.LdrRs).Value > IntegerVal(Addr'Last) - IntegerVal(Inst.LdrOffs))
             ));
   end DetectInvalidLdr;
   
   -- detects invalid STR instruction behaviour
   function DetectInvalidStr(Inst : in Instr; Regs: in Register) return Boolean is
   begin
      return (DetectUninitializedVariable(Regs(Inst.StrRa))) or
             (Regs(Inst.StrRa).State = Initialized and then (
                (Regs(Inst.StrRa).Value < IntegerVal(Addr'First) - IntegerVal(Inst.StrOffs)) or
                (Regs(Inst.StrRa).Value > IntegerVal(Addr'Last) - IntegerVal(Inst.StrOffs))
             ));
   end DetectInvalidStr;
   
   -- detects invalid MOV instruction behaviour
   function DetectInvalidMov(Inst : in Instr) return Boolean is
   begin
      return -- MOV Ra -65536
             (Inst.MovOffs < Offset'First) or
             -- MOV Ra 65536
             (Inst.MovOffs > Offset'Last);
   end DetectInvalidMov;

   -- detects invalid JMP instruction behaviour
   function DetectInvalidJmp(Inst : in Instr; PC : in ProgramCounter) return Boolean is
   begin
      return -- JMP 0 (infinite loop)
             (Integer(Inst.JmpOffs) = 0) or
             -- JMP before program starts
             (Integer(PC) + Integer(Inst.JmpOffs) < Integer(ProgramCounter'First)) or
             -- JMP after program ends
             (Integer(PC) + Integer(Inst.JmpOffs) > Integer(ProgramCounter'Last));
   end DetectInvalidJmp;
   
   -- detects invalid JZ instruction behaviour
   function DetectInvalidJz(Inst : in Instr; PC : in ProgramCounter; Regs: in Register) return Boolean is
   begin
      return -- {Ra : X}
             (Regs(Inst.JzRa).State = Initialized and then (
                -- JZ 0 X
                (Regs(Inst.JzRa).Value = 0 and then (
                   -- JZ 0 0 (infinite loop)
                   (Integer(Inst.JzOffs) = 0) or
                   -- JZ before program starts
                   (Integer(PC) + Integer(Inst.JzOffs) < Integer(ProgramCounter'First)) or
                   -- JZ after program ends
                   (Integer(PC) + Integer(Inst.JzOffs) > Integer(ProgramCounter'Last))
                )) or
                -- JZ /0 X
                (Regs(Inst.JzRa).Value /= 0 and then (
                   -- JZ before program starts
                   (Integer(PC) + 1 < Integer(ProgramCounter'First)) or
                   -- JZ after program ends
                   (Integer(PC) + 1 > Integer(ProgramCounter'Last)))
                )
             )) or
             -- {Ra : ?}
             (Regs(Inst.JzRa).State = Uninitialized and then (
                -- JZ 0 X
                (Regs(Inst.JzRa).Garbage = 0 and then (
                   -- JZ 0 0 (infinite loop)
                   (Integer(Inst.JzOffs) = 0) or
                   -- JZ before program starts
                   (Integer(PC) + Integer(Inst.JzOffs) < Integer(ProgramCounter'First)) or
                   -- JZ after program ends
                   (Integer(PC) + Integer(Inst.JzOffs) > Integer(ProgramCounter'Last))
                )) or
                -- JZ /0 X
                (Regs(Inst.JzRa).Garbage /= 0 and then (
                   -- JZ before program starts
                   (Integer(PC) + 1 < Integer(ProgramCounter'First)) or
                   -- JZ after program ends
                   (Integer(PC) + 1 > Integer(ProgramCounter'Last)))
                )
             ));
   end DetectInvalidJz;
   
   -- detects if a program has exhausted the given cycles
   function DetectInvalidCycle(CycleCount : in Integer; Cycles : in Integer) return Boolean is
   begin
      return not (CycleCount < Cycles);
   end DetectInvalidCycle;
   
   -- detects invalid program counter values
   function DetectInvalidPC(PC : in ProgramCounter; Offs : in Offset) return Boolean is
   begin
      return -- PC > 65536
             (Integer(PC) > Integer(ProgramCounter'Last) - Integer(Offs)) or
             -- PC < 1
             (Integer(PC) < Integer(ProgramCounter'First) - Integer(Offs));
   end DetectInvalidPC;
   
   -- performs ADD instruction
   procedure PerformAdd(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidAdd(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         -- {Rb : X, Rc : Y}
         if Regs(Inst.AddRs1).State = Initialized and then Regs(Inst.AddRs2).State = Initialized then
            Regs(Inst.AddRd) := (State => Initialized,
                                 Value => Regs(Inst.AddRs1).Value + Regs(Inst.AddRs2).Value);
         -- {Rb : X, Rc : ?}
         elsif Regs(Inst.AddRs1).State = Initialized and then Regs(Inst.AddRs2).State = Uninitialized then
            Regs(Inst.AddRd) := (State => Initialized,
                                 Value => Regs(Inst.AddRs1).Value + Regs(Inst.AddRs2).Garbage);
         -- {Rb : ?, Rc : Y}
         elsif Regs(Inst.AddRs1).State = Uninitialized and then Regs(Inst.AddRs2).State = Initialized then
            Regs(Inst.AddRd) := (State => Initialized,
                                 Value => Regs(Inst.AddRs1).Garbage + Regs(Inst.AddRs2).Value);
         end if;
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformAdd;
   
   -- performs ADD instruction
   procedure PerformSub(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidSub(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         -- {Rb : X, Rc : Y}
         if Regs(Inst.SubRs1).State = Initialized and then Regs(Inst.SubRs2).State = Initialized then
            Regs(Inst.SubRd) := (State => Initialized,
                                 Value => Regs(Inst.SubRs1).Value - Regs(Inst.SubRs2).Value);
         -- {Rb : X, Rc : ?}
         elsif Regs(Inst.SubRs1).State = Initialized and then Regs(Inst.SubRs2).State = Uninitialized then
            Regs(Inst.SubRd) := (State => Initialized,
                                 Value => Regs(Inst.SubRs1).Value - Regs(Inst.SubRs2).Garbage);
         -- {Rb : ?, Rc : Y}
         elsif Regs(Inst.SubRs1).State = Uninitialized and then Regs(Inst.SubRs2).State = Initialized then
            Regs(Inst.SubRd) := (State => Initialized,
                                 Value => Regs(Inst.SubRs1).Garbage - Regs(Inst.SubRs2).Value);
         end if;
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformSub;
   
   -- performs MUL instruction
   procedure PerformMul(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidMul(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         -- {Rb : X, Rc : Y}
         if Regs(Inst.MulRs1).State = Initialized and then Regs(Inst.MulRs2).State = Initialized then
            Regs(Inst.MulRd) := (State => Initialized,
                                 Value => Regs(Inst.MulRs1).Value * Regs(Inst.MulRs2).Value);
         -- {Rb : X, Rc : ?}
         elsif Regs(Inst.MulRs1).State = Initialized and then Regs(Inst.MulRs2).State = Uninitialized then
            Regs(Inst.MulRd) := (State => Initialized,
                                 Value => Regs(Inst.MulRs1).Value * Regs(Inst.MulRs2).Garbage);
         -- {Rb : ?, Rc : Y}
         elsif Regs(Inst.MulRs1).State = Uninitialized and then Regs(Inst.MulRs2).State = Initialized then
            Regs(Inst.MulRd) := (State => Initialized,
                                 Value => Regs(Inst.MulRs1).Garbage * Regs(Inst.MulRs2).Value);
         end if;
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformMul;
   
   -- performs DIV instruction
   procedure PerformDiv(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidDiv(Inst, Regs) or DetectInvalidPC(PC, 1)) then
         -- {Rb : X, Rc : Y}
         if Regs(Inst.DivRs1).State = Initialized and Regs(Inst.DivRs2).State = Initialized then
            Regs(Inst.DivRd) := (State => Initialized,
                                 Value => Regs(Inst.DivRs1).Value / Regs(Inst.DivRs2).Value);
         -- {Rb : ?, Rc : Y}
         elsif Regs(Inst.DivRs1).State = Uninitialized and then Regs(Inst.DivRs2).State = Initialized then
            Regs(Inst.DivRd) := (State => Initialized,
                                 Value => Regs(Inst.DivRs1).Garbage / Regs(Inst.DivRs2).Value);
         end if;
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformDiv;
   
   -- performs LDR instruction
   procedure PerformLdr(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Mem : in Memory; Ret : in out Boolean) is
   begin
      if not (DetectInvalidLdr(Inst, Regs) or DetectInvalidPC(PC, 1)) and Regs(Inst.LdrRs).State = Initialized then
         -- memory value has been initialised
         if Mem(Addr(Regs(Inst.LdrRs).Value + IntegerVal(Inst.LdrOffs))).State = Initialized then
            Regs(Inst.LdrRd) := (State => Initialized,
                                 Value => Mem(Addr(Regs(Inst.LdrRs).Value + IntegerVal(Inst.LdrOffs))).Value);
         -- memory value has not been initialised
         elsif Mem(Addr(Regs(Inst.LdrRs).Value + IntegerVal(Inst.LdrOffs))).State = Uninitialized then
            Regs(Inst.LdrRd) := (State => Initialized,
                                 Value => Mem(Addr(Regs(Inst.LdrRs).Value + IntegerVal(Inst.LdrOffs))).Garbage);
         end if;
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformLdr;
   
   -- performs STR instruction
   procedure PerformStr(Inst : in Instr; PC : in out ProgramCounter; Regs : in Register; Mem : in out Memory; Ret : in out Boolean) is
   begin
      if not (DetectInvalidStr(Inst, Regs) or DetectInvalidPC(PC, 1)) and Regs(Inst.StrRa).State = Initialized then
         -- register value has been initialised
         if Regs(Inst.StrRb).State = Initialized then
            -- initialise memory value
            Mem(Addr(Regs(Inst.StrRa).Value + IntegerVal(Inst.StrOffs))) := (State => Initialized,
                                                                             Value => Regs(Inst.StrRb).Value);
         -- register value has not been initialised
         elsif Regs(Inst.StrRb).State = Uninitialized then
            -- initialise memory value
            Mem(Addr(Regs(Inst.StrRa).Value + IntegerVal(Inst.StrOffs))) := (State => Initialized,
                                                                             Value => Regs(Inst.StrRb).Garbage);
         end if;
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformStr;
   
   -- performs MOV instruction
   procedure PerformMov(Inst : in Instr; PC : in out ProgramCounter; Regs : in out Register; Ret : in out Boolean) is
   begin
      if not (DetectInvalidMov(Inst) or DetectInvalidPC(PC, 1)) then
         -- initialise register value
         Regs(Inst.MovRd) := (State => Initialized,
                              Value => IntegerVal(Inst.MovOffs));
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformMov;
   
   -- performs RET instruction
   procedure PerformRet(Ret : out Boolean) is
   begin
      Ret := False;
   end PerformRet;
   
   -- performs JMP instruction
   procedure PerformJmp(Inst : in Instr; PC : in out ProgramCounter; Ret : in out Boolean) is
   begin
      if not (DetectInvalidJmp(Inst, PC) or DetectInvalidPC(PC, Inst.JmpOffs)) then
         PC := ProgramCounter(Integer(PC) + Integer(Inst.JmpOffs));
         Ret := False;
      end if; 
   end PerformJmp;
   
   -- performs JZ instruction
   procedure PerformJz(Inst : in Instr; PC : in out ProgramCounter; Regs : in Register; Ret : in out Boolean) is
   begin
      if not DetectInvalidJz(Inst, PC, Regs) then
         -- {Ra : 0}
         if (Regs(Inst.JzRa).State = Initialized) and then (Regs(Inst.JzRa).Value = 0) then
            if not DetectInvalidPC(PC, Inst.JzOffs) then
               PC := ProgramCounter(Integer(PC) + Integer(Inst.JzOffs));
               Ret := False;
            end if;
         -- {Ra : /0}
         elsif (Regs(Inst.JzRa).State = Initialized) and then (Regs(Inst.JzRa).Value /= 0) then
            if not DetectInvalidPC(PC, 1) then
               PC := ProgramCounter(Integer(PC) + Integer(1));
               Ret := False;
            end if;
         -- {Ra : ?}
         elsif Regs(Inst.JzRa).State = Uninitialized and Regs(Inst.JzRa).Garbage = 0 then
            if not DetectInvalidPC(PC, Inst.JzOffs) then
               PC := ProgramCounter(Integer(PC) + Integer(Inst.JzOffs));
               Ret := False;
            end if;
         -- {Ra : ?}
         elsif Regs(Inst.JzRa).State = Uninitialized and Regs(Inst.JzRa).Garbage /= 0 then
            if not DetectInvalidPC(PC, 1) then
               PC := ProgramCounter(Integer(PC) + Integer(1));
               Ret := False;
            end if;
         end if;
      end if;
   end PerformJz;
   
   -- performs NOP instruction
   procedure PerformNop(PC : in out ProgramCounter; Ret : in out Boolean) is
   begin
      if not DetectInvalidPC(PC, 1) then
         PC := ProgramCounter(Integer(PC) + Integer(1));
         Ret := False;
      end if;
   end PerformNop;
   
   -- performs dynamic analysis to detect invalid behaviour
   function DynamicAnalysis(Prog : in Program; Cycles : in Integer) return Boolean is
      CycleCount : Integer := 0;
      Inst : Instr;
      PC : ProgramCounter := ProgramCounter'First;
      Regs : Register;
      Mem : Memory;
      Ret : Boolean := True;
   begin
      -- set registers and memory to uninitialised values
      Regs := (others => (State => Uninitialized, Garbage => 0));
      Mem := (others => (State => Uninitialized, Garbage => 0));
      while not DetectInvalidCycle(CycleCount, Cycles) loop
         Inst := Prog(PC);
         Ret := True;
         
         -- debug print pc and current instruction
         -- Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         -- DebugPrintInstr(Inst);
         -- New_Line;
         
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
               PerformRet(Ret);
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

   -- detects invalid behaviour before executing the program
   function DetectInvalidBehaviour(Prog : in Program; Cycles : in Integer) return Boolean is
   begin
      return Ret : Boolean do
         Ret := DynamicAnalysis(Prog, Cycles);
      end return;
   end DetectInvalidBehaviour;
   
end Machine;
