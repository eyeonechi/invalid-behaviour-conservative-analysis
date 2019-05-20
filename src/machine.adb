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


   procedure IncPC(PC : in out ProgramCounter; Offs : in Offset) is 
   begin
         if (Integer(PC) <= Integer(ProgramCounter'Last) - Integer(Offs)) 
           and ( Integer(PC) >= Integer(ProgramCounter'First) - Integer(Offs)) then
            PC := ProgramCounter(Integer(PC) + Integer(Offs));
         end if;
   end IncPC;
   
   function DetectInvalidBehaviour(Prog : in Program;
                                   Cycles : in Integer) return Boolean 
   is
      CycleCount : Integer := 0;
      Inst : Instr;
      PC : ProgramCounter := ProgramCounter'First;
      Regs : array (Reg) of DataVal := (others => 0);
      Memory : array (Addr) of DataVal := (others => 0);
      A : Addr;
   begin
      while (CycleCount < Cycles) loop
         Inst := Prog(PC);
         Put(Integer(PC)); Put(':'); Put(Ada.Characters.Latin_1.HT);
         DebugPrintInstr(Inst);
         New_Line;
         case Inst.Op is
            when ADD =>
               if Regs(Inst.AddRs2) > 0 and then Regs(Inst.AddRs1) > DataVal'Last - Regs(Inst.AddRs2) then
                   return True;
               elsif Regs(Inst.AddRs2) < 0 and then Regs(Inst.AddRs1) < DataVal'First - Regs(Inst.AddRs2) then
                   return True;
               else
                  Regs(Inst.AddRd) := Regs(Inst.AddRs1) + Regs(Inst.AddRs2);
               end if;
               IncPC(PC,1);
            when SUB =>
               if Regs(Inst.SubRs2) < 0 and then (Regs(Inst.SubRs1) > DataVal'Last +  Regs(Inst.SubRs2)) then
                  return True;
               elsif Regs(Inst.SubRs2) > 0 and then (Regs(Inst.SubRs1) < DataVal'First + Regs(Inst.SubRs2)) then
                  return True;
               else
                  Regs(Inst.SubRd) := Regs(Inst.SubRs1) - Regs(Inst.SubRs2);
               end if;
               IncPC(PC,1);
            when MUL =>
               if Regs(Inst.MulRs1) < 0 and then Regs(Inst.MulRs2) < 0 and then Regs(Inst.MulRs1) < DataVal'Last / Regs(Inst.MulRs2) then
                  return True;
               elsif Regs(Inst.MulRs1) < 0 and then Regs(Inst.MulRs2) > 0 and then Regs(Inst.MulRs1) < DataVal'First / Regs(Inst.MulRs2)  then
                  return True;
               elsif (Regs(Inst.MulRs2) /= 0 and then Regs(Inst.MulRs1) > 0 and then Regs(Inst.MulRs1) > DataVal'Last / Regs(Inst.MulRs2)) then
                   return True;
               else
                  Regs(Inst.MulRd) := Regs(Inst.MulRs1) * Regs(Inst.MulRs2);
               end if;
               IncPC(PC,1);
            when DIV =>
               if Regs(Inst.DivRs2) = 0 then
                  return True;
               elsif (Regs(Inst.DivRs1) = DataVal'First and Regs(Inst.DivRs2) = -1) then
                  return True;
               else
                  Regs(Inst.DivRd) := Regs(Inst.DivRs1) / Regs(Inst.DivRs2);
               end if;
               IncPC(PC,1);
            when LDR =>
               if Regs(Inst.LdrRs) < 0 - DataVal(Inst.LdrOffs) then
                   return True;
               elsif Regs(Inst.LdrRs) > 65535 - DataVal(Inst.LdrOffs) then
                  return True;
               else
                  A := Addr(Regs(Inst.LdrRs) + DataVal(Inst.LdrOffs));
                  Regs(Inst.LdrRd) := Memory(A);
               end if;
               IncPC(PC,1);
            when STR =>
               if Regs(Inst.StrRa) < 0 - DataVal(Inst.StrOffs) then
                   return True;
               elsif Regs(Inst.StrRa) > 65535 - DataVal(Inst.StrOffs) then
                   return True;               
               else 
                  A := Addr(Regs(Inst.StrRa) + DataVal(Inst.StrOffs));  
                  Memory(A) := Regs(Inst.StrRb);
               end if;
               IncPC(PC,1);
            when MOV =>
               if Inst.MovOffs < 0 then
                  return True;
               elsif Inst.MovOffs > 65535 then
                  return True;
               else
                  Regs(Inst.MovRd) := DataVal(Inst.MovOffs);
               end if;
               IncPC(PC,1);
            when Instruction.RET =>
               return False;
            when JMP =>
               if Integer(PC) + Integer(Inst.JmpOffs) < 0 then
                  return True;
               elsif Integer(PC) + Integer(Inst.JmpOffs) > 65535 then
                  return True;
               end if;    
               IncPC(PC,Inst.JmpOffs);
            when JZ =>
               if Regs(Inst.JzRa) = 0 then
                  if Integer(PC) + Integer(Inst.JzOffs) < 0 then
                     return True;
                  elsif Integer(PC) + Integer(Inst.JzOffs) > 65535 then
                     return True;
                  end if;
               IncPC(PC,Inst.JzOffs);
               else
                  if Integer(PC) + 1 < 0 then
                     return True;
                  elsif Integer(PC) + 1 > 65535 then
                     return True;
                  end if;
                  IncPC(PC,1);
               end if;
            when NOP =>
                IncPC(PC,1);
         end case;  
         CycleCount := CycleCount + 1;
      end loop;   
      return True;
   end DetectInvalidBehaviour;
   
end Machine;
