with Debug; use Debug;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

package body Instruction is
   package Random_OpCode is new Ada.Numerics.Discrete_Random (OpCode);
   package Random_Reg is new Ada.Numerics.Discrete_Random (Reg);   
   package Random_Offset is new Ada.Numerics.Discrete_Random (Offset);      
   OpCodeG : Random_OpCode.Generator;
   RegG : Random_Reg.Generator;   
   OffsetG : Random_Offset.Generator;      
   
   procedure Init is
   begin
      Random_OpCode.Reset(OpcodeG);
      Random_Reg.Reset(RegG);
      Random_Offset.Reset(OffsetG);
   end Init;
   
   procedure GenerateRandomInstr(Inst : out Instr) is
      Op : OpCode := Random_OpCode.Random(OpCodeG);
      R1 : Reg := Random_Reg.Random(RegG);      
      R2 : Reg := Random_Reg.Random(RegG);      
      R3 : Reg := Random_Reg.Random(RegG);            
      Offs : Offset := Random_Offset.Random(OffsetG);
   begin
      case Op is
         when ADD =>
            Inst := (Op => ADD, AddRd => R1, AddRs1 => R2, AddRs2 => R3);
            return;
         when SUB =>
            Inst := (Op => SUB, SubRd => R1, SubRs1 => R2, SubRs2 => R3);
            return;
         when MUL =>
            Inst := (Op => MUL, MulRd => R1, MulRs1 => R2, MulRs2 => R3);
            return;
         when DIV =>
            Inst := (Op => DIV, DivRd => R1, DivRs1 => R2, DivRs2 => R3);
            return;
         when RET =>
            Inst := (Op => RET, RetRs => R1);
            return;
         when LDR =>
            Inst := (Op => LDR, LdrRd => R1, LdrRs => R2, LdrOffs => Offs);
            return;
         when STR =>
            Inst := (Op => STR, StrRa => R1, StrOffs => Offs, StrRb => R2);
            return;
         when MOV =>
            Inst := (Op => MOV, MovRd => R1, MovOffs => Offs);
            return;
         when JMP =>
            Inst := (Op => JMP, JmpOffs => Offs);
            return;
         when JZ =>
            Inst := (Op => JZ, JzRa => R1, JzOffs => Offs);
            return;
         when NOP =>
            Inst := (OP => NOP);
      end case;
   end GenerateRandomInstr;

   
   procedure PutReg(R : in Reg) is
   begin
      Put("R");
      Put(Item => Integer(R));
   end PutReg;
   
   procedure PutOffset(Offs : in Offset) is
   begin
      Put(Item => Integer(Offs));
   end PutOffset;
   
   procedure DebugPrintInstr(Inst : in Instr) with SPARK_Mode is
   begin
      Put(Instruction.OpCode'Image(Inst.Op));
      case Inst.Op is
         when ADD =>
            Put(HT); PutReg(Inst.AddRd);
            Put(HT); PutReg(Inst.AddRs1);
            Put(HT); PutReg(Inst.AddRs2);
            return;
         when SUB =>
            Put(HT); PutReg(Inst.SubRd);
            Put(HT); PutReg(Inst.SubRs1);
            Put(HT); PutReg(Inst.SubRs2);
            return;
         when MUL =>
            Put(HT); PutReg(Inst.MulRd);
            Put(HT); PutReg(Inst.MulRs1);
            Put(HT); PutReg(Inst.MulRs2);
            return;
         when DIV =>
            Put(HT); PutReg(Inst.DivRd);
            Put(HT); PutReg(Inst.DivRs1);
            Put(HT); PutReg(Inst.DivRs2);
            return;
         when RET =>
            Put(HT); PutReg(Inst.RetRs);
            return;
         when LDR =>
            Put(HT); PutReg(Inst.LdrRd);
            Put(HT); PutReg(Inst.LdrRs);
            Put(HT); PutOffset(Inst.LdrOffs);
            return;
         when STR =>
            Put(HT); PutReg(Inst.StrRa);
            Put(HT); PutOffset(Inst.StrOffs);
            Put(HT); PutReg(Inst.StrRb);
            return;
         when MOV =>
            Put(HT); PutReg(Inst.MovRd);
            Put(HT); PutOffset(Inst.MovOffs);
            return;
         when JMP =>
            Put(HT); PutOffset(Inst.JmpOffs);
            return;
         when JZ =>
            Put(HT); PutReg(Inst.JzRa);
            Put(HT); PutOffset(Inst.JzOffs);
            return;
         when NOP =>
            return;
      end case;
   end DebugPrintInstr;

end Instruction;
