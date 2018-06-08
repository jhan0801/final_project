#include "thumbsim.hpp"
// These are just the register NUMBERS
#define PC_REG 15
#define LR_REG 14
#define SP_REG 13

// These are the contents of those registers
#define PC rf[PC_REG]
#define LR rf[LR_REG]
#define SP rf[SP_REG]

Stats stats;
Caches caches(0);

unsigned int bitCount(unsigned short regList) {
   const int cmp = 1;
   int cnt = 0;

   for (int i = 0; i < 8; i++) {
      if (regList & cmp)
         cnt++;
      regList >>= 1;
   }
   return cnt;
}

// CPE 315: you'll need to implement a custom sign-extension function
// in addition to the ones given below, specifically for the unconditional
// branch instruction, which has an 11-bit immediate field
unsigned int signExtend16to32ui(short i) {
  return static_cast<unsigned int>(static_cast<int>(i));
}

unsigned int signExtend8to32ui(char i) {
  return static_cast<unsigned int>(static_cast<int>(i));
}

// Given a binary instruction, separates the immediate and sign extends it
// from 11 to 32 bits
// Types: short -> int
// Arguments: a binary instruction
// Returns: the offset from the instruction, sign-extended to 32 bits
int signExtend11to32ui(short instruction){
	short i;
	int clear_to = 2^11;
	//clear the 5 leftmost bits (leaves just the offset)
	i = instruction%clear_to;
	return static_cast<int>(i);
}

// This is the global object you'll use to store condition codes N,Z,V,C
// Set these bits appropriately in execute below.
ASPR flags;

// CPE 315: You need to implement a function to set the Negative and Zero
// flags for each instruction that does that. It only needs to take
// one parameter as input, the result of whatever operation is executing
void setNegZero(int num){
	if(num == 0){
		flags.Z = 1;
	}
	else{
		flags.Z = 0;
	}
	if(num < 0){
		flags.N = 1;
	}
	else{
		flags.N = 0;
	}

}

// This function is complete, you should not have to modify it
void setCarryOverflow (int num1, int num2, OFType oftype) {
  switch (oftype) {
    case OF_ADD:
      if (((unsigned long long int)num1 + (unsigned long long int)num2) ==
          ((unsigned int)num1 + (unsigned int)num2)) {
        flags.C = 0;
      }
      else {
        flags.C = 1;
      }
      if (((long long int)num1 + (long long int)num2) ==
          ((int)num1 + (int)num2)) {
        flags.V = 0;
      }
      else {
        flags.V = 1;
      }
      break;
    case OF_SUB:
      if (num1 >= num2) {
        flags.C = 1;
      }
      else if (((unsigned long long int)num1 - (unsigned long long int)num2) ==
          ((unsigned int)num1 - (unsigned int)num2)) {
        flags.C = 0;
      }
      else {
        flags.C = 1;
      }
      if (((num1==0) && (num2==0)) ||
          (((long long int)num1 - (long long int)num2) ==
           ((int)num1 - (int)num2))) {
        flags.V = 0;
      }
      else {
        flags.V = 1;
      }
      break;
    case OF_SHIFT:
      // C flag unaffected for shifts by zero
      if (num2 != 0) {
        if (((unsigned long long int)num1 << (unsigned long long int)num2) ==
            ((unsigned int)num1 << (unsigned int)num2)) {
          flags.C = 0;
        }
        else {
          flags.C = 1;
        }
      }
      // Shift doesn't set overflow
      break;
    default:
      cerr << "Bad OverFlow Type encountered." << __LINE__ << __FILE__ << endl;
      exit(1);
  }
}

// CPE 315: You're given the code for evaluating BEQ, and you'll need to
// complete the rest of these conditions. See Page 208 of the armv7 manual
static int checkCondition(unsigned short cond) {
  switch(cond) {
    case EQ:
      if (flags.Z == 1) {
        return TRUE;
      }
      break;
    case NE:
	if (flags.Z == 0){
		return TRUE;
	}
      break;
    case CS:
	if(flags.C == 1){
		return TRUE;
	}
      break;
    case CC:
	if(flags.C == 0){
		return TRUE;
	}
      break;
    case MI:
      	if(flags.N == 1){
		return TRUE;
	}
      break;
    case PL:
      if(flags.N == 0){
      	return TRUE;
      }
      break;
    case VS:
      if(flags.V == 1){
      	return TRUE;
      }
      break;
    case VC:
      if(flags.V == 0){
      	return TRUE;
      }
      break;
    case HI:
      if(flags.C == 1 && flags.Z == 0){
      	return TRUE;
      }
      break;
    case LS:
      if(flags.C == 0 || flags.Z == 1){
      	return TRUE;
      }
      break;
    case GE:
      if(flags.N == flags.V){
      	return TRUE;
      }
      break;
    case LT:
      if(flags.N != flags.V){
      	return TRUE;
      }
      break;
    case GT:
      if(flags.Z == 0 && flags.N == flags.V){
      	return TRUE;
      }
      break;
    case LE:
      if(flags.Z == 1 || flags.N != flags.V){
      	return TRUE;
      }
      break;
    case AL:
      return TRUE;
      break;
  }
  return FALSE;
}

void execute() {
  Data16 instr = imem[PC];
  Data16 instr2;
  Data32 temp(0); // Use this for STRB instructions
  Thumb_Types itype;
  // the following counts as a read to PC
  unsigned int pctarget = PC + 2;
  unsigned int addr;
  int i, n, offset;
  unsigned int list, mask;
  int num1, num2, result, BitCount, word;
  unsigned int bit;
  unsigned int word_addr, byte_index, val;

  /* Convert instruction to correct type */
  /* Types are described in Section A5 of the armv7 manual */
  BL_Type blupper(instr);
  ALU_Type alu(instr);
  SP_Type sp(instr);
  DP_Type dp(instr);
  LD_ST_Type ld_st(instr);
  MISC_Type misc(instr);
  COND_Type cond(instr);
  UNCOND_Type uncond(instr);
  LDM_Type ldm(instr);
  STM_Type stm(instr);
  LDRL_Type ldrl(instr);
  ADD_SP_Type addsp(instr);

  BL_Ops bl_ops;
  ALU_Ops add_ops;
  DP_Ops dp_ops;
  SP_Ops sp_ops;
  LD_ST_Ops ldst_ops;
  MISC_Ops misc_ops;

  // This counts as a write to the PC register
  rf.write(PC_REG, pctarget);

  itype = decode(ALL_Types(instr));
  stats.numRegReads++;
  stats.numRegWrites++;

  // CPE 315: The bulk of your work is in the following switch statement
  // All instructions will need to have stats and caches access info added
  // as appropriate for that instruction.
  switch(itype) {
    case ALU:
      add_ops = decode(alu);
      switch(add_ops) {
        case ALU_LSLI:
          // lsls r2, r2, #2
          // 1 reg read, 1 reg write
          setCarryOverflow(rf[alu.instr.lsli.rm], alu.instr.lsli.imm, OF_SHIFT);
          setNegZero(rf[alu.instr.lsli.rm] << alu.instr.lsli.imm);
          stats.numRegReads++;
          stats.numRegWrites++;
          rf.write(alu.instr.lsli.rd, rf[alu.instr.lsli.rm] << alu.instr.lsli.imm);
          break;
        case ALU_ADDR:
          // N, Z, C, V flags set, 2 reg reads, 1 reg write, no mem access
      	  setCarryOverflow(rf[alu.instr.addr.rn], rf[alu.instr.addr.rm], OF_ADD);
      	  setNegZero(rf[alu.instr.addr.rn] + rf[alu.instr.addr.rm]);
          stats.numRegReads += 2;
	  stats.numRegWrites += 1;
          rf.write(alu.instr.addr.rd, rf[alu.instr.addr.rn] + rf[alu.instr.addr.rm]);
          break;
        case ALU_SUBR:
      	  // N, Z, C, V flags set, reg reads + 2, writes + 1, no mem access
      	  setCarryOverflow(rf[alu.instr.subr.rn], rf[alu.instr.subr.rm], OF_SUB);
      	  setNegZero(rf[alu.instr.subr.rn] - rf[alu.instr.subr.rm]);
      	  stats.numRegReads += 2;
      	  stats.numRegWrites += 1;
      	  rf.write(alu.instr.subr.rd, rf[alu.instr.subr.rn] - rf[alu.instr.subr.rm]);
           break;
        case ALU_ADD3I:
          // N, Z, C, V flags set, reg reads and writes each incremented, no mem access
          stats.numRegReads += 1;
	  stats.numRegWrites += 1;
	  setCarryOverflow(rf[alu.instr.add3i.rn], alu.instr.add3i.imm, OF_ADD);
	  setNegZero(rf[alu.instr.add3i.rn] + alu.instr.add3i.imm);
          rf.write(alu.instr.add3i.rd, rf[alu.instr.add3i.rn] + alu.instr.add3i.imm);
          break;
        case ALU_SUB3I:
	  // N, Z, C, V flags set, reg reads and writes incremented, no mem access
	  stats.numRegReads += 1;
	  stats.numRegWrites += 1;
	  setCarryOverflow(rf[alu.instr.sub3i.rn], alu.instr.sub3i.imm, OF_SUB);
	  setNegZero(rf[alu.instr.sub3i.rn] - alu.instr.sub3i.imm);
	  rf.write(alu.instr.sub3i.rd, rf[alu.instr.sub3i.rn] - alu.instr.sub3i.imm);
          break;
        case ALU_MOV:
          // 1 reg read, 1 reg write, no mem access, N, Z flags set
	  setNegZero(rf[alu.instr.mov.rdn]);
          stats.numRegReads += 1;
	  stats.numRegWrites += 1;
          rf.write(alu.instr.mov.rdn, alu.instr.mov.imm);
          break;
        case ALU_CMP:
         // 1 reg read, 0 reg writes, no mem access, N, Z, C, V flags set
          stats.numRegReads++;
          setNegZero(rf[alu.instr.cmp.rdn] - alu.instr.cmp.imm);
          setCarryOverflow(rf[alu.instr.cmp.rdn], alu.instr.cmp.imm, OF_SUB);
          break;
        case ALU_ADD8I:
          // 1 reg read, 1 reg write, no mem access, N, Z, C, V flags set
          stats.numRegReads++;
      	  stats.numRegWrites++;
      	  setNegZero(rf[alu.instr.add8i.rdn] + alu.instr.add8i.imm);
      	  setCarryOverflow(rf[alu.instr.add8i.rdn], alu.instr.add8i.imm, OF_ADD);
          rf.write(alu.instr.add8i.rdn, rf[alu.instr.add8i.rdn] + alu.instr.add8i.imm);
          break;
        case ALU_SUB8I:
          // 1 reg read, 1 reg write, no mem access, N, Z, C, V flags set
          setNegZero(rf[alu.instr.sub8i.rdn] - alu.instr.sub8i.imm);
          setCarryOverflow(rf[alu.instr.sub8i.rdn], alu.instr.sub8i.imm, OF_SUB);
          rf.write(alu.instr.sub8i.rdn, rf[alu.instr.sub8i.rdn] - alu.instr.sub8i.imm);
	  stats.numRegReads++;
	  stats.numRegWrites++;
          break;
        default:
          cout << "instruction not implemented" << endl;
          exit(1);
          break;
      }
      break;
    case BL:
      // This instruction is complete, nothing needed here
      bl_ops = decode(blupper);
      if (bl_ops == BL_UPPER) {
        // PC has already been incremented above
        instr2 = imem[PC];
        BL_Type bllower(instr2);
        if (blupper.instr.bl_upper.s) {
          addr = static_cast<unsigned int>(0xff<<24) |
            ((~(bllower.instr.bl_lower.j1 ^ blupper.instr.bl_upper.s))<<23) |
            ((~(bllower.instr.bl_lower.j2 ^ blupper.instr.bl_upper.s))<<22) |
            ((blupper.instr.bl_upper.imm10)<<12) |
            ((bllower.instr.bl_lower.imm11)<<1);
        }
        else {
          addr = ((blupper.instr.bl_upper.imm10)<<12) |
            ((bllower.instr.bl_lower.imm11)<<1);
        }
        // return address is 4-bytes away from the start of the BL insn
        rf.write(LR_REG, PC + 2);
        // Target address is also computed from that point
        rf.write(PC_REG, PC + 2 + addr);

        stats.numRegReads += 1;
        stats.numRegWrites += 2;
      }
      else {
        cerr << "Bad BL format." << endl;
        exit(1);
      }
      break;
    case DP:
      dp_ops = decode(dp);
      switch(dp_ops) {
        case DP_CMP:
	  // 2 reg reads, 0 reg writes, no mem access, N, Z, C, V flags set
	  stats.numRegReads += 2;
	  setNegZero(rf[dp.instr.DP_Instr.rdn] - rf[dp.instr.DP_Instr.rm]);
	  setCarryOverflow(rf[dp.instr.DP_Instr.rdn], rf[dp.instr.DP_Instr.rm], OF_SUB);
          break;
      }
      break;
    case SPECIAL:
      sp_ops = decode(sp);
      switch(sp_ops) {
        case SP_MOV:
         // 1 reg read, 1 reg write, no mem access
      	 stats.numRegReads++;
      	 stats.numRegWrites++;
         rf.write((sp.instr.mov.d << 3 ) | sp.instr.mov.rd, rf[sp.instr.mov.rm]);
         break;
        case SP_ADD:
   	 // 2 reg reads, 1 reg write, no mem access
         rf.write((sp.instr.add.d << 3) | sp.instr.add.rd, rf[sp.instr.add.rd] + rf[sp.instr.add.rm]);
 	 stats.numRegReads += 2;
   	 stats.numRegWrites++;
        case SP_CMP:
      	 // 2 reg reads, 0 reg writes, no mem access, N, Z, C, V flags set
      	 stats.numRegReads += 2;
         setNegZero(((sp.instr.cmp.d << 3) | rf[sp.instr.cmp.rd]) - rf[sp.instr.cmp.rm]);
	      setCarryOverflow(sp.instr.cmp.d << 3 | rf[sp.instr.cmp.rd], rf[sp.instr.cmp.rm], OF_SUB);
         break;
      }
      break;
    case LD_ST:
      // You'll want to use these load and store models
      // to implement ldrb/strb, ldm/stm and push/pop
      ldst_ops = decode(ld_st);
      switch(ldst_ops) {
        case STRI:
         // 2 reg reads, 0 reg writes, 0 mem reads, 1 mem write
         addr = rf[ld_st.instr.ld_st_imm.rn] + (ld_st.instr.ld_st_imm.imm * 4);
         dmem.write(addr, rf[ld_st.instr.ld_st_imm.rt]);
      	 stats.numRegReads += 2;
      	 stats.numMemWrites++;
      	 caches.access(addr);
          break;
        case LDRI:
	  // 1 reg reads, 1 reg writes, 1 mem read, 0 mem writes
          addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm * 4;
          rf.write(ld_st.instr.ld_st_imm.rt, dmem[addr]);
      	  stats.numRegReads++;
      	  stats.numRegWrites++;
      	  stats.numMemReads++;
      	  caches.access(addr);
           break;
        case STRR:
      	  // 3 reg reads, 0 reg writes, 0 mem reads, 1 mem write
      	  addr = rf[ld_st.instr.ld_st_reg.rn] + rf[ld_st.instr.ld_st_reg.rm] * 4;
      	  dmem.write(addr, rf[ld_st.instr.ld_st_reg.rt]);
      	  stats.numRegReads += 3;
      	  stats.numMemWrites++;
      	  caches.access(addr);
          break;
        case LDRR:
      	  // 2 reg reads, 1 reg write, 1 mem read, 0 mem writes
      	  addr = rf[ld_st.instr.ld_st_reg.rn] + rf[ld_st.instr.ld_st_reg.rm] * 4;
      	  rf.write(ld_st.instr.ld_st_reg.rt, dmem[addr]);
      	  stats.numRegReads += 2;
      	  stats.numRegWrites++;
      	  stats.numMemReads++;
      	  caches.access(addr);
           break;
        case STRBI:
          // 2 reg reads, 0 reg writes, 0 mem reads, 1 mem write, no flag updates
          stats.numRegReads += 2;
	  stats.numMemWrites++;
	  addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm;
	  caches.access(addr);
	  temp = dmem[addr]; // get everything but the byte index (up until the last 2 bits)
	  temp.set_data_ubyte4(0, rf[ld_st.instr.ld_st_imm.rt]); // set the byte to the value in rt
    	  dmem.write(addr, temp);
          break;
        case LDRBI:
	  // 1 reg read, 1 reg write, 1 mem read, 0 mem writes, no flag updates
	  stats.numRegReads++;
	  stats.numRegWrites++;
	  stats.numMemReads++;
	  addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm;
	  caches.access(addr);
	  val = signExtend8to32ui(dmem[addr].data_ubyte4(0));
	  rf.write(ld_st.instr.ld_st_imm.rt, val);
          break;
        case STRBR:
	  // 3 reg reads, 0 reg writes, 0 mem reads, 1 mem write, no flag updates
	  addr = rf[ld_st.instr.ld_st_imm.rn] + rf[ld_st.instr.ld_st_reg.rm];
	  temp = dmem[addr]; // get everything but the byte index (up until the last 2 bits)
	  temp.set_data_ubyte4(0, rf[ld_st.instr.ld_st_imm.rt]); // set the byte to the value in rt
          dmem.write(addr, temp);
          stats.numRegReads += 3;
	  stats.numMemWrites++;
	  caches.access(addr);
          break;
        case LDRBR:
	  // 2 reg reads, 1 reg write, 1 mem read, 0 mem writes, no flag updates
	  stats.numRegReads += 2;
	  stats.numRegWrites++;
	  stats.numMemReads++;
	  addr = rf[ld_st.instr.ld_st_imm.rn] + rf[ld_st.instr.ld_st_reg.rm]; // calculate address
	  val = signExtend8to32ui(dmem[addr].data_ubyte4(0));
	  rf.write(ld_st.instr.ld_st_imm.rt, val);
	  caches.access(addr);
          break;
      }
      break;
    case MISC:
      misc_ops = decode(misc);
      switch(misc_ops) {
        case MISC_PUSH:
          // need to implement
          if (misc.instr.push.m == 1) {
            addr = SP - 4;
            dmem.write(addr, LR);
            rf.write(SP_REG, SP - 4);
	    stats.numMemWrites++;
	    stats.numRegWrites++;
	    stats.numRegReads += 2;
          }
          if (misc.instr.push.reg_list != 0) {
             addr -= (4 * bitCount(misc.instr.push.reg_list));
             rf.write(SP_REG, SP - 4*bitCount(misc.instr.push.reg_list));
	     stats.numRegReads++;
	     stats.numRegWrites++;
             unsigned short tmp = misc.instr.push.reg_list;
             for (int i = 0; i < 8; i++) {
                if (tmp & 1) {
                   dmem.write(addr, rf[i]);
                   addr += 4;
		   stats.numMemWrites++;
		   stats.numRegReads++;
                }
                tmp >>= 1;
             }
          }
          break;
        case MISC_POP:
          if (misc.instr.pop.reg_list != 0) {
             addr = SP;
             unsigned short tmp = misc.instr.pop.reg_list;
             for (int i = 0; i < 8; i++) {
                if (tmp & 1) {
                   rf.write(i, dmem[addr]);
                   addr += 4;
		   stats.numMemReads++;
		   stats.numRegWrites++;
                }
                tmp >>= 1;
             }
             rf.write(SP_REG, SP + 4*bitCount(misc.instr.push.reg_list));
	     stats.numRegReads++;
	     stats.numRegWrites++;
          }
          if (misc.instr.pop.m == 1) {
            rf.write(PC_REG, dmem[addr]);
            rf.write(SP_REG, SP + 4);
	    stats.numRegWrites += 2;
	    stats.numMemReads++;
	    stats.numRegReads++;
          }
          break;
        case MISC_SUB:
      	  // 1 reg read, 1 reg write, no memory access, no flag updates
      	  stats.numRegReads++;
      	  stats.numRegWrites++;
           rf.write(SP_REG, SP - (misc.instr.sub.imm*4));
           break;
        case MISC_ADD:
      	  // 1 reg read, 1 reg write, no memory access, no flag updates
      	 stats.numRegReads++;
      	 stats.numRegWrites++;
          rf.write(SP_REG, SP + (misc.instr.add.imm*4));
          break;
      }
      break;
    case COND:
      decode(cond);
      // Once you've completed the checkCondition function,
      // this should work for all your conditional branches.
      stats.instrs++;
      if (checkCondition(cond.instr.b.cond)){
        rf.write(PC_REG, PC + 2 * signExtend8to32ui(cond.instr.b.imm) + 2);
	if((cond.instr.b.imm & (unsigned int)exp2(7)) != 0){
	  stats.numBackwardBranchesTaken++;
	}
	else{
	  stats.numForwardBranchesTaken++;
	}
	stats.numRegWrites++;
	stats.numRegReads++;
      }
      else{
        if((cond.instr.b.imm & (unsigned int)exp2(7)) != 0){
          stats.numBackwardBranchesNotTaken++;
        }
        else{
          stats.numForwardBranchesNotTaken++;
        }
      }
      break;
    case UNCOND:
      // Essentially the same as the conditional branches, but with no
      // condition check, and an 11-bit immediate field
      // 1 reg write
      decode(uncond);
      rf.write(PC_REG, PC + 2 * signExtend8to32ui(cond.instr.b.imm) + 2);
      stats.numRegWrites++;
      stats.numRegReads++;
      break;
    case LDM:
      decode(ldm);
      // need to implement
      if (ldm.instr.ldm.reg_list != 0) {
         addr = rf[ldm.instr.ldm.rn] - (4*bitCount(ldm.instr.ldm.reg_list));
	 stats.numRegReads++;
         unsigned short tmp = ldm.instr.ldm.reg_list;
         for (int i = 0; i < 8; i++) {
            if (tmp & 1) {
               rf.write(rf[i], dmem[addr]);
               addr += 4;
	       stats.numMemReads++;
	       stats.numRegWrites++;
            }
            tmp >>= 1;
         }
      }
      caches.access(addr);
      break;
    case STM:
      decode(stm);
      // need to implement
      if (stm.instr.stm.reg_list != 0) {
         addr = rf[stm.instr.stm.rn] - (4*bitCount(stm.instr.stm.reg_list));
	 stats.numRegReads++;
         unsigned short tmp = stm.instr.stm.reg_list;
         for (int i = 0; i < 8; i++) {
            if (tmp & 1) {
               dmem.write(addr, rf[i]);
               addr += 4;
	       stats.numRegReads++;
	       stats.numMemWrites++;
            }
            tmp >>= 1;
         }
      }
      caches.access(addr);
      break;
    case LDRL:
      // This instruction is complete, nothing needed
      decode(ldrl);
      // Need to check for alignment by 4
      if (PC & 2) {
        addr = PC + 2 + (ldrl.instr.ldrl.imm)*4;
      }
      else {
        addr = PC + (ldrl.instr.ldrl.imm)*4;
      }
      // Requires two consecutive imem locations pieced together
      temp = imem[addr] | (imem[addr+2]<<16);  // temp is a Data32
      rf.write(ldrl.instr.ldrl.rt, temp);
      // One write for updated reg
      stats.numRegWrites++;
      // One read of the PC
      stats.numRegReads++;
      // One mem read, even though it's imem, and there's two of them
      stats.numMemReads++;
      break;
    case ADD_SP:
      // 1 reg write, 1 reg read, no mem access
      decode(addsp);
      stats.numRegWrites++;
      stats.numRegReads++;
      rf.write(addsp.instr.add.rd, SP + (addsp.instr.add.imm*4));
      break;
    default:
      cout << "[ERROR] Unknown Instruction to be executed" << endl;
      exit(1);
      break;
  }
}
