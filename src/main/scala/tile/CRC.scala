package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter

class CRCAccelerator(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new CRCAcceleratorModule(this)
}

class CRCAcceleratorModule(outer: CRCAccelerator)(implicit p: Parameters) extends LazyRoCCModule(outer)
with HasCoreParameters {
  // The parts of the command are as follows
  // inst - the parts of the instruction itself
  //   opcode
  //   rd - destination register number
  //   rs1 - first source register number
  //   rs2 - second source register number
  //   funct
  //   xd - is the destination register being used?
  //   xs1 - is the first source register being used?
  //   xs2 - is the second source register being used?
  // rs1 - the value of source register 1
  // rs2 - the value of source register 2
  val resp_rd = Reg(io.resp.bits.rd)

  // The 32 bits that determine the location of the XOR taps
  val taps = Reg(Vec(32, Bool()))

  val length = Reg(UInt(width = 10))
  // The memory location to read for CRC calculation
  val memsrc = Reg(UInt(width = coreMaxAddrBits))
  // The length (in bits) of the data to calculate the CRC across
  val memlen = Reg(UInt(width = 32))
  // The current global data offset (in bits)
  val globalCount = Reg(UInt(width = 32))
  // The current block data offset (in bits)
  val localIndex = Reg(UInt(width = 16))

  val startVal = Reg(UInt(width = 64), init = UInt(0))
  val finalXOR = Reg(UInt(width = 32), init = UInt(0))

  val flipInput = Reg(Bool(), init = false.B)
  val flipOutput = Reg(Bool(), init = false.B)

  val crcState = Reg(Vec(32, Bool()))
  val recv_data = Reg(UInt(width = 64)) //Get 64 bits per load

  val s_idle :: s_update :: s_acq :: s_recv :: s_calc :: s_zeroes:: s_resp :: Nil = Enum(Bits(), 7)
  //  Idle, update len, acq data, recv data, calc CRC, responding
  val state = Reg(init = s_idle)

  // Ready to receive new command when in idle state
  io.cmd.ready := (state === s_idle)
  // Cmd response is valid when in response state
  io.resp.valid := (state === s_resp)
  io.resp.bits.data := Reverse(Cat(crcState))
  io.resp.bits.rd := resp_rd


  // Command received from CPU
  when (io.cmd.fire()) {
    // Funct of 0 indicate updating type/length
    resp_rd := io.cmd.bits.inst.rd
    val funct = io.cmd.bits.inst.funct
    when (funct === UInt(0)) {
      // Update taps for CRC polynomial
      length := io.cmd.bits.rs2(9, 0)
      for (i <- 0 to 31) {
        taps(i) := io.cmd.bits.rs1(i)
      }
      state := s_resp
    } .elsewhen (funct === UInt(1)) {
      //Just update the start and final values
      startVal := io.cmd.bits.rs1
      finalXOR := io.cmd.bits.rs2
      state := s_resp
    } .elsewhen (funct === UInt(2)) {
      when (io.cmd.bits.rs1 === UInt(0)) {
        flipInput := false.B
      } .otherwise {
        flipInput := true.B
      }
      when (io.cmd.bits.rs2 === UInt(0)) {
        flipOutput := false.B
      } .otherwise {
        flipOutput := true.B
      }
      state := s_resp
    } .otherwise {
      memsrc := io.cmd.bits.rs1
      memlen := io.cmd.bits.rs2

      globalCount := UInt(0)
      for (i <- 0 to 31) {
        crcState(i) := false.B
      }
      state := s_acq
    }
  }

  // Memory request sent
  when (state === s_acq) {
    when (io.mem.req.fire()) {
      state := s_recv
    }
  }

  // Memory request received
  when (state === s_recv) {
    when (io.mem.resp.valid) {
      val shiftAmt = UInt(64) - length
      val shifted = startVal << shiftAmt
      val memData = io.mem.resp.bits.data ^ shifted

/*      val allBytes = new Array[UInt](8)
      for (i <- 7 to 0) {
        val currByte = memData(i*8 + 7, i*8)
        allBytes(i) := Mux(flipInput === true.B, Reverse(currByte), currByte)
      }
      val halfBytes = new Array[UInt](4)
      for (i <- 3 to 0) {
        halfBytes(i) = Cat(allBytes(i*2 + 1), allBytes(i*2))
      }
      val quarterBytes = new Array[UInt](2)
      for (i <- 1 to 0) {
        quarterBytes(i) = Cat(halfBytes(i*2 + 1), halfBytes(i*2))
      }
      val endData = Cat(quarterBytes(1), quarterBytes(0))*/

      val byte1 = memData(63, 56)
      val byte2 = memData(55, 48)
      val byte3 = memData(47, 40)
      val byte4 = memData(39, 32)
      val byte5 = memData(31, 24)
      val byte6 = memData(23, 16)
      val byte7 = memData(15, 8)
      val byte8 = memData(7, 0)
      //val allBytes = Array(byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8)
      val finalByte1 = Mux(flipInput === true.B, Reverse(byte1), byte1)
      val finalByte2 = Mux(flipInput === true.B, Reverse(byte2), byte2)
      val finalByte3 = Mux(flipInput === true.B, Reverse(byte3), byte3)
      val finalByte4 = Mux(flipInput === true.B, Reverse(byte4), byte4)
      val finalByte5 = Mux(flipInput === true.B, Reverse(byte5), byte5)
      val finalByte6 = Mux(flipInput === true.B, Reverse(byte6), byte6)
      val finalByte7 = Mux(flipInput === true.B, Reverse(byte7), byte7)
      val finalByte8 = Mux(flipInput === true.B, Reverse(byte8), byte8)

      val endData = Cat(finalByte1, finalByte2, finalByte3, finalByte4, finalByte5, finalByte6, finalByte7, finalByte8)

      recv_data := endData
      startVal := UInt(0)
      localIndex := UInt(64)
      state := s_calc
    }
  }

  when (state === s_calc) {
    when (globalCount === memlen) {
      localIndex := UInt(0)
      state := s_zeroes
    } .elsewhen (localIndex === UInt(0)) {
      memsrc := memsrc + UInt(8)
      state := s_acq
    } .otherwise {
      //Use localIndex - 1 because localIndex = 0 is the escape from the current block
      val normalIndex = localIndex - UInt(1)
      val reversedIndex = UInt(64) - localIndex
      val index = normalIndex //Mux(flipInput === true.B, reversedIndex, normalIndex)
      val dataBit = recv_data(index)
      val finalBit = crcState(length - UInt(1))
      
      for (i <- 1 to 31) {
        val bitXOR = crcState(i - 1) ^ finalBit
        crcState(i) := Mux(taps(i), bitXOR, crcState(i - 1))
      }
      val bitXOR = dataBit ^ finalBit
      crcState(0) := Mux(taps(0), bitXOR, dataBit)

      localIndex := localIndex - UInt(1)
      globalCount := globalCount + UInt(1)
    }
  }

  when (state === s_zeroes) {
    when (localIndex >= length) {
      val finalState = Reverse(Cat(crcState))
      val shiftAmt = UInt(32) - length
      val reversed = Reverse(finalState) >> shiftAmt
      val finalVal = Mux(flipOutput === true.B, reversed, finalState)
      val mask = ~(SInt(0xFFFFFFFF) << length)
      //Mux(flipOutput =/= UInt(0), Cat(crcState), 
      for (i <- 0 to 31) {
        crcState(i) := (finalVal(i) ^ finalXOR(i)) & mask(i)
      }
      state := s_resp
    } .otherwise {
      val dataBit = UInt(0)
      val finalBit = crcState(length - UInt(1))
      for (i <- 1 to 31) {
        val bitXOR = crcState(i - 1) ^ finalBit
        crcState(i) := Mux(taps(i), bitXOR, crcState(i - 1))
      }
      val bitXOR = dataBit ^ finalBit
      crcState(0) := Mux(taps(0), bitXOR, dataBit)
      localIndex := localIndex + UInt(1)
    }
  }

  // Response sent back to CPU
  when (io.resp.fire()) {
    state := s_idle
  }

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := (state === s_acq)
  io.mem.req.bits.addr := memsrc
  io.mem.req.bits.tag := memsrc(5, 0)
  io.mem.req.bits.cmd := M_XRD // perform a load (M_XWR for stores)
  io.mem.req.bits.typ := MT_D // D = 8 bytes, W = 4, H = 2, B = 1
  io.mem.req.bits.data := Bits(0) // we're not performing any stores...
  io.mem.req.bits.phys := Bool(false)
  io.mem.invalidate_lr := Bool(false)
}