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
  // The bitmask that determines the final CRC length
  val outMask = Reg(UInt(width = 32))
  // The memory location to read for CRC calculation
  val memsrc = Reg(UInt(width = coreMaxAddrBits))
  // The length (in bits) of the data to calculate the CRC across
  val memlen = Reg(UInt(width = 32))
  // The current global data offset (in bits)
  val globalCount = Reg(UInt(width = 32))
  // The current block data offset (in bits)
  val localIndex = Reg(UInt(width = 16))

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
    when (io.cmd.bits.inst.funct === UInt(0)) {
      // Update taps for CRC polynomial
      val lengthInput = io.cmd.bits.rs2(9, 0)
      length := lengthInput
      when (lengthInput < UInt(33)) {
        outMask := ~(Fill(32, true.B) << lengthInput)
      }
      val shiftAmt = UInt(32) - lengthInput
      val shifted = io.cmd.bits.rs1 << shiftAmt
      for (i <- 0 to 31) {
        taps(i) := shifted(i)
      }
      /*for (i <- 10 to 31) {
        taps(i) := false.B
      }*/
      state := s_resp
      //taps := (io.cmd.bits.rs1) << (shiftAmt)
    } .otherwise {
      memsrc := io.cmd.bits.rs1
      memlen := io.cmd.bits.rs2

      globalCount := UInt(0)
      crcState := Vec.fill(32){false.B}
      // Should be good to go with the memory
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
      recv_data := io.mem.resp.bits.data
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
      //TODO:  Calculate CRC
      //Use localIndex - 1 because localIndex = 0 is the escape from the current block
      val dataBit = recv_data(localIndex - UInt(1))
      val finalBit = crcState(31)
      
      for (i <- 1 to 31) {
        val bitXOR = crcState(i - 1) ^ finalBit
        crcState(i) := Mux(taps(i), bitXOR, crcState(i - 1))
      }
      val bitXOR = dataBit ^ finalBit
      crcState(0) := Mux(taps(0), bitXOR, dataBit)
      /*for (i <- 0 to 31) {
        crcState(i) := recv_data(i)
      }*/;
      localIndex := localIndex - UInt(1)
      globalCount := globalCount + UInt(1)
    }
  }

  when (state === s_zeroes) {
    when (localIndex >= UInt(32)) {
      //val masked = Cat(crcState) & outMask
      val shiftAmt = UInt(32) - length
      val shifted = Reverse(Cat(crcState)) >> shiftAmt
      val masked = shifted & outMask
      for (i <- 0 to 31) {
        crcState(i) := shifted(i)  
      }
      state := s_resp
    } .otherwise {
      val dataBit = UInt(0)
      val finalBit = crcState(31)
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