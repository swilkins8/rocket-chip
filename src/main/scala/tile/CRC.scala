class CRC_Accelerator(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new CustomAcceleratorModule(this)
  override val atlNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters("CharacterCountRoCC")))))
}

class CRC_AcceleratorModule(outer: CustomAccelerator) extends LazyRoCCModule(outer)
with HasCoreParameters
with HasL1CacheParameters {
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

  // Used to determine if updating length/type or CRC calculation
  val cacheParams = tileParams.icache.get

  // Cache block offset, used for address calculation
  private val blockOffset = blockOffBits 
  // Offset into the cache block, to grab bytes for CRC calculation
  private val beatOffset = log2Up(cacheDataBits/8)

  val resp_rd = Reg(io.resp.bits.rd)

  // The 32 bits that determine the location of the XOR taps
  val taps = Reg(UInt(width = 32))
  // The bitmask that determines the final CRC length
  val outMask = Reg(UInt(width = 32))
  // The memory location to read for CRC calculation
  val memsrc = Reg(UInt(width = coreMaxAddrBits))
  // The length (in bytes) of the data to calculate the CRC across
  val memlen = Reg(UInt(width = 32))
  // The current data offset (in bytes)
  val count = Reg(UInt(width = 32))

  val addr_block = memsrc(coreMaxAddrBits - 1, blockOffset)
  val offset = memsrc(blockOffset - 1, 0)
  val next_addr = (addr_block + UInt(1)) << UInt(blockOffset)

  // Used to read memory for CRC calculation
  val (tl_out, edgesOut) = outer.atlNode.out(0)
  // The received data from the mem request
  val gnt = tl_out.d.bits
  // The cache line
  val recv_data = Reg(UInt(width = cacheDataBits))
  // The current byte
  val recv_beat = Reg(UInt(width = log2Up(cacheDataBeats+1)), init = UInt(0))


  

  val s_idle :: s_update :: s_acq :: s_recv :: s_calc :: s_resp :: Nil = Enum(Bits(), 6)
  //  Idle, update len, acq data, recv data, calc CRC, responding
  val state = Reg(init = s_idle)

  // Ready to receive new command when in idle state
  io.cmd.ready := (state === s_idle)
  // Cmd response is valid when in response state
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := count

  // Request is valid in the acq state
  tl_out.a.valid := (state === s_acq)
  // Mem address
  tl_out.a.bits := edgesOut.Get(
                       fromSource = UInt(0),
                       toAddress = addr_block << blockOffset,
                       lgSize = UInt(lgCacheBlockBytes))._2
  // Receive is valid in the recv state
  tl_out.d.ready := (state === s_recv)

  // Command received from CPU
  when (io.cmd.fire()) {
    // Funct of 0 indicate updating type/length
    when (io.cmd.bits.funct === 0) {
      taps := io.cmd.bits.rs1
      val length = Uint(io.cmd.bits.rs2)
      when (length < 32) {
        outMask := Uint(0) + Fill(length, "1")
      }
      // Is update state actually needed?
      // If we're doing hardcoded type checks, yes
      state := s_update
    } .otherwise {
      memsrc := io.cmd.bits.rs1
      resp_rd := io.cmd.bits.inst.rd
      count := UInt(0)
    }
  }

  // Update taps/length
  when (state === s_update) {
    when (taps === UInt(0)) {
      // TODO:  Set taps & mask based on table of predefined types
    }
    state := s_idle
  }

  // Memory request sent
  when (tl_out.a.fire()) {
    state := s_recv
  }

  // Memory request received
  when (tl_out.d.fire()) {
    recv_beat := recv_beat + UInt(1)
    recv_data := gnt.data
    state := s_calc
  }

  when (state === s_calc) {
    //TODO:  Calculate CRC
  }

  // Response sent back to CPU
  when (io.resp.fire()) {
    state := s_idle
  }

  io.busy := (state =/= s_idle)
  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
  // Tie off unused channels
  tl_out.b.ready := Bool(true)
  tl_out.c.valid := Bool(false)
  tl_out.e.valid := Bool(false)
}