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
  val funct = Reg(io.cmd.inst.funct)
  val resp_rd = Reg(io.resp.bits.rd)
  val taps = Reg(io.cmd.rs2)
  val memsrc = Reg(io.cmd.rs1)
  

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

}