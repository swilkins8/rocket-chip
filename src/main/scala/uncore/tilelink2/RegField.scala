// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

case class RegReadFn private(combinational: Boolean, fn: (Bool, Bool) => (Bool, Bool, UInt))
object RegReadFn
{
  // (ivalid: Bool, oready: Bool) => (iready: Bool, ovalid: Bool, data: UInt)
  // iready may combinationally depend on oready
  // all other combinational dependencies forbidden (e.g. ovalid <= ivalid)
  // effects must become visible on the cycle after ovalid && oready
  implicit def apply(x: (Bool, Bool) => (Bool, Bool, UInt)) =
    new RegReadFn(false, x)
  // (ready: Bool) => (valid: Bool, data: UInt)
  // valid must not combinationally depend on ready
  // effects must become visible on the cycle after valid && ready
  // ready is only guaranteed to stay high if fed by an Irrevocable (eg: TL2 or concurrency > 0)
  // ... which is irrelevant if you can service the read immediately
  // ... if irrevocable, you may start reading on rising ready and raise valid when done
  // ... otherwise, use the more general in&out ready-valid method above
  implicit def apply(x: Bool => (Bool, UInt)) =
    new RegReadFn(true, { case (_, oready) =>
      val (ovalid, data) = x(oready)
      (Bool(true), ovalid, data)
    })
  // read from a DecoupledIO (only safe if there is a consistent source of data)
  implicit def apply(x: DecoupledIO[UInt]):RegReadFn = RegReadFn(ready => { x.ready := ready; (x.valid, x.bits) })
  // read from a register
  implicit def apply(x: UInt):RegReadFn = RegReadFn(ready => (Bool(true), x))
  // noop
  implicit def apply(x: Unit):RegReadFn = RegReadFn(UInt(0))
}

case class RegWriteFn private(combinational: Boolean, fn: (Bool, Bool, UInt) => (Bool, Bool))
object RegWriteFn
{
  // (ivalid: Bool, oready: Bool, data: UInt) => (iready: Bool, ovalid: Bool)
  // iready may combinationally depend on both oready and data
  // all other combinational dependencies forbidden (e.g. ovalid <= ivalid)
  // effects must become visible on the cycle after ovalid && oready
  implicit def apply(x: (Bool, Bool, UInt) => (Bool, Bool)) =
    new RegWriteFn(false, x)
  // (valid: Bool, data: UInt) => (ready: Bool)
  // ready may combinationally depend on data (but not valid)
  // effects must become visible on the cycle after valid && ready
  // valid is only guaranteed to stay high if fed by an Irrevocable (eg: TL2 or concurrency > 0)
  // ... which is irrelevant if you can service the write immediately
  // ... if irrevocable, you may start writing on rising valid and raise ready when done
  // ... otherwise, use the more general in&out ready-valid method above
  implicit def apply(x: (Bool, UInt) => Bool) =
    // combinational => data valid on oready
    new RegWriteFn(true, { case (_, oready, data) =>
      (Bool(true), x(oready, data))
    })
  // write to a DecoupledIO (only safe if there is a consistent sink draining data)
  implicit def apply(x: DecoupledIO[UInt]): RegWriteFn = RegWriteFn((valid, data) => { x.valid := valid; x.bits := data; x.ready })
  // updates a register
  implicit def apply(x: UInt): RegWriteFn = RegWriteFn((valid, data) => { when (valid) { x := data }; Bool(true) })
  // noop
  implicit def apply(x: Unit): RegWriteFn = RegWriteFn((valid, data) => { Bool(true) })
}

case class RegField(width: Int, read: RegReadFn, write: RegWriteFn)
{
  require (width > 0)
  def pipelined = !read.combinational || !write.combinational
}

object RegField
{
  type Map = (Int, Seq[RegField])
  def apply(n: Int)            : RegField = apply(n, (), ())
  def apply(n: Int, rw: UInt)  : RegField = apply(n, rw, rw)
  def R(n: Int, r: RegReadFn)  : RegField = apply(n, r, ())
  def W(n: Int, w: RegWriteFn) : RegField = apply(n, (), w)
}

trait HasRegMap
{
  def regmap(mapping: RegField.Map*): Unit
}

// See GPIO.scala for an example of how to use regmap