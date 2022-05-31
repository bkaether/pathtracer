import spatial.dsl._

@spatial object PathTracingGPU extends SpatialApp {
  def main(args: Array[String]): Unit = {
//    type T = FixPt[TRUE, _32, _32]
    type T = FixPt[TRUE, _20, _20]
//    type O = FixPt[FALSE, _3, _3]
    type O = Byte
    type F = Float

    val ADD = 0.to[O]
    val SUB = 1.to[O]
    val MUL = 2.to[O]
    val DIV = 3.to[O]
    val NUL = -1.to[O]
    val argOutTest = ArgOut[I32]
    val _0 = I32(0)
    val _1 = I32(1)
    val ip = I32(4)

    val nInstructions = I32(4)
    val data0DRAM = DRAM[F](nInstructions)
    val data1DRAM = DRAM[F](nInstructions)
    val opsDRAM = DRAM[O](nInstructions)
    setMem(data0DRAM, Array.fromSeq(Seq[F](1.to[F], 2.to[F], 3.14.to[F], 5.12354234.to[F])))
    setMem(data1DRAM, Array.fromSeq(Seq[F](13.3254234.to[F], 12321.to[F], 12.to[F], 9.232.to[F])))
    setMem(opsDRAM, Array.fromSeq(Seq[O](ADD, SUB, MUL, DIV)))


    Accel {
//      val data0 = LUT[F](4)(1.to[F], 2.to[F], 3.14.to[F], 5.12354234.to[F])
//      val data1 = LUT[F](4)(13.3254234.to[F], 12321.to[F], 12.to[F], 9.232.to[F])

//      val data0 = LUT[T](4)(1.to[T], 2.to[T], 3.14.to[T], 5.12354234.to[T])
//      val data1 = LUT[T](4)(13.3254234.to[T], 12321.to[T], 12.to[T], 9.232.to[T])
//      val ops = LUT[O](4)(ADD, SUB, MUL, DIV)

      val data0 = SRAM[F](nInstructions)
      val data1 = SRAM[F](nInstructions)
      val ops = SRAM[O](nInstructions)

      data0 load data0DRAM
      data1 load data1DRAM
      ops load opsDRAM

      class FPU {
        val lhsQueue: FIFO[T] = FIFO[T](ip)
        val rhsQueue: FIFO[T] = FIFO[T](ip)
        val opQueue: FIFO[O] = FIFO[O](ip)
        val resultQueue: FIFO[T] = FIFO[T](ip)

        def exec(): Void = {
          val lv = lhsQueue.deq()
          val rv = rhsQueue.deq()
          val op = opQueue.deq()
          val addVal = lv + rv
          val divVal = lv / rv
          val mulVal = lv * rv
          val subVal = lv - rv
          resultQueue.enq(mux(op == ADD, addVal, mux(op == SUB, subVal, mux(op == DIV, divVal, mulVal))))
        }

        def enq(lhs: T, rhs: T, op: O): Void = {
          lhsQueue.enq(lhs)
          rhsQueue.enq(rhs)
          opQueue.enq(op)
        }

        def deq(): T = resultQueue.deq()
      }

      // Implement a FPU.
      val fpu = new FPU()

      // Registers needed for the loop
      val accumReg = Reg[T](0.to[T])
      val lhsReg = Reg[T](0.to[T])
      val rhsReg = Reg[T](0.to[T])
      val opReg = Reg[O](NUL)

      Sequential.Foreach(nInstructions by _1) { instIndex =>
        Sequential {
          lhsReg := data0(instIndex).to[T]
          rhsReg := data1(instIndex).to[T]
          opReg := ops(instIndex)
          fpu.enq(lhsReg.value, rhsReg.value, opReg.value)
          fpu.exec()
          val intermediateResult = fpu.deq()
          accumReg := intermediateResult + accumReg.value
        }
      }

      val gold = -12266.439600176689.to[T]
      val thresh = 0.1.to[T]
      val test = abs(gold - accumReg) < thresh
      argOutTest := mux(test, 111.to[I32], 0.to[I32])
    }

    println(getArg(argOutTest))
  }
}
