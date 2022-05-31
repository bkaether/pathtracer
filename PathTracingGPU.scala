import spatial.dsl._

@spatial object PathTracingGPU extends SpatialApp {
  def main(args: Array[String]): Unit = {
    type T = FixPt[TRUE, _32, _32]
    type O = FixPt[FALSE, _3, _3]
    type F = Float

    val ADD = 0.to[O]
    val SUB = 1.to[O]
    val MUL = 2.to[O]
    val DIV = 3.to[O]
    val argOutTest = ArgOut[I32]
    val ip = I32(4)

    val nInstructions = 4

    Accel {
//      val data0 = LUT[F](4)(1.to[F], 2.to[F], 3.14.to[F], 5.12354234.to[F])
//      val data1 = LUT[F](4)(13.3254234.to[F], 12321.to[F], 12.to[F], 9.232.to[F])

      val data0 = LUT[T](4)(1.to[T], 2.to[T], 3.14.to[T], 5.12354234.to[T])
      val data1 = LUT[T](4)(13.3254234.to[T], 12321.to[T], 12.to[T], 9.232.to[T])
      val ops = LUT[O](4)(ADD, SUB, MUL, DIV)

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
      val accumReg = Reg[T](0.to[T])
      val fpu = new FPU()

      Sequential.Foreach(nInstructions by 1) { instIndex =>
        Sequential {
          val lhs = data0(instIndex)
          val rhs = data1(instIndex)
          val op = ops(instIndex)
          fpu.enq(lhs, rhs, op)
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
