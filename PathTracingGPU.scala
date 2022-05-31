import spatial.dsl._

import javax.sound.midi.SysexMessage

/**
 * TODOs:
 * [] Accumulation: How should I allocate registers to do that?
 * [] Vec3 ops: Looking at how pathtracer is being done, we should have support for manipulating 3-D points.
 */

trait Types {
  type O = Byte
  type F = Float

  @struct class Vec3(x: F, y: F, z: F)
}

@spatial object PathTracingGPU extends SpatialApp with Types {
  def main(args: Array[String]): Unit = {
    val nDim = 3

    val ADD = 0.to[O]
    val SUB = 1.to[O]
    val MUL = 2.to[O]
    val DIV = 3.to[O]
    val NUL = -1.to[O]
    val LTMP = 4.to[O]
    val RTMP = 5.to[O]
    val MIN = 6.to[O]
    val MAX = 7.to[O]

    val _0 = I32(0)
    val _1 = I32(1)
    val ip = I32(4)

    val nInstructions = I32(4)
    val nInstructionsS = 4
    val data0XDRAM = DRAM[F](nInstructions)
    val data0YDRAM = DRAM[F](nInstructions)
    val data0ZDRAM = DRAM[F](nInstructions)

    val data1XDRAM = DRAM[F](nInstructions)
    val data1YDRAM = DRAM[F](nInstructions)
    val data1ZDRAM = DRAM[F](nInstructions)

    val opsDRAM = DRAM[O](nInstructions)
    val outDRAMSeq = Seq.tabulate(nDim)(_ => DRAM[F](nInstructions))

    val data0XS = Seq(
      1,
      2,
      3.14,
      5.12354234
    )
    val data0YS = Seq(
      0.2313,
      0.001235,
      -978.823490,
      -94568.213,
    )
    val data0ZS = Seq(
      201231,
      0.0123,
      0.0002315,
      -1000.001,
    )

    setMem(data0XDRAM, Array.fromSeq(data0XS.map(m => m.to[F])))
    setMem(data0YDRAM, Array.fromSeq(data0YS.map(m => m.to[F])))
    setMem(data0ZDRAM, Array.fromSeq(data0ZS.map(m => m.to[F])))

    val data1XS = Seq(
      13.3254234,
      12321,
      12,
      9.232
    )

    val data1YS = Seq(
      -0.245108,
      1.04435,
      5432.123,
      0.003497
    )

    val data1ZS = Seq(
      -0.000123123,
      1000042141,
      0.00002310897,
      0.90213
    )

    setMem(data1XDRAM, Array.fromSeq(data1XS.map(m => m.to[F])))
    setMem(data1YDRAM, Array.fromSeq(data1YS.map(m => m.to[F])))
    setMem(data1ZDRAM, Array.fromSeq(data1ZS.map(m => m.to[F])))

    setMem(opsDRAM, Array.fromSeq(Seq[O](ADD, SUB, MUL, DIV)))

    val data0DRAMSeq = Seq(data0XDRAM, data0YDRAM, data0ZDRAM)
    val data1DRAMSeq = Seq(data1XDRAM, data1YDRAM, data1ZDRAM)

    Accel {
      val ops = SRAM[O](nInstructions)

      val data0Seq = Seq.tabulate(nDim)(_ => SRAM[F](nInstructions))
      val data1Seq = Seq.tabulate(nDim)(_ => SRAM[F](nInstructions))

      data0Seq.zip(data0DRAMSeq).foreach { case (s, d) => s load d }
      data1Seq.zip(data1DRAMSeq).foreach { case (s, d) => s load d }
      ops load opsDRAM

      val outMemSeq = Seq.tabulate(nDim)(_ => SRAM[F](nInstructions))

      class FPU(nDim: scala.Int) {
        val lhsQueueSeq: Seq[FIFO[F]] = Seq.tabulate(nDim)(_ => FIFO[F](ip))
        val rhsQueueSeq: Seq[FIFO[F]] = Seq.tabulate(nDim)(_ => FIFO[F](ip))
        val resultQueueSeq: Seq[FIFO[F]] = Seq.tabulate(nDim)(_ => FIFO[F](ip))
        val opQueue: FIFO[O] = FIFO[O](ip)

        private def exec1D(lhsQueue: FIFO[F], rhsQueue: FIFO[F], resultQueue: FIFO[F]): Void = {
          val lv = lhsQueue.deq()
          val rv = rhsQueue.deq()
          val op = opQueue.deq()
          val addVal = lv + rv
          val divVal = lv / rv
          val mulVal = lv * rv
          val subVal = lv - rv
          resultQueue.enq(mux(op == ADD, addVal, mux(op == SUB, subVal, mux(op == DIV, divVal, mulVal))))
        }

        def execND(): Void = {
          (lhsQueueSeq zip rhsQueueSeq zip resultQueueSeq).foreach { case ((lhsQueue, rhsQueue), resultQueue) =>
            exec1D(lhsQueue, rhsQueue, resultQueue)
          }
        }

        def enqND(lhsSeq: Seq[F], rhsSeq: Seq[F], op: O): Void = {
          try {
            (lhsQueueSeq zip lhsSeq).foreach { case (q, f) => q.enq(f) }
            (rhsQueueSeq zip rhsSeq).foreach { case (q, f) => q.enq(f) }
            opQueue.enq(op)
          } catch {
            case ex: Exception => Console.println(s"Failed enqueuing ND FPU! Error message = $ex")
          }
        }

        def deqND(): Seq[F] = resultQueueSeq.map(q => q.deq())
      }

      // Implement a FPU.
      val fpu = new FPU(nDim)

      // Registers needed for the loop
      val lhsRegFile = Seq.tabulate(nDim)(_ => Reg[F](0.to[F]))
      val rhsRegFile = Seq.tabulate(nDim)(_ => Reg[F](0.to[F]))
      val opReg = Reg[O](NUL)

      FSM(0)(instIndex => instIndex < nInstructions) { instIndex =>
        (lhsRegFile zip data0Seq).foreach { case (reg, sram) => reg := sram(instIndex) }
        (rhsRegFile zip data1Seq).foreach { case (reg, sram) => reg := sram(instIndex) }
        opReg := ops(instIndex)
        fpu.enqND(
          lhsRegFile.map(r => r.value),
          rhsRegFile.map(r => r.value),
          opReg.value
        )
        fpu.execND()
        val resultSeq = fpu.deqND()
        (outMemSeq zip resultSeq).foreach { case (m, r) => m(instIndex) = r }
      } { instIndex => instIndex + 1.to[I32] }

      //      val gold = -12266.439600176689.to[F]
      (outDRAMSeq zip outMemSeq).foreach { case (d, m) => d store m }
    }

    println("RESULT = ")
    println("---------------------")
    outDRAMSeq.foreach { d =>
      printArray(getMem(d))
    }
    println("---------------------")

    println("GOLD = ")
    println("---------------------")
    for (i <- 0 to nInstructionsS - 1) {
      val d0x = data0XS(i)
      val d0y = data0YS(i)
      val d0z = data0ZS(i)
      val d1x = data1XS(i)
      val d1y = data1YS(i)
      val d1z = data1ZS(i)
      i match {
        case 0 => println(d0x + d1x, d0y + d1y, d0z + d1z)
        case 1 => println(d0x - d1x, d0y - d1y, d0z - d1z)
        case 2 => println(d0x * d1x, d0y * d1y, d0z * d1z)
        case _ => println(d0x / d1x, d0y / d1y, d0z / d1z)
      }
    }
    println("---------------------")

    //    println(getArg(argOutTest))
  }
}
