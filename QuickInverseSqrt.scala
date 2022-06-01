import spatial.dsl._

// src: https://en.wikipedia.org/wiki/Fast_inverse_square_root
@spatial object QuickInverseSqrt extends SpatialApp {
  type F = Float
  type L = UInt32

  def main(args: Array[String]): Unit = {
    val argOut = ArgOut[F]
    val dram = DRAM[F](I32(8))
    setMem(dram, Array.fromSeq(Seq(0.15625, 1, 2, 3, 4, 5, 6, 7).map(m => m.to[F])))
    Accel {
      val numberTestMem = SRAM[F](I32(8))
      numberTestMem load dram
      val number = numberTestMem(I32(0))
      val threeHalves = 1.5.to[F]
      val x2 = number * 0.5.to[F]
      val y = number
      val i = y.as[L]
      val ii = 0x5f3759df.to[L] - i / 2.to[L] // Shift seems not working. Replacing with a FixDiv.
      val yy = ii.as[F]
      val resultNewtonIter0 = yy * (threeHalves - x2 * yy * yy)
      val resultNewtonIter1 = resultNewtonIter0 * (threeHalves - x2 * resultNewtonIter0 * resultNewtonIter0)
      argOut := resultNewtonIter1
    }

    println("GOLD = " + 1.0 / sqrt(0.15625) + ", accel_val = " + getArg(argOut))
    println(getArg(argOut))
  }
}
