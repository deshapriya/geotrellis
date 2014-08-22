package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class TanhSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Tanh MultiBandTile") {
    it("finds the tanh of a int multiband raster") {
      val a0 = Array(0, 1, 2, 3, 4, 5, 6, 7)
      val a1 = Array(-4, -5, -6, -7, -1, -2, -3, NODATA)

      val t0 = IntArrayTile(a0, 4, 2)
      val t1 = IntArrayTile(a1, 4, 2)
      val m = MultiBandTile(Array(t0, t1))

      val t2 = DoubleArrayTile(a0.map(math.tanh(_)), 4, 2)
      val t3 = DoubleArrayTile(a1.map(math.tanh(_)), 4, 2)
      val ex = MultiBandTile(Array(t2, t3))
      val r = m.localTanh

      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val tanh = r.getBand(band).getDouble(col, row)
            val epsilon = math.ulp(tanh)
            val ep = ex.getBand(band).getDouble(col, row)
            if (isNoData(m.getBand(band).get(col, row)))
              tanh.isNaN() should be(true)
            else
              tanh should be(ep +- epsilon)
          }
        }
      }
    }

    it("finds the tanh of a double multiband raster") {
      val arr0 = Array(0.0, 1.0 / 6, 1.0 / 3, 1.0 / 2, 2.0 / 3, 5.0 / 6).map(_ * math.Pi)
      val arr1 = Array(1.0, 7.0 / 6, 4.0 / 3, 3.0 / 2, 5.0 / 3, 11.0 / 6).map(_ * math.Pi)
      val arr2 = Array(2.0, 13.0 / 6, 7.0 / 3, 5.0 / 2, 8.0 / 3, 17.0 / 6).map(_ * math.Pi)
      val arr3 = Array(-0.0, -1.0 / 6, -1.0 / 3, -1.0 / 2, -2.0 / 3, -5.0 / 6).map(_ * math.Pi)
      val arr4 = Array(-1.0, -7.0 / 6, -4.0 / 3, -3.0 / 2, -5.0 / 3, -11.0 / 6).map(_ * math.Pi)
      val arr5 = Array(-2.0, -13.0 / 6, -7.0 / 3, Double.PositiveInfinity, Double.NegativeInfinity, Double.NaN).map(_ * math.Pi)

      val t0 = DoubleArrayTile(arr0, 3, 2)
      val t1 = DoubleArrayTile(arr1, 3, 2)
      val t2 = DoubleArrayTile(arr2, 3, 2)
      val t3 = DoubleArrayTile(arr3, 3, 2)
      val t4 = DoubleArrayTile(arr4, 3, 2)
      val t5 = DoubleArrayTile(arr5, 3, 2)
      val m = MultiBandTile(Array(t0, t1, t2, t3, t4, t5))

      val t6 = DoubleArrayTile(arr0.map(math.tanh(_)), 3, 2)
      val t7 = DoubleArrayTile(arr1.map(math.tanh(_)), 3, 2)
      val t8 = DoubleArrayTile(arr2.map(math.tanh(_)), 3, 2)
      val t9 = DoubleArrayTile(arr3.map(math.tanh(_)), 3, 2)
      val t10 = DoubleArrayTile(arr4.map(math.tanh(_)), 3, 2)
      val t11 = DoubleArrayTile(arr5.map(math.tanh(_)), 3, 2)
      val ex = MultiBandTile(Array(t6, t7, t8, t9, t10, t11))

      val r = m.localTanh
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val tanh = r.getBand(band).getDouble(col, row)
            val epsilon = math.ulp(tanh)
            val ep = ex.getBand(band).getDouble(col, row)
            if (m.getBand(band).getDouble(col, row).isNaN())
              tanh.isNaN() should be(true)
            else if (tanh.isInfinite())
              tanh should be(ep)
            else
              tanh should be(ep +- epsilon)
          }
        }
      }
    }

  }
}
