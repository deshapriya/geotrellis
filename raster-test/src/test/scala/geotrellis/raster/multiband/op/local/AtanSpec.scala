package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class AtanSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("ArcTan MultiBandTile") {
    it("finds arctan of double multiband rasters") {
      val arr0 = Array(0.0, 1 / math.sqrt(3), 1.0,
        math.sqrt(3), Double.PositiveInfinity, Double.NaN)
      val arr1 = Array(-0.0, -1 / math.sqrt(3), -1.0,
        -math.sqrt(3), Double.NegativeInfinity, Double.NaN)
      val arr2 = Array(0.0, 1 / math.sqrt(3), 1.0,
        math.sqrt(3), Double.PositiveInfinity, Double.NaN)
      val arr3 = Array(-0.0, -1 / math.sqrt(3), -1.0,
        -math.sqrt(3), Double.NegativeInfinity, Double.NaN)

      val t0 = DoubleArrayTile(arr0, 3, 2)
      val t1 = DoubleArrayTile(arr1, 3, 2)
      val t2 = DoubleArrayTile(arr2, 3, 2)
      val t3 = DoubleArrayTile(arr3, 3, 2)
      val mb1 = MultiBandTile(Array(t0, t1, t2, t3))

      val arr4 = Array(0.0, 1.0 / 6, 1.0 / 4, 1.0 / 3, 0.5, Double.NaN)
      val arr5 = Array(-0.0, -1.0 / 6, -1.0 / 4, -1.0 / 3, -0.5, -Double.NaN)
      val arr6 = Array(0.0, 1.0 / 6, 1.0 / 4, 1.0 / 3, 0.5, Double.NaN)
      val arr7 = Array(-0.0, -1.0 / 6, -1.0 / 4, -1.0 / 3, -0.5, -Double.NaN)

      val t4 = DoubleArrayTile(arr4, 3, 2)
      val t5 = DoubleArrayTile(arr5, 3, 2)
      val t6 = DoubleArrayTile(arr6, 3, 2)
      val t7 = DoubleArrayTile(arr7, 3, 2)
      val mb2 = MultiBandTile(Array(t4, t5, t6, t7))

      val em = mb2.mapDouble(x => math.Pi * x)
      val result = mb1.localAtan

      for (band <- 0 until result.bands) {
        for (col <- 0 until result.cols) {
          for (row <- 0 until result.rows) {
            val angle = result.getBand(band).getDouble(col, row)
            val epsilon = math.ulp(angle)
            if (mb1.getBand(band).getDouble(col, row).isNaN())
              angle.isNaN() should be(true)
            else
              angle should be(em.getBand(band).getDouble(col, row) +- epsilon)
          }
        }
      }
    }

    it("finds arctan of an int multiband raster") {
      val m1 = arcIntMB
      val expectedAngles = Array(0.0, 0.25 * math.Pi, -0.25 * math.Pi,
        math.atan(2), math.atan(-2), Double.NaN)
      val result = m1.localAtan

      for (band <- 0 until result.bands) {
        for (col <- 0 until result.cols) {
          for (row <- 0 until result.rows) {
            val angle = result.getBand(band).getDouble(col, row)
            val epsilon = math.ulp(angle)
            if (m1.getBand(band).getDouble(col, row).isNaN())
              angle.isNaN() should be(true)
            else
              angle should be(expectedAngles(band) +- epsilon)
          }
        }
      }

    }

  }
}
