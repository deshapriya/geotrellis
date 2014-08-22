package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class MaskSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Mask MultiBandTile") {
    it("should work with integers") {
      val m1 = intConstMB
      val m2 = arcIntMB
      val result = m1.localMask(m2, 2, NODATA)

      for (band <- 0 until m1.bands) {
        for (col <- 0 until m1.cols) {
          for (row <- 0 until m1.rows) {
            if (band == 3)
              result.getBand(band).get(col, row) should be(NODATA)
            else
              result.getBand(band).get(col, row) should be(m1.getBand(band).get(col, row))
          }
        }
      }
    }
    
    it("should work with double values") {
      val m1 = doubleConstMB
      val m2 = arcDoubleMB

      val result = m1.localMask(m2, 2, NODATA)
      for (band <- 0 until m1.bands) {
        for (col <- 0 until m1.cols) {
          for (row <- 0 until m1.rows) {
            if (band == 3)
              result.getBand(band).getDouble(col, row).isNaN() should be(true)
            else
              result.getBand(band).getDouble(col, row) should be(m1.getBand(band).get(col, row))
          }
        }
      }
    }

  }
}