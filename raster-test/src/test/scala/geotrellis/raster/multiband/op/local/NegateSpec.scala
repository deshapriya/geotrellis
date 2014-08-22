package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class NegateSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Negate MultiBandTile") {
    it("negates an integer multiband raster") {
      val m = intConstMB
      val result = -m

      for (band <- 0 until m.bands) {
        for (col <- 0 until m.cols) {
          for (row <- 0 until m.rows) {
            result.getBand(band).get(col, row) should be(-m.getBand(band).get(col, row))
          }
        }
      }
    }

    it("negates a double multiband raster") {
      val m = doubleConstMB
      val result = -m

      for (band <- 0 until m.bands) {
        for (col <- 0 until m.cols) {
          for (row <- 0 until m.rows) {
            result.getBand(band).getDouble(col, row) should be(-m.getBand(band).getDouble(col, row))
          }
        }
      }
    }

  }
}