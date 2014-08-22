package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class UndefinedSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Undefined MultiBandTile") {
    it("returns correct result for an integer multiband raster") {
      val m = positiveIntNoDataMB
      val r = m.localUndefined

      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (isNoData(m.getBand(band).get(col, row)))
              r.getBand(band).get(col, row) should be(1)
            else
              r.getBand(band).get(col, row) should be(0)
          }
        }
      }
    }

    it("returns correct result for a double multiband raster") {
      val m = positiveDoubleNoDataMB
      val r = m.localUndefined

      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (isNoData(m.getBand(band).get(col, row)))
              r.getBand(band).get(col, row) should be(1)
            else
              r.getBand(band).get(col, row) should be(0)
          }
        }
      }
    }

  }
}
