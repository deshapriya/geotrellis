package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class LogSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Log MultiBandTile") {
    it("takes log of int multiband Raster") {
      val m = intCeilMB
      val result = m.localLog

      for (band <- 0 until m.bands) {
        for (col <- 0 until m.cols) {
          for (row <- 0 until m.rows) {
            if (isNoData(m.getBand(band).get(col, row)))
              result.getBand(band).get(col, row) should be(NODATA)
            else
              result.getBand(band).get(col, row) should be(math.log(1).toInt)
          }
        }
      }
    }

    it("takes log of double multiband Raster") {
      val m = doubleCeilMB
      val result = m.localLog

      for (band <- 0 until m.bands) {
        for (col <- 0 until m.cols) {
          for (row <- 0 until m.rows) {
            if (isNoData(m.getBand(band).getDouble(col, row)))
              result.getBand(band).getDouble(col, row).isNaN() should be(true)
            else
              result.getBand(band).getDouble(col, row) should be(math.log(3.4))
          }
        }
      }
    }

  }
}