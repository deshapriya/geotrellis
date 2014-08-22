package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class FloorSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Floor MultiBandTile") {
    it("takes floor of int multiband Raster") {
      val m = intCeilMB
      val result = m.localFloor
      for (band <- 0 until m.bands) {
        for (col <- 0 until m.cols) {
          for (row <- 0 until m.rows) {
            val r = result.getBand(band).get(col, row)
            val x = m.getBand(band).get(col, row)
            if (isNoData(x)) r should be(NODATA)
            else r should be(1)
          }
        }
      }
    }
    
    it("takes floor of double multiband Raster") {
      val m = doubleCeilMB
      val result = m.localFloor
      for (band <- 0 until m.bands) {
        for (col <- 0 until m.cols) {
          for (row <- 0 until m.rows) {
            val r = result.getBand(band).getDouble(col, row)
            val x = m.getBand(band).getDouble(col, row)
            if (isNoData(x)) r.isNaN should be(true)
            else r should be(3.0)
          }
        }
      }
    }

  }
}
