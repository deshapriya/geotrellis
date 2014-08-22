package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class NotSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Not MultiBandTile") {
    it("negates an Int multiband raster") {
      val m = intConstMB  
      val result = m.localNot
      
      for (band <- 0 until m.bands) {
        for (col <- 0 until m.cols) {
          for (row <- 0 until m.rows) {
            result.getBand(band).get(col, row) should be(~1)
          }
        }
      }
    }
    
  }
}
