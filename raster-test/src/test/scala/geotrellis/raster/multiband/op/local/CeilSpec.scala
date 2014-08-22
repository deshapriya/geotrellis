package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class CeilSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Ceil MultiBandTile") {
    it("takes ceil of int multiband Raster"){
      val m = intCeilMB 
      val result = m.localCeil
      
      for(band <- 0 until result.bands){
        for(col <- 0 until result.cols){
          for(row <- 0 until result.rows){
            if(isNoData(m.getBand(band).get(col, row)))
              isNoData(result.getBand(band).get(col, row)) should be(true)
            else
              result.getBand(band).get(col, row) should be(1)
          }
        }
      }
    }
    
    it("takes ceil of double multiband Raster"){
      val m = doubleCeilMB 
      val result = m.localCeil
      
      for(band <- 0 until result.bands){
        for(col <- 0 until result.cols){
          for(row <- 0 until result.rows){
            if(m.getBand(band).getDouble(col, row).isNaN())
              result.getBand(band).get(col, row).isNaN() should be(true)
            else
              result.getBand(band).get(col, row) should be(4)
          }
        }
      }
    }
    
  }
}