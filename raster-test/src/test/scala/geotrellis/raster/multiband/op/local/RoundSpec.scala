package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class RoundSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Round MultiBandTile") {    
    it("correctly rounds a Int multiband Raster") {
      val m = intCeilMB 
      val result = m.localRound
      
      for(band <- 0 until m.bands){
        for(col <- 0 until m.cols){
          for(row <- 0 until m.rows){
            if(isNoData(m.getBand(band).get(col, row)))
              result.getBand(band).get(col,row) should be (NODATA)
            else
              result.getBand(band).get(col,row) should be(math.round(m.getBand(band).get(col, row)).toInt)
          }
        }
      }
    }
    
    it("correctly rounds a double multiband Raster") {
      val m = doubleMultiBand  
      val result = m.localRound
      
      for(band <- 0 until m.bands){
        for(col <- 0 until m.cols){
          for(row <- 0 until m.rows){
            if(m.getBand(band).getDouble(col, row).isNaN())
              result.getBand(band).getDouble(col,row).isNaN() should be (true)
            else
              result.getBand(band).getDouble(col,row) should be(math.round(m.getBand(band).getDouble(col, row)))
          }
        }
      }
    }
    
  }
}