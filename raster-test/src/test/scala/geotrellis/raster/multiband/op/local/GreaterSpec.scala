package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class GreaterSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Greater MultiBandTile") {
    it("checks int valued multiband raster against int constant") {
    	val r = intMultiBand
      val result = r > 2
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).get(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z > 2) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }
    
    it("checks int valued multiband raster against int constant(right associative)") {
    	val r = intMultiBand
      val result = 10 >>: r
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).get(col, row)
            val rz = result.getBand(band).get(col, row)
            if (10 > z) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }
    
    it("checks int valued multiband raster against double constant") {
      val r = doubleMultiBand.convert(TypeInt).map(a => a.toInt)
      val result = r > 4.5
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).getDouble(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z > 4.5) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }
    
    it("checks double valued multiband raster against int constant") {
      val r = intMultiBand.convert(TypeDouble).mapDouble(a => a.toDouble)
      val result = r > -2
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).getDouble(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z > -2) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }
    
    it("checks double valued multiband raster against double constant") {
      val r = doubleMultiBand
      val result = r > 14.6
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).getDouble(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z > 14.6) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }
    
    it("checks double valued raster against int constant (right associative)") {
      val r = doubleMultiBand
      val result = 20 >>: r
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).getDouble(col, row)
            val rz = result.getBand(band).get(col, row)
            if (20 > z) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }
    
    it("checks an integer multiband raster against itself") {
      val r = intMultiBand
      val result = r > r
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val rz = result.getBand(band).get(col, row)
            rz should be(0)
          }
        }
      }
    }
    
    it("checks an integer multiband raster against a different raster") {
      val r = intMultiBand
      val r2 = absIntmb
      val result = r > r2
      val result2 = r2 > r
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).get(col, row)
            val z2 = r2.getBand(band).get(col, row)
            val rz = result.getBand(band).get(col, row)
            val rz2 = result2.getBand(band).get(col, row)
            if (z > z2) rz should be(1)
            else rz should be(0)
            
            if (z2 > z) rz2 should be(1)
            else rz2 should be(0)
          }
        }
      }
    }
    
    it("checks a double multiband raster against itself") {
      val r = doubleMultiBand
      val result = r > r
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val rz = result.getBand(band).get(col, row)
            rz should be(0)
          }
        }
      }
    }
    
    it("checks a double multiband raster against a different raster") {
      val r = doubleMultiBand
      val r2 = absDubmb
      val result = r > r2
      val result2 = r2 > r
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).getDouble(col, row)
            val z2 = r2.getBand(band).getDouble(col, row)
            val rz = result.getBand(band).get(col, row)
            val rz2 = result2.getBand(band).get(col, row)
            if (z > z2) rz should be(1)
            else rz should be(0)
            
            if (z2 > z) rz2 should be(1)
            else rz2 should be(0)
          }
        }
      }
    }


  }
}
