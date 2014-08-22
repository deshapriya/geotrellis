package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class EqualSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Equal MultiBandTile") {
    it("checks int valued multiband raster against int constant") {
      val r = intMultiBand
      val result = r.localEqual(6)
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).get(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z == 6) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }

    it("checks int valued multiband raster against double constant") {
      val r = doubleMultiBand.convert(TypeInt)
      val result = r.localEqual(5.0)
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).get(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z == 5.0) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }

    it("checks double valued multiband raster against int constant") {
      val r = intMultiBand.convert(TypeDouble).mapDouble(a => a.toDouble)
      val result = r.localEqual(-9)
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).getDouble(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z == -9) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }

    it("checks double valued multiband raster against double constant") {
      val r = doubleMultiBand
      val result = r.localEqual(14.5)
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).getDouble(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z == 14.5) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }

    it("checks an integer multiband raster against itself") {
      val r = intMultiBand
      val result = r.localEqual(r)
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val rz = result.getBand(band).get(col, row)
            rz should be(1)
          }
        }
      }
    }

    it("checks an integer multiband raster against a different raster") {
      val r = intMultiBand
      val r2 = absIntmb
      val result = r.localEqual(r2)
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).get(col, row)
            val z2 = r2.getBand(band).get(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z == z2) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }
    
    it("checks a double multiband raster against itself") {
      val r = doubleMultiBand
      val result = r.localEqual(r)
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val rz = result.getBand(band).get(col, row)
            rz should be(1)
          }
        }
      }
    }
    
    it("checks a double multiband raster against a different raster") {
      val r = doubleMultiBand
      val r2 = absDubmb
      val result = r.localEqual(r2)
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            val z = r.getBand(band).getDouble(col, row)
            val z2 = r2.getBand(band).getDouble(col, row)
            val rz = result.getBand(band).get(col, row)
            if (z == z2) rz should be(1)
            else rz should be(0)
          }
        }
      }
    }

  }
}
