package geotrellis.raster.multiband.op.local

import geotrellis.raster._
import geotrellis.raster.multiband._

import org.scalatest._

import geotrellis.testkit._

class DivideSpec extends FunSpec
  with Matchers
  with TestEngine
  with MultiBandTileBuilder {

  describe("Divide MultiBandTile") {
    it("divides a constant value to each cell of an int valued multiband raster, from right hand side") {
      val m = positiveIntNoDataMB
      val r = m / 6
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (isNoData(m.getBand(band).get(col, row)))
              isNoData(r.getBand(band).get(col, row)) should be(true)
            else
              r.getBand(band).get(col, row) should be(m.getBand(band).get(col, row) / 6)
          }
        }
      }
    }

    it("divides a constant value to each cell of a double valued multiband raster, from right hand side") {
      val m = positiveDoubleNoDataMB
      val r = m / 5
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (m.getBand(band).getDouble(col, row).isNaN())
              r.getBand(band).getDouble(col, row).isNaN() should be(true)
            else
              r.getBand(band).getDouble(col, row) should be(m.getBand(band).getDouble(col, row) / 5)
          }
        }
      }
    }

    it("divides a constant value to each cell of an int valued multiband raster, from left hand side") {
      val m = positiveIntNoDataMB
      val r = -7 /: m
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (isNoData(m.getBand(band).get(col, row)))
              isNoData(r.getBand(band).get(col, row)) should be(true)
            else
              r.getBand(band).get(col, row) should be(-7 / m.getBand(band).get(col, row))
          }
        }
      }
    }

    it("divides a constant value to each cell of an double valued multiband raster, from left hand side") {
      val m = positiveDoubleNoDataMB
      val r = -8 /: m
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (m.getBand(band).getDouble(col, row).isNaN())
              r.getBand(band).getDouble(col, row).isNaN() should be(true)
            else
              r.getBand(band).getDouble(col, row) should be(-8 / m.getBand(band).getDouble(col, row))
          }
        }
      }
    }

    it("divides a double constant value to each cell of an int valued multiband raster, from right hand side") {
      val m = positiveIntNoDataMB
      val r = m / 12.1
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (isNoData(m.getBand(band).get(col, row)))
              isNoData(r.getBand(band).get(col, row)) should be(true)
            else
              r.getBand(band).get(col, row) should be((m.getBand(band).get(col, row) / 12.1).toInt)
          }
        }
      }
    }

    it("divides a double constant value to each cell of an double valued multiband raster, from right hand side") {
      val m = positiveDoubleNoDataMB
      val r = m / .4
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (m.getBand(band).getDouble(col, row).isNaN())
              r.getBand(band).getDouble(col, row).isNaN() should be(true)
            else
              r.getBand(band).getDouble(col, row) should be(m.getBand(band).getDouble(col, row) / .4)
          }
        }
      }
    }

    it("divides a double constant value to each cell of an int valued multiband raster, from left hand side") {
      val m = positiveIntNoDataMB
      val r = -56.09 /: m
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (isNoData(m.getBand(band).get(col, row)))
              isNoData(r.getBand(band).get(col, row)) should be(true)
            else
              r.getBand(band).get(col, row) should be((-56.09 / m.getBand(band).get(col, row)).toInt)
          }
        }
      }
    }

    it("divides a double constant value to each cell of an double valued multiband raster, from left hand side") {
      val m = positiveDoubleNoDataMB
      val r = -.89 /: m
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (m.getBand(band).getDouble(col, row).isNaN())
              r.getBand(band).getDouble(col, row).isNaN() should be(true)
            else
              r.getBand(band).getDouble(col, row) should be(-.89 / m.getBand(band).getDouble(col, row))
          }
        }
      }
    }

    it("divides an integer multiband raster to itself") {
      val m = positiveIntNoDataMB
      val r = m / m
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (isNoData(m.getBand(band).get(col, row)))
              isNoData(r.getBand(band).get(col, row)) should be(true)
            else
              r.getBand(band).get(col, row) should be(1)
          }
        }
      }
    }

    it("divides a double multiband raster to itself") {
      val m = positiveDoubleNoDataMB
      val r = m / m
      for (band <- 0 until r.bands) {
        for (col <- 0 until r.cols) {
          for (row <- 0 until r.rows) {
            if (m.getBand(band).getDouble(col, row).isNaN())
              r.getBand(band).getDouble(col, row).isNaN() should be(true)
            else
              r.getBand(band).getDouble(col, row) should be(1.0)
          }
        }
      }
    }

    it("divides first band by next band and result band by next band till end of int valued multiband raster") {
      val m = absIntmb
      val r = m.localDivide

      for (col <- 0 until r.cols) {
        for (row <- 0 until r.rows) {
          if (!isNoData(r.get(col, row)))
            r.get(col, row) should be((
              ((m.getBand(0).get(col, row) / m.getBand(1).get(col, row)) / m.getBand(2).get(col, row)) / m.getBand(3).get(col, row)).toInt)
        }
      }
    }

    it("divides first band by next band and result band by next band till end of double valued multiband raster") {
      val m = absDubmb
      val r = m.localDivide

      for (col <- 0 until r.cols) {
        for (row <- 0 until r.rows) {
          if (!isNoData(r.getDouble(col, row)))
            r.getDouble(col, row) should be(
              ((m.getBand(0).getDouble(col, row) / m.getBand(1).getDouble(col, row)) / m.getBand(2).getDouble(col, row)) / m.getBand(3).getDouble(col, row))
        }
      }
    }

    it("divides first band by next band and result band by next band in the given range  of bands in int valued multiband raster") {
      val m = absIntmb
      val r = m.localDivide(0, 2)

      for (col <- 0 until r.cols) {
        for (row <- 0 until r.rows) {
          if (!isNoData(r.get(col, row)))
            r.get(col, row) should be((
              ((m.getBand(0).get(col, row) / m.getBand(1).get(col, row)) / m.getBand(2).get(col, row))).toInt)
        }
      }
    }

    it("divides first band by next band and result band by next band in the given range  of bands in double valued multiband raster") {
      val m = absDubmb
      val r = m.localDivide(1, 3)

      for (col <- 0 until r.cols) {
        for (row <- 0 until r.rows) {
          if (!isNoData(r.getDouble(col, row)))
            r.getDouble(col, row) should be(
              (m.getBand(1).getDouble(col, row) / m.getBand(2).getDouble(col, row)) / m.getBand(3).getDouble(col, row))
        }
      }
    }

  }
}