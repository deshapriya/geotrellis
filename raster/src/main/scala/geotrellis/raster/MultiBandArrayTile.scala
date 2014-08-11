package geotrellis.raster

import geotrellis.vector.Extent
import com.sun.org.apache.xalan.internal.xsltc.compiler.ConcatCall

case class MultiBandArrayTile(multiBandData: Array[Tile]) extends MultiBandTile with Serializable {

  /**
   * check whether there is at least two bands
   */
  if (multiBandData.length < 2)
    sys.error("There should be at least two Tiles to be MultiBandTile")

  val cols: Int = multiBandData(0).cols
  val rows: Int = multiBandData(0).rows
  val bands: Int = multiBandData.length

  val cellType: CellType = multiBandData(0).cellType

  /**
   * check whether each Tile in multiBandData
   *  has similar CellType, Rows and Cols
   */
  for (i <- 1 until bands if (multiBandData(i).cols != cols || multiBandData(i).rows != rows || multiBandData(i).cellType != cellType)) yield { sys.error("All band should have same cols, rows and Type, Band at Index $i is differ") }

  def getBand(bandNo: Int): Tile = {
    if (bandNo < bands)
      multiBandData(bandNo)
    else
      throw new IndexOutOfBoundsException("MultiBandTile.band")
  }

  def map(f: Int => Int): MultiBandTile = {
    val outputData = (for (i <- 0 until bands) yield { this.getBand(i).map(f) }).toArray
    MultiBandTile(outputData)
  }

  def mapDouble(f: Double => Double): MultiBandTile = {
    val outputData = (for (i <- 0 until bands) yield { this.getBand(i).mapDouble(f) }).toArray
    MultiBandTile(outputData)
  }

  def convert(cellType: CellType): MultiBandTile = {
    val outputData = (for (i <- 0 until bands) yield { this.getBand(i).convert(cellType) }).toArray
    MultiBandTile(outputData)
  }

  def combine(other: MultiBandTile)(f: (Int, Int) => Int): MultiBandTile = {
    if (this.bands != other.bands) {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    } else if (this.dimensions != other.dimensions) {
      throw new Exception("MultiBandTile dimensions of bands are not Equal")
    } else {
      val output = (for (i <- 0 until this.bands) yield { this.getBand(i).combine(other.getBand(i))(f) }).toArray
      MultiBandTile(output)
    }
  }

  def combineDouble(other: MultiBandTile)(f: (Double, Double) => Double): MultiBandTile = {
    if (this.bands != other.bands) {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    } else if (this.dimensions != other.dimensions) {
      throw new Exception("MultiBandTile dimensions of bands are not Equal")
    } else {
      val output = (for (i <- 0 until this.bands) yield { this.getBand(i).combineDouble(other.getBand(i))(f) }).toArray
      MultiBandTile(output)
    }
  }

  def combine(first: Int, last: Int)(f: (Int, Int) => Int): Tile = {
    if (first < bands || last < bands) {
      var result = getBand(first)
      for (i <- first + 1 to last) yield { result = result.combine(getBand(i))(f) }
      result
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def combineDouble(first: Int, last: Int)(f: (Double, Double) => Double): Tile = {
    if (first < bands || last < bands) {
      var result = getBand(first)
      for (i <- first + 1 to last) yield { result = result.combineDouble(getBand(i))(f) }
      result
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def warp(source: Extent, target: RasterExtent): MultiBandTile = {
    val outPutData = Array.ofDim[Tile](bands)
    for (i <- 0 until bands) yield { outPutData(i) = getBand(i).warp(source, target) }
    MultiBandTile(outPutData)
  }

  def warp(source: Extent, target: Extent): MultiBandTile = {
    val outPutData = Array.ofDim[Tile](bands)
    for (i <- 0 until bands) yield { outPutData(i) = getBand(i).warp(source, target) }
    MultiBandTile(outPutData)
  }

  def warp(source: Extent, targetCols: Int, targetRows: Int): MultiBandTile = {
    val outPutData = Array.ofDim[Tile](bands)
    for (i <- 0 until bands) yield { outPutData(i) = getBand(i).warp(source, targetCols, targetRows) }
    MultiBandTile(outPutData)
  }

  override def equals(other: Any): Boolean = other match {
    case r: MultiBandArrayTile => {
      if (r == null) return false
      val len = bands
      if (len != r.bands) return false
      var i = 0
      while (i < len) {
        if (this.getBand(i) != r.getBand(i)) return false
        i += 1
      }
      true
    }
    case _ => false
  }

  /**
   * implementation of Local sequence methods on single MultiBandTile
   */

  def min(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      combine(first, last)((i, j) => math.min(i, j))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def minDouble(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      combineDouble(first, last)((i, j) => math.min(i, j))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def dualMin(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      dualCombine(first, last)((i, j) => math.min(i, j))((i, j) => math.min(i, j))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def max(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      combine(first, last)((i, j) => math.max(i, j))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def maxDouble(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      combineDouble(first, last)((i, j) => math.max(i, j))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def dualMax(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      dualCombine(first, last)((i, j) => math.max(i, j))((i, j) => math.max(i, j))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def mean(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      combine(first, last)((i, j) => i + j).map(k => k / (last - first + 1))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def meanDouble(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      combineDouble(first, last)((i, j) => i + j).map(k => k / (last - first + 1))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def dualMean(first: Int, last: Int): Tile = {
    if (first < bands || last < bands) {
      dualCombine(first, last)((i, j) => i + j)((i, j) => i + j).map(k => k / (last - first + 1))
    } else {
      throw new IndexOutOfBoundsException("MultiBandTile.bands")
    }
  }

  def ndvi(redBandIndex: Int, greenBandIndex: Int): Tile = {
    if (redBandIndex >= bands || greenBandIndex >= bands)
      throw new IndexOutOfBoundsException("MultiBandTile.bands < (redBandIndex or greenBandIndex)")
    else
      combine(redBandIndex, greenBandIndex)((r, g) => (r - g) / (r + g))
  }

  def ndviDouble(redBandIndex: Int, greenBandIndex: Int): Tile = {
    if (redBandIndex >= bands || greenBandIndex >= bands)
      throw new IndexOutOfBoundsException("MultiBandTile.bands < (redBandIndex or greenBandIndex)")
    else
      combineDouble(redBandIndex, greenBandIndex)((r, g) => (r - g) / (r + g))
  }

  /**
   * Implementation of Local specific operations on single MultiBandTile
   */
  def localAdd(constant: Int): MultiBandTile = {
    this.map(a => a + constant)
  }

  def localAdd(): Tile = {
    this.combine(0, this.bands - 1)((a, b) => a + b)
  }

  def localSubtract(constant: Int): MultiBandTile = {
    this.map(a => a - constant)
  }

  def localSubtract(): Tile = {
    this.combine(0, this.bands - 1)((a, b) => a - b)
  }

  def localMultiply(constant: Int): MultiBandTile = {
    this.map(a => a * constant)
  }

  def localMultiply(): Tile = {
    this.combine(0, this.bands - 1)((a, b) => a * b)
  }

  def localDivide(constnt: Int): MultiBandTile = {
    this.map(a => a / constnt)
  }

  def localDivide(): Tile = {
    this.combine(0, this.bands - 1)((a, b) => a / b)
  }
}
