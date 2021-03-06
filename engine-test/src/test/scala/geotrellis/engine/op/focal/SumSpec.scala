/*
 * Copyright (c) 2014 Azavea.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package geotrellis.engine.op.focal

import geotrellis.vector.Extent
import geotrellis.raster._
import geotrellis.raster.op.focal._
import geotrellis.testkit._
import geotrellis.engine._

import org.scalatest._

class SumSpec extends FunSpec with TestEngine with TileBuilders {
  val sq1 = Square(1)
  val sq2 = Square(2)
  val sq3 = Square(3)

  val e = Extent(0.0, 0.0, 4.0, 4.0)
  val re = RasterExtent(e, 1.0, 1.0, 4, 4)

  val r = IntConstantTile(1, 4, 4)
  val rd = DoubleConstantTile(1.1, 4, 4)

  val data16 = Array(16, 16, 16, 16,
                     16, 16, 16, 16,
                     16, 16, 16, 16,
                     16, 16, 16, 16)

  describe("Sum") {
    it("should square sum r=1 for raster source") {
      val rs1 = createRasterSource(
        Array( nd,1,1,      1,1,1,      1,1,1,
                1,1,1,      2,2,2,      1,1,1,

                1,1,1,      3,3,3,      1,1,1,
                1,1,1,     1,nd,1,      1,1,1
        ),
        3,2,3,2
      )

      run(rs1.focalSum(Square(1))) match {
        case Complete(result,success) =>
//          println(success)
          assertEqual(result,
            Array(3, 5, 7,    8, 9, 8,    7, 6, 4,
                  5, 8,12,   15,18,15,   12, 9, 6,

                  6, 9,12,   14,17,14,   12, 9, 6,
                  4, 6, 8,    9,11, 9,    8, 6, 4))
        case Error(msg,failure) =>
          println(msg)
          println(failure)
          assert(false)

      }
    }

    it("should square sum with 5x5 neighborhood") {
      val rs1 = createRasterSource(
        Array( nd,1,1,      1,1,1,      1,1,1,
                1,1,1,      2,2,2,      1,1,1,

                1,1,1,      3,3,3,      1,1,1,
                1,1,1,     1,nd,1,      1,1,1
        ),
        3,2,3,2
      )

      run(rs1.focalSum(Square(2))) match {
        case Complete(result,success) =>
//          println(success)
          assertEqual(result,
            Array( 8, 14, 20,   24,24,24,    21,15, 9,
                  11, 18, 24,   28,28,28,    25,19,12,

                  11, 18, 24,   28,28,28,    25,19,12,
                   9, 15, 20,   23,23,23,    20,15, 9))
        case Error(msg,failure) =>
          println(msg)
          println(failure)
          assert(false)

      }
    }

    it("should square sum r=1") {
      assertEqual(r.focalSum(Square(1)), Array(4, 6, 6, 4,
                                           6, 9, 9, 6,
                                           6, 9, 9, 6,
                                           4, 6, 6, 4))
    }

    it("should square sum r=1 double") {
      assertEqual(rd.focalSum(Square(1)), Array(4.4, 6.6, 6.6, 4.4,
                                            6.6, 9.9, 9.9, 6.6,
                                            6.6, 9.9, 9.9, 6.6,
                                            4.4, 6.6, 6.6, 4.4))
    }

    it("should square sum r=2") {
      assertEqual(r.focalSum(Square(2)), Array(9, 12, 12, 9,
                                           12, 16, 16, 12,
                                           12, 16, 16, 12,
                                           9, 12, 12, 9))
    }

    it("should square sum r=3+") {
      assertEqual(r.focalSum(Square(3)), data16)
      assertEqual(r.focalSum(Square(4)), data16)
      assertEqual(r.focalSum(Square(5)), data16)
    }

    it("should circle sum r=1") {
      assertEqual(r.focalSum(Circle(1)), Array(3, 4, 4, 3,
                                           4, 5, 5, 4,
                                           4, 5, 5, 4,
                                           3, 4, 4, 3))
    }

    it("should circle sum r=2") {
      assertEqual(r.focalSum(Circle(2)), Array(6, 8, 8, 6,
                                           8, 11, 11, 8,
                                           8, 11, 11, 8,
                                           6, 8, 8, 6))
    }

    it("should circle sum r=3") {
      assertEqual(r.focalSum(Circle(3)), Array(11, 13, 13, 11,
                                           13, 16, 16, 13,
                                           13, 16, 16, 13,
                                           11, 13, 13, 11))
    }

    it("should circle sum r=4+") {
      assertEqual(r.focalSum(Circle(4)), Array(15, 16, 16, 15,
                                           16, 16, 16, 16,
                                           16, 16, 16, 16,
                                           15, 16, 16, 15))
      assertEqual(r.focalSum(Circle(5)), data16)
      assertEqual(r.focalSum(Circle(6)), data16)
    }

    it("should circle sum for raster source") {
      val rs1 = createRasterSource(
        Array( nd,1,1,      1,1,1,      1,1,1,
                1,1,1,      2,2,2,      1,1,1,

                1,1,1,      3,3,3,      1,1,1,
                1,1,1,     1,nd,1,      1,1,1
        ),
        3,2,3,2
      )

      run(rs1.focalSum(Circle(1))) match {
        case Complete(result,success) =>
          //println(success)
          assertEqual(result,
            Array(2, 3, 4,    5, 5, 5,    4, 4, 3,
                  3, 5, 6,    9,10, 9,    6, 5, 4,

                  4, 5, 7,   10,11,10,    7, 5, 4,
                  3, 4, 4,    5, 5, 5,    4, 4, 3))
        case Error(msg,failure) =>
          // println(msg)
          // println(failure)
          assert(false)

      }
    }
  }
}
