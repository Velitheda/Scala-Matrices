package matrices

import org.specs2.mutable._

import MatrixOps._

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Gen

object MatrixSpecification extends Properties("Matrix") {
  val number = Gen.choose(-100.0, 100.0)
  val listNumber = Gen.choose(0, 10)
  def row(n: Int) = Gen.containerOfN[Array, Double](n, number)

  def matrixFactory(rowNum: Int, colNum: Int): Gen[ArrayMatrix] = for {
    s <- Gen.containerOfN[Array, Array[Double]](rowNum, row(colNum))
  } yield {
    val i = s.map(r => r.map(_.toInt.toDouble))
    new ArrayMatrix(i)
  }

  def matrixListFactory(num: Int):Gen[List[ArrayMatrix]] = for {
    r <- listNumber
    c <- listNumber
    s <- Gen.containerOfN[List, ArrayMatrix](num, matrixFactory(r, c))
  } yield s

  property("matrix addition should be commutative") =
    forAll(matrixListFactory(2)) { case(List(a, b)) =>
      (a + b).isEqual(b + a)
    }

  property("matrix addition should be associative") =
    forAll(matrixListFactory(3)) { case(List(a, b, c)) =>
      ((a + b) + c).isEqual((b + c) + a)
    }

  property("adding a matrix of zeros should result in the same matrix") =
    forAll(matrixListFactory(1)) { case(List(a)) =>
      //dimensions might be reversed here
      val identity = new ArrayMatrix(Array.fill(a.rows.length)(Array.fill(a.rows.head.length)(0)))
      (a + identity).isEqual(a)
    }

  // multiplication

  //matrices aren't the right size here for this. Need to transpose b and make c the size of the result
  property("matrix multiplication should be associative") =
    forAll(matrixListFactory(3)) { case(List(a, b, c)) =>
      ((a * b) * c).isEqual((b * c) * a)
    }
  //TODO identity test

}

class MainSpec extends Specification {

  "A matrix implementation" should {
    "check two matrices are equal" in {
      val matA = new ArrayMatrix(Array(Array(1, 2), Array(3, 4)))
      val matB = new ArrayMatrix(Array(Array(1, 2), Array(3, 4)))
      matA.isEqual(matB) must beTrue
    }

    "add together two simple matrices" in {
      val matA = new ArrayMatrix(Array(Array(1)))
      val matB = new ArrayMatrix(Array(Array(2)))
      val matC = matA + matB
      matC.rows must beEqualTo(Array(Array(3)))
    }

    "add two two by two matrices" in {
      val matA = new ArrayMatrix(Array(Array(1, 1), Array(2, 2)))
      val matB = new ArrayMatrix(Array(Array(2, 2), Array(1, 1)))
      val matC = matA + matB

      val result = new ArrayMatrix(Array(Array(3, 3), Array(3, 3)))
      matC.rows must beEqualTo(result.rows)
    }

    "commutitavely add together two two by two matrices" in {
      val matA = new ArrayMatrix(Array(Array(1, 2), Array(3, 4)))
      val matB = new ArrayMatrix(Array(Array(2, 3), Array(5, 6)))
      val matC = matA + matB
      matC.isEqual(matB + matA) must beTrue
    }

    "throw a type error if you atmept to add two wrongly sized matrices" in {
      val a = new ArrayMatrix(Array(Array(1, 2)))
      val b = new ArrayMatrix(Array(Array(1)))
      a + b must throwA(new RuntimeException("Matrices are the wrong size"))
    }

    "apply a simple function to each element in the matrix" in {
      val m = new ArrayMatrix(Array(Array(1, 2)))
      val addFive = (a: Double) => a + 5.0
      m.function(addFive).rows must beEqualTo(Array(Array(6, 7)))
    }

    "subtract two two by two matrices" in {
      val matA = new ArrayMatrix(Array(Array(1, 1), Array(2, 2)))
      val matB = new ArrayMatrix(Array(Array(2, 2), Array(1, 1)))

      val matC = matA - matB

      val result = Array(Array(-1, -1), Array(1, 1))
      matC.rows must beEqualTo(result)
    }

    "transpose a matrix" in {
      val m = ArrayMatrix(Array(
        Array(1, 2),
        Array(3, 4),
        Array(5, 6)))
      val transposed = ArrayMatrix(Array(
        Array(1, 3, 5),
        Array(2, 4, 6)
      ))
      m.transpose().rows must beEqualTo(transposed.rows)
    }

    "multiply two two by two matrices" in {
      val m1 = ArrayMatrix(Array(
        Array(1, 2),
        Array(3, 4),
        Array(5, 6)))
      val m2 = ArrayMatrix(Array(
        Array(1, 3, 5),
        Array(2, 4, 6)
      ))
      val result = ArrayMatrix(Array(
        Array(5, 11, 17),
        Array(11, 25, 39),
        Array(17, 39, 61)
      ))
      (m1 * m2).rows must beEqualTo(result.rows)
    }

    "take the dot product" in {
      val v1 = ArrayMatrix(Array(
        Array(1, 2, 3)
      ))
      val v2 = ArrayMatrix(Array(
        //Array(2, 3, 4)
        Array(1), Array(2), Array(3)
      ))
      v1.dotProduct(v2) must beEqualTo(14)
    }

    "count the correct number of rows and columns" in {
      val m = ArrayMatrix(Array(
        Array(1, 3, 5),
        Array(2, 4, 6)
      ))
      m.numRows must beEqualTo(2)
      m.numColumns must beEqualTo(3)
    }

    "get the nth row and the nth column" in {
      val m = ArrayMatrix(Array(
        Array(1, 3, 5),
        Array(2, 4, 6)
      ))
      m.getRow(1).rows must beEqualTo(Array(Array(2, 4, 6)))
      m.getColumn(2).rows must beEqualTo(Array(Array(5), Array(6)))
    }

    val oneByOneMat = ArrayMatrix(Array(Array(1.0)))
    val twoByTwoMat = ArrayMatrix(Array(
      Array(2.0, 1.0),
      Array(4.0, 3.0)
    ))
    val matrix = ArrayMatrix(Array(
      Array(3.0, 4.0, 5.0),
      Array(6.0, 7.0, 8.0),
      Array(1.0, 2.0, 3.0)
    ))

    "remove a row from a matrix" in {
      matrix.removeRow(0).rows must beEqualTo(Array(
        Array(6.0, 7.0, 8.0),
        Array(1.0, 2.0, 3.0)
      ))
    }
    "remove a column from a matrix" in {
      matrix.removeColumn(2).rows must beEqualTo(Array(
        Array(3.0, 4.0),
        Array(6.0, 7.0),
        Array(1.0, 2.0)
      ))
    }
    "get an element from a matrix" in {
      matrix.getElement(1, 2) must beEqualTo(8.0)
    }
    "multiply a row in a matrix by a scalar" in { ko}
    "add a multiple of a row to another row in a matrix" in { ko}
    "determine if a matrix is square" in { ko}
    "calculate create a submatrix by deleting a row and a column of a matrix" in {ko}
    "calculate the correct cofactor sign for an element in a matrix" in {
      //matrix.cofactorSign()
      ko
    }
    "calculate the matrix of cofactors" in { ko}
    "calculate the determinant of a 1x1 matrix" in {
      oneByOneMat.det must beEqualTo(1.0)
    }
    "calculate the determinant of a 2x2 matrix" in {
      twoByTwoMat.det must beEqualTo(2.0)
    }
    "calculate the determinant of a 3x3 matrix" in {
      twoByTwoMat.det must beEqualTo(0.0)
    }
    "calculate the determinant of a 4x4 matrix" in { ko}
    "calculate the inverse of a 1x1 matrix" in { ko}
    "calculate the inverse of a 2x2 matrix" in { ko}
    "calculate the inverse of a 3x3 matrix" in { ko}
    "calculate the inverse of a 4x4 matrix" in { ko}

  }
}
