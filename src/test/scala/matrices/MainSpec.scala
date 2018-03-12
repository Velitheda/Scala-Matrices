package matrices

import org.specs2.mutable._
import MatrixOps._
import matrices.ArrayMatrix.ArrayMatrixImpl
import org.scalacheck.Properties
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Gen

object MatrixSpecification extends Properties("Matrix") {
  val number = Gen.choose(-100.0, 100.0)
  val integer = Gen.choose(-100, 100)

  //Tests start failing at matrix sizes of 8x8 due to problems with very large double calculations losing accuracy
  val listNumber = Gen.choose(1, 7)
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

  def squareMatrix(size: Int): Gen[ArrayMatrix] = for {
    s <- Gen.containerOfN[Array, Array[Double]](size, row(size))
  } yield {
    val i = s.map(r => r.map(_.toInt.toDouble))
    new ArrayMatrix(i)
  }

  def squareMatrixList(num: Int): Gen[List[ArrayMatrix]] = for {
    size <- listNumber
    list <- Gen.containerOfN[List, ArrayMatrix](num, squareMatrix(size))
  } yield {
    list
  }

  def identityMatrix(): Gen[ArrayMatrix] = for {
    n <- listNumber
  } yield {
    ArrayMatrixImpl.identity(n)
  }

  def invertableMatrix(): Gen[ArrayMatrix] = {
    for {
      n <- listNumber
      m <- squareMatrix(n) suchThat(_.det != 0)
    } yield {
       m
    }
  }

  //Addition
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
      val identity = new ArrayMatrix(Array.fill(a.rows.length)(Array.fill(a.rows.head.length)(0)))
      (a + identity).isEqual(a)
    }

  property("the order a scalar is applied to matrix addition does not matter") =
    forAll(matrixFactory(3, 2), matrixFactory(3, 2), integer) { case(a, b, n) =>
      val f = (e: Double) => e * n
      (a.function(f) * b).isEqual(a * b.function(f))
    }

  //Multiplication
  //Find a way to generalise the sizes better that still allows them to be multiplied
  property("matrix multiplication should be commutitiave") =
    forAll(matrixFactory(3, 2), matrixFactory(2, 3), matrixFactory(3, 3)) { case(a, b, c) =>
      ((a * b) * c).isEqual(a * (b * c))
    }

  property("matrix multiplication should be distributitve") =
    forAll(matrixFactory(3, 2), matrixFactory(3, 2), matrixFactory(2, 3)) { case(a, b, c) =>
      ((a + b) * c).isEqual((b * c) + (a * c))
      (c * (a + b)).isEqual((c * b) + (c * a))
    }

  property("matrix multiplication should hold the identity property") =
    forAll(matrixListFactory(1)) { case(List(a)) =>
      val identity = ArrayMatrix(Array.tabulate(a.numRows())(rowIndex => Array.tabulate(a.numRows())(columnIndex => if (rowIndex == columnIndex) 1 else 0)))
      (a * identity).isEqual(a)
    }

  property("the order a scalar is applied to matrix multiplication does not matter") =
    forAll(matrixFactory(3, 2), matrixFactory(2, 3), number) { case(a, b, n) =>
      val f = (e: Double) => e * n.toInt
      (a.function(f) * b).isEqual(a * b.function(f))
    }

  //Transpose
  property("A double transpose should be the same as itself") =
    forAll(matrixListFactory(1)) { case(List(a)) =>
      a.transpose().transpose().isEqual(a)
    }

  property("The transpose of two added matrices is the same as transposing them and adding them") =
    forAll(matrixFactory(3, 2), matrixFactory(3, 2)) { case(a, b) =>
      (a + b).transpose().isEqual(a.transpose() + b.transpose())
    }

  property("The transpose of two multiplied matrices is the same as transposing them and multiplying them in reverse order") =
    forAll(matrixFactory(3, 2), matrixFactory(2, 3)) { case(a, b) =>
      (a * b).transpose().isEqual(b.transpose() * a.transpose())
    }

  property("A matrix can be transposed before or after being multiplied by a scalar") =
    forAll(matrixListFactory(1), integer) { case(List(a), n) =>
      val f = (e: Double) => e * n
      a.transpose().function(f).isEqual(a.function(f).transpose())
    }

  //Determinants
  property("The determinant of a matrix with a zero row or column is 0") =
    forAll(matrixFactory(0, 0)) { case(a) =>
      a.det() == 0
    }

  property("The determinant of a 1 by 1 matrix is its only element") =
    forAll(matrixFactory(1, 1)) { case(a) =>
      a.det() == a.getElement(0, 0)
    }

  property("The determinant of the identity matrix is 1") =
    forAll(identityMatrix()) { case(identity) =>
      identity.det() == 1
    }

  property("The determinant of a matrix is the same as the determinant of its transpose") =
    forAll(squareMatrixList(1)) { case(List(a)) =>
      a.det() == a.transpose().det()
    }

  //This test fails on matrices bigger than 3x3 due to rounding errors with double calculation
  property("the product of a determinant of two matrices is the same as the product of their determinants") =
    forAll(squareMatrix(3), squareMatrix(3)) { case(a, b) =>
      (a * b).det() == a.det() * b.det()
    }

  //Inverse
  //Rounding errors here again
  property("taking the inverse twice results in the same matrix") =
    forAll(invertableMatrix()) { case(a) =>
      a.inverse().inverse().isEqual(a)
    }

  property("the inverse of the transpose is the same as the transpose of the inverse") =
    forAll(invertableMatrix()) { case(a) =>
      a.transpose().inverse().isEqual(a.inverse().transpose())
    }

  //(kA)^−1 = (k^−1)(A^−1) for nonzero scalar k
  property("the inverse of a nonzero scalar times a matrix is the same as one over the scalar times the inverse of a matrix") =
    forAll(invertableMatrix(), Gen.choose(-100.0, 100.0) suchThat(_ != 0.0)) { case (a, n) =>
      a.function((e: Double) => e * n).inverse().isEqual(
        a.inverse().function((e: Double) => e * (1 / n))
      )
    }

  property("the determinant of the inverse is the same as one over the determinant") =
    forAll(invertableMatrix()) { case (a) =>
      val det = a.det()
      a.inverse().det() == (1 / a.det())
    }

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

    "multiply two matrices" in {
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
//    "multiply a row in a matrix by a scalar" in { ko}
//    "add a multiple of a row to another row in a matrix" in { ko}
//    "determine if a matrix is square" in { ko}

    "create a submatrix by deleting a row and a column of a matrix" in {
      ArrayMatrixImpl.removeRowAndColumn(matrix, 1, 1).rows must beEqualTo(Array(
        Array(3.0, 5.0),
        Array(1.0, 3.0)
      ))
    }
    "calculate the correct cofactor sign for an element in a matrix" in {
      ArrayMatrixImpl.cofactorSign(1, 1) must beEqualTo (1)
      ArrayMatrixImpl.cofactorSign(1, 2) must beEqualTo (-1)
    }
    "calculate the matrix of cofactors" in { ko}

    "calculate the determinant of a 1x1 matrix" in {
      oneByOneMat.det must beEqualTo(1.0)
    }
    "calculate the determinant of a 2x2 matrix" in {
      twoByTwoMat.det must beEqualTo(2.0)
    }
    "calculate the determinant of a 3x3 matrix" in {
      matrix.det must beEqualTo(0.0)
    }
    "calculate the inverse of a 1x1 matrix" in { ko}
    "calculate the inverse of a 2x2 matrix" in {
      twoByTwoMat.inverse().rows must beEqualTo(Array(
        Array(1.5, -0.5),
        Array(-2.0, 1.0)
      ))
    }
    //"calculate the inverse of a 3x3 matrix" in { ko}
    //"calculate the inverse of a 4x4 matrix" in { ko}

  }
}
