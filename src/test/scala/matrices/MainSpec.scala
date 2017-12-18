package matrices

import org.specs2.mutable._

import Main._

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Gen

object MatrixSpecification extends Properties("Matrix") {
  val number = Gen.choose(-100, 100)
  val listNumber = Gen.choose(0, 10)
  def row(n: Int) = Gen.containerOfN[Array, Int](n, number)

  def matrix(rowNum: Int, colNum: Int):Gen[MatrixImpl] = for {
    s <- Gen.containerOfN[Array, Array[Int]](rowNum, row(colNum))
  } yield new MatrixImpl(s)

  def matrixList(num: Int):Gen[List[MatrixImpl]] = for {
    r <- listNumber
    c <- listNumber
    s <- Gen.containerOfN[List, MatrixImpl](num, matrix(r, c))
  } yield s

  property("matrix addition should be commutative") =
    forAll(matrixList(2)) { case(List(a, b)) =>
      println(a.toString)
      println(b.toString)
      a.add(b).equals(b.add(a))
    }

  property("matrix addition should be associative") =
    forAll(matrixList(3)) { case(List(a, b, c)) =>
      a.add(b).add(c).equals(b.add(c).add(a))
    }

  property("adding a matrix of zeros should result in the same matrix") =
    forAll(matrixList(1)) { case(List(a)) =>
      val identity = new MatrixImpl(a.getRows, a.getColumns)
      a.add(identity).equals(a)
    }

}

class MainSpec extends Specification {

  "A simple matrix" should {
    val rows = 2
    val columns = 3
    val matrix: Matrix = new MatrixImpl(rows, columns)

    "create a matrix with m rows and n columns" in {
      matrix.getRows must beEqualTo(rows)
      matrix.getColumns must beEqualTo(columns)
    }

    "return the value at row m and column n" in {
      matrix.get(0, 0) must beEqualTo(1)
    }

    "set the value at row m and column n" in {
      val result = matrix.set(0, 0)(50)
      result.get(0, 0) must beEqualTo(50)
    }

    "check two matrices are equal" in {
      val matA: Matrix = new MatrixImpl(Array(Array(1, 2), Array(3, 4)))
      val matB: Matrix = new MatrixImpl(Array(Array(1, 2), Array(3, 4)))
      matA.equals(matB) must beTrue
    }

    "print out a correct respresentation of the matrix body" in {
      val stringMatrix =
        """1, 1, 1
          |1, 1, 1""".stripMargin
      matrix.toString must beEqualTo(stringMatrix)
    }
  }

  "Basic operations" should {

    "add together two simple matrices" in {
      val matA: Matrix = new MatrixImpl(Array(Array(1)))
      val matB: Matrix = new MatrixImpl(Array(Array(2)))
      val matC: Matrix = matA.add(matB)
      matC.get(0, 0) must beEqualTo(3)
    }

    "add two two by two matrices" in {
      val matA: Matrix = new MatrixImpl(Array(Array(1, 1), Array(2, 2)))
      val matB: Matrix = new MatrixImpl(Array(Array(2, 2), Array(1, 1)))
      val matC: Matrix = matA.add(matB)

      val result = new MatrixImpl(Array(Array(3, 3), Array(3, 3)))
      matC.equals(result) must beTrue
    }

    "throw a type error if you attmept to add two wrongly sized matrices" in {
      val a = new MatrixImpl(Array(Array(1, 2)))
      val b = new MatrixImpl(Array(Array(1)))
      a.add(b) must throwA(new RuntimeException("Matrices are the wrong size"))
    }

    "subtract two two by two matrices" in {
      val matA: Matrix = new MatrixImpl(Array(Array(1, 1), Array(2, 2)))
      val matB: Matrix = new MatrixImpl(Array(Array(2, 2), Array(1, 1)))
      val matC: Matrix = matA.subtract(matB)

      val result = new MatrixImpl(Array(Array(-1, -1), Array(1, 1)))
      matC.equals(result) must beTrue
    }

  }
}
