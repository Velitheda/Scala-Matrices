package matrices

import org.specs2.mutable._

import MatrixOps._

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Gen

object MatrixSpecification extends Properties("Matrix") {
  val number = Gen.choose(-100, 100)
  val listNumber = Gen.choose(0, 10)
  def row(n: Int) = Gen.containerOfN[Array, Int](n, number)

  def matrix(rowNum: Int, colNum: Int): Gen[ArrayMatrix] = for {
    s <- Gen.containerOfN[Array, Array[Int]](rowNum, row(colNum))
  } yield new ArrayMatrix(s)

  def matrixList(num: Int):Gen[List[ArrayMatrix]] = for {
    r <- listNumber
    c <- listNumber
    s <- Gen.containerOfN[List, ArrayMatrix](num, matrix(r, c))
  } yield s

  property("matrix addition should be commutative") =
    forAll(matrixList(2)) { case(List(a, b)) =>
      (a + b).equals(b + a)
    }

  property("matrix addition should be associative") =
    forAll(matrixList(3)) { case(List(a, b, c)) =>
      ((a + b) + c).equals((b + c) + a)
    }

  property("adding a matrix of zeros should result in the same matrix") =
    forAll(matrixList(1)) { case(List(a)) =>
      //dimentions might be reversed here
      val identity = new ArrayMatrix(Array.tabulate(a.body.length)(index => Array.fill(a.body.head.length)(0)))
      (a + identity).equals(a)
    }

}

class MainSpec extends Specification {

  "A matrix implimentation" should {
    val rows = 2
    val columns = 3
    val body = Array.tabulate(rows)(index => Array.fill(columns)(0))
    val matrix = new ArrayMatrix(body)

    "check two matrices are equal" in {
      val matA = new ArrayMatrix(Array(Array(1, 2), Array(3, 4)))
      val matB = new ArrayMatrix(Array(Array(1, 2), Array(3, 4)))
      matA.isEqual(matB) must beTrue
    }

    "add together two simple matrices" in {
      val matA = new ArrayMatrix(Array(Array(1)))
      val matB = new ArrayMatrix(Array(Array(2)))
      val matC = matA + matB
      matC.body must beEqualTo(Array(Array(3)))
    }

    "add two two by two matrices" in {
      val matA = new ArrayMatrix(Array(Array(1, 1), Array(2, 2)))
      val matB = new ArrayMatrix(Array(Array(2, 2), Array(1, 1)))
      val matC = matA + matB

      val result = new ArrayMatrix(Array(Array(3, 3), Array(3, 3)))
      matC.isEqual(result) must beTrue
    }

    "throw a type error if you attmept to add two wrongly sized matrices" in {
      val a = new ArrayMatrix(Array(Array(1, 2)))
      val b = new ArrayMatrix(Array(Array(1)))
      a + b must throwA(new RuntimeException("Matrices are the wrong size"))
    }

    "apply a simple function to each element in the matrix" in {
      val m = new ArrayMatrix(Array(Array(1, 2)))
      val addFive = (a: Int) => a + 5
      m.elementsFunction(addFive) must beEqualTo(Array(Array(6, 7)))
    }

    "subtract two two by two matrices" in {
      val matA = new ArrayMatrix(Array(Array(1, 1), Array(2, 2)))
      val matB = new ArrayMatrix(Array(Array(2, 2), Array(1, 1)))

      val matC = matA - matB

      val result = Array(Array(-1, -1), Array(1, 1))
      matC.body must beEqualTo(result)
    }

  }
}
