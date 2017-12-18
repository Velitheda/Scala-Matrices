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
    r <- listNumber
    c <- listNumber
    s <- Gen.containerOfN[Array, Array[Int]](r, row(r))
  } yield new MatrixImpl(s)

//  property("matrix addition should be commutative") =
//    forAll(matrix(listNumber, listNumber), matrix(listNumber, listNumber)) {
//      (a, b) => a.add(b).body sameElements b.add(a).body
//    }
//
//  property("matrix addition result should be the same size") =
//    forAll( listNumber { (size: Int) =>
//
//      val a = matrix(size, size)
//      val b = matrix(size, size)//forAll(matrix(size, size) { m =>
//      true
//    })
//      println("a")
//      println(a.toString)
//      println("b")
//      println(b.toString)
//      println("c")
//      //val c: Matrix = a.add(b)
//      //println(c.toString)
//      //this now throws when columns different, confusing the test
//      true
//      //c.getRows == a.getRows && c.getColumns == a.getColumns // should throw or even fail compilation
//
//  }
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
      println(result.toString)
      println(matC.toString)
      matC.equals(result) must beTrue
    }

    "throw a type error if you attmept to add two wrongly sized matrices" in {
      val a = new MatrixImpl(Array(Array(1, 2)))
      val b = new MatrixImpl(Array(Array(1)))
      a.add(b) must throwA(new RuntimeException("Matrices are the wrong size"))
    }

  }
}
