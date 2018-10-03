package matrices

import MatrixOps._

object Main {

  def main(args: Array[String]): Unit =  {
    println("hi")
  }

  val m = new ArrayMatrix(Array(Array(1)))
  m + m


//  case class ListMatrix(array: List[List[Int]])
//  implicit object MatrixOperationsList extends Matrix[ListMatrix] {
//    def add(matrix: ListMatrix, other: ListMatrix) = matrix // + other TODO: implement
//    def multiply(matrix: ListMatrix, other: ListMatrix) = matrix // * other TODO: implement
//  }
//
//  val listM = ListMatrix(List(List(1)))
//
//  listM + listM

}
