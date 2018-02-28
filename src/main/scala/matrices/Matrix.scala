package matrices


trait Matrix[M] {
  def add(matrix: M, other: M): M
  def multiply(matrix: M, other: M): M

  // need to enforce that T is the type of the elements inside M
  def elementsFunction[T](matrix: M, f: (T) => T): M
  //def transpose(matrix: M): M
  def isEqual(matrix: M, other: M): Boolean
}

object MatrixOps {
  implicit class ExposedMatrixOps[M](m: M)(implicit ops: Matrix[M]) {
    def +(other: M): M = ops.add(m, other)
    def *(other: M): M = ops.multiply(m, other)
    def elementsFunction[T](f: (T) => T): M = ops.elementsFunction[T](m, f)
    def isEqual(other: M): Boolean = ops.isEqual(m, other)

    def -(other: M): M = ops.add(m, other.elementsFunction((e: Int) => e * -1))
  }
}

case class ArrayMatrix(body: Array[Array[Int]])
object ArrayMatrix {
  implicit object ArrayMatrixImpl extends Matrix[ArrayMatrix] {
    def add(matrix: ArrayMatrix, other: ArrayMatrix) = matrix // + other TODO: implement
    def multiply(matrix: ArrayMatrix, other: ArrayMatrix) = matrix // * other TODO: implement
    def elementsFunction[Int](matrix: ArrayMatrix, f: (Int) => Int): ArrayMatrix = {
      ArrayMatrix(Array(Array(1)))
    }
    //is .equals enough?
    def isEqual(matrix: ArrayMatrix, other: ArrayMatrix) = false // * other TODO: implement
  }
}
