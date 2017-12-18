package matrices


trait BasicOperations extends Matrix with Utilities {
  private def performOperation(other: Matrix)(operation: Function2[Int, Int, Int]): MatrixImpl = {
    if(!sameDimensions(other)){
      throw new RuntimeException("Matrices are the wrong size")
    }
    new MatrixImpl(
      (body zip other.body).map { case (row, otherRow) =>
        operateRows(row, otherRow)(operation)
      }
    )
  }

  private def operateRows(row: Array[Int], otherRow: Array[Int])(operation: Function2[Int, Int, Int]): Array[Int] = {
    (row zip otherRow).map{
      case (i, j) => operation(i, j)
    }
  }

  def add(matrix: Matrix): MatrixImpl = {
    performOperation(matrix)(_ + _)
  }

  def subtract(matrix: Matrix): MatrixImpl = {
    performOperation(matrix)(_ - _)
  }
}
