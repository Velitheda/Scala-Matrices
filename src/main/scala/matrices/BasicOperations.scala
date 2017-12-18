package matrices


trait BasicOperations extends Matrix {
  def sameDimensions(other: Matrix): Boolean = {
    getRows == other.getRows && getColumns == other.getColumns
  }

  //def equal inverted dimensions

  private def performOperation(other: Matrix)(operation: Function2[Int, Int, Int]): MatrixImpl = {
    if(!sameDimensions(other)){
      throw new RuntimeException("Matrices are the wrong size")
    }
    new MatrixImpl(
      (other.body zip body).map { case (row, thisRow) =>
        operateRows(row, thisRow)(operation)
      }
    )
  }

  private def operateRows(row: Array[Int], thisRow: Array[Int])(operation: Function2[Int, Int, Int]): Array[Int] = {
    (row zip thisRow).map{
      case (i, j) => operation(i, j)
    }
  }

  def add(matrix: Matrix): MatrixImpl = {
    performOperation(matrix)(_ + _)
  }
}
