package matrices


trait Utilities extends Matrix {
  def getRows = body.length

  def getColumns = body.foldLeft[Int](0){(a, b) => b.length}

  def get(row: Int, column: Int): Int = {
    body(row)(column)
  }

  def set(row: Int, column: Int)(value: Int): MatrixImpl = {
    val newRow = body(row).updated(column, value)
    new MatrixImpl(body.updated(row, newRow))
  }

  def sameDimensions(other: Matrix): Boolean = {
    getRows == other.getRows && getColumns == other.getColumns
  }

  override def toString: String = {
    body.map(_.mkString(", ")).mkString("\n")
  }

  //def equal inverted dimensions

  def equals(other: Matrix): Boolean = {
    println("the same")
    if(!sameDimensions(other)){
      return false
    }
    (other.body zip body).flatMap { case (row, thisRow) =>
      (row zip thisRow).map{
        case (i, j) => i == j
      }
    }.forall(b => b)
  }

}
