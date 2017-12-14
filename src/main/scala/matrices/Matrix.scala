package matrices

trait Matrix {
  val body: Array[Array[Int]]
  def get(row: Int, column: Int): Int
  def set(row: Int, column: Int)(value: Int): Matrix
  def add(matrix: Matrix): Matrix
  def getRows: Int
  def getColumns: Int
  def toString: String
}

class MatrixImpl(bdy: Array[Array[Int]]) extends Matrix {
  val body: Array[Array[Int]] = bdy
  lazy val rows = body.length

  def this(rows: Int, columns: Int){
    this(Array.tabulate(rows, columns)((a,b) => 1))
  }

  def getRows = this.body.length

  def getColumns = this.body.foldLeft[Int](0){(a, b) => b.length}

  def get(row: Int, column: Int): Int = {
    body(row)(column)
  }

  def set(row: Int, column: Int)(value: Int): MatrixImpl = {
    val newRow = body(row).updated(column, value)
    new MatrixImpl(body.updated(row, newRow))
  }

  def add(matrix: Matrix): MatrixImpl = {
    val newBody = if(matrix.body.length == this.body.length) {
      (matrix.body zip this.body).map {
        case (mRow: Array[Int], thisRow: Array[Int]) if mRow.length == thisRow.length =>
          (mRow zip thisRow).map {
            case (i, j) => i + j
          }
        case other =>
          throw new RuntimeException("Matrix columns are the wrong size")
      }
    }
    else throw new RuntimeException("Matrix rows are the wrong size")
    new MatrixImpl(newBody)
  }

  override def toString: String = {
    this.body.map(_.mkString(", ")).mkString("\n")
  }
}

object MatrixImpl {


}
