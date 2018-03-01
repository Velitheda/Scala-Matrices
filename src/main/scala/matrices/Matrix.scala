package matrices


trait Matrix[M] {
  //TODO: Should this be apply a function to corresponding elements on two matrices?
  def add(matrix: M, other: M): M
  def multiply(matrix: M, other: M): M

  // need to enforce that T is the type of the elements inside M
  def function(matrix: M, f: (Int) => Int): M
  def transpose(matrix: M): M
  //TODO: decide if .equals is enough
  def isEqual(matrix: M, other: M): Boolean

  /*
  def numRows: Int
  def numColumns: Int

  def rows: ???
  def columns: ???
   */
}

object MatrixOps {
  implicit class ExposedMatrixOps[M](m: M)(implicit ops: Matrix[M]) {
    def +(other: M): M = ops.add(m, other)
    def *(other: M): M = ops.multiply(m, other)
    def function(f: (Int) => Int): M = ops.function(m, f)
    def transpose(): M = ops.transpose(m)
    def isEqual(other: M): Boolean = ops.isEqual(m, other)

    // TODO: handle Numeric types properly
    def -(other: M): M = ops.add(m, other.function((e: Int) => e * -1))
  }
}

case class ArrayMatrix(rows: Array[Array[Int]])
object ArrayMatrix {

  private def dotProduct(row: Array[Int], column: Array[Int]): Int = {
    val s = row zip column map {t => t._1 * t._2} //.sum
    s.sum
  }
  
  private def numColumns(matrix: ArrayMatrix): Int = matrix.rows.headOption.getOrElse(Array()).length

  implicit object ArrayMatrixImpl extends Matrix[ArrayMatrix] {
    def add(matrix: ArrayMatrix, other: ArrayMatrix) = {
      ArrayMatrix(matrix.rows zip other.rows map {
        case(row, otherRow) => row zip otherRow map { case(element, otherElement) => element + otherElement }
      })
    }
    def multiply(matrix: ArrayMatrix, other: ArrayMatrix) = {
      //each row takes the dot product with each column to get a set of new rows
      val result = matrix.rows.map(row => transpose(other).rows.map { column => dotProduct(row, column)})
      ArrayMatrix(result)
    }
    def function(matrix: ArrayMatrix, f: (Int) => Int): ArrayMatrix = {
      ArrayMatrix(matrix.rows.map(_.map(f)))
    }
    def transpose(matrix: ArrayMatrix): ArrayMatrix = {
      //TODO: enforce these are rectangular.
      ArrayMatrix(
        Array.tabulate(numColumns(matrix)){ columnIndex =>
          matrix.rows.map(row => row(columnIndex))
        }
      )
    }

    def isEqual(matrix: ArrayMatrix, other: ArrayMatrix) = {
      val a = matrix.rows zip other.rows flatMap {
        case(row, otherRow) => row zip otherRow map { case(element, otherElement) => element == otherElement }
      }
      !a.toSet.contains(false)
    }

  }
}
