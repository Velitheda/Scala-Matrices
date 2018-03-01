package matrices


trait Matrix[M] {
  // Should this be 'apply a function to corresponding elements on two matrices'?
  def add(matrix: M, other: M): M
  def multiply(matrix: M, other: M): M

  // TODO: Validate dimensions
  def dotProduct(matrix: M, other: M): Double

  // TODO: handle Numeric types other than double
  def function(matrix: M, f: (Double) => Double): M
  def transpose(matrix: M): M
  //TODO: decide if .equals is enough
  def isEqual(matrix: M, other: M): Boolean

  def numColumns(matrix: M): Int
  def numRows(matrix: M): Int
  def getColumn(matrix: M, columnIndex: Int): M
  def getRow(matrix: M, rowIndex: Int): M

  def det(matrix: M): Double
}

object MatrixOps {
  implicit class ExposedMatrixOps[M](m: M)(implicit ops: Matrix[M]) {
    def +(other: M): M = ops.add(m, other)
    def *(other: M): M = ops.multiply(m, other)
    def dotProduct(other: M): Double = ops.dotProduct(m, other)
    def function(f: (Double) => Double): M = ops.function(m, f)
    def transpose(): M = ops.transpose(m)
    def isEqual(other: M): Boolean = ops.isEqual(m, other)

    def numColumns(): Int = ops.numColumns(m)
    def numRows(): Int = ops.numRows(m)

    def getColumn(columnIndex: Int): M = ops.getColumn(m, columnIndex)
    def getRow(rowIndex: Int): M = ops.getRow(m, rowIndex)

    def det(matrix: M): Double = ops.det(m)
    def -(other: M): M = ops.add(m, other.function((e: Double) => e * -1))
  }
}

case class ArrayMatrix(rows: Array[Array[Double]])
object ArrayMatrix {

  implicit object ArrayMatrixImpl extends Matrix[ArrayMatrix] {
    def add(matrix: ArrayMatrix, other: ArrayMatrix) = {
      ArrayMatrix(matrix.rows zip other.rows map {
        case(row, otherRow) => row zip otherRow map {
          case(element, otherElement) => element + otherElement
        }
      })
    }

    def multiply(matrix: ArrayMatrix, other: ArrayMatrix) = {
//      val result = for(rowIndex <- 0 until numRows(matrix)) yield {
//        Array(
//          for(columnIndex <- 0 until numColumns(other)) yield {
//            dotProduct(getRow(matrix, rowIndex), getColumn(other, columnIndex))
//          }
//        )
//      }

      //each row takes the dot product with each column to get a set of new rows
      val otherColumns = transpose(other).rows
      val result = matrix.rows.map(row => otherColumns.map (column =>
        dotProduct(ArrayMatrix(Array(row)), transpose(ArrayMatrix(Array(column))))
      ))
      ArrayMatrix(result)
    }

    def dotProduct(rowMatrix: ArrayMatrix, columnMatrix: ArrayMatrix): Double = {
      val row = rowMatrix.rows.headOption.getOrElse(Array())
      val column = transpose(columnMatrix).rows.headOption.getOrElse(Array())
      val s = row zip column map { t => t._1 * t._2 }
      s.sum
    }

    def function(matrix: ArrayMatrix, f: (Double) => Double): ArrayMatrix = {
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

    def numColumns(matrix: ArrayMatrix): Int = matrix.rows.headOption.getOrElse(Array()).length
    def numRows(matrix: ArrayMatrix): Int = matrix.rows.length

    def getRow(matrix: ArrayMatrix, columnIndex: Int): ArrayMatrix =
      ArrayMatrix(Array(matrix.rows(columnIndex)))

    def getColumn(matrix: ArrayMatrix, columnIndex: Int): ArrayMatrix =
      transpose(getRow(transpose(matrix), columnIndex))

    def det(matrix: ArrayMatrix): Double = -1
  }
}
