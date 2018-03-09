package matrices


trait Matrix[M] {
  // Should this be 'apply a function to corresponding elements on two matrices'?
  def add(matrix: M, other: M): M
  def -(m: M, other: M): M = add(m, function(other, (e: Double) => e * -1))
  def multiply(matrix: M, other: M): M

  // TODO: Validate dimensions
  def dotProduct(matrix: M, other: M): Double

  // TODO: handle Numeric types other than double
  def function(matrix: M, f: (Double) => Double): M
  def transpose(matrix: M): M
  //TODO: decide if .equals is enough
  def isEqual(matrix: M, other: M): Boolean

  def numRows(matrix: M): Int
  def numColumns(matrix: M): Int
  def getRow(matrix: M, rowIndex: Int): M
  def getColumn(matrix: M, columnIndex: Int): M = transpose(getRow(transpose(matrix), columnIndex))

  protected def setColumn(matrix: M, columnIndex: Int, column: M): M =
    transpose(setRow(transpose(matrix), columnIndex, column))
  protected def setRow(matrix: M, rowIndex: Int, row: M): M

  def removeRow(matrix: M, rowIndex: Int): M
  def removeColumn(matrix: M, columnIndex: Int): M = transpose(removeRow(transpose(matrix), columnIndex))

  protected def swapRows(matrix: M, startIndex: Int, destinationIndex: Int): M
  protected def multiplyRow(matrix: M, rowIndex: Int, multiplier: Double): M

  def getElement(matrix: M, rowIndex: Int, columnIndex: Int): Double

  def cofactorMatrix(matrix: M): M
  def adjoint(matrix: M): M = transpose(cofactorMatrix(matrix))
  def inverse(matrix: M): M = function(adjoint(matrix), e => e * (1 / det(matrix)))

  def det(matrix: M): Double
  def identity(size :Int): M
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

    def removeRow(rowIndex: Int): M = ops.removeRow(m, rowIndex)
    def removeColumn(columnIndex: Int): M = ops.removeColumn(m, columnIndex)

    def getElement(rowIndex: Int, columnIndex: Int): Double = ops.getElement(m, rowIndex, columnIndex)

    def det(): Double = ops.det(m)
    def -(other: M): M = ops.-(m, other)

    def identity(size: Int): M = ops.identity(size)
  }
}

