package matrices

import spire.math._
import spire.implicits._
import spire.algebra._

trait Matrix[M] {
  // Should this be 'apply a function to corresponding elements on two matrices'?
  def add(matrix: M, other: M): M
  def -(m: M, other: M): M = add(m, function(other, (e) => e * -1))
  def multiply(matrix: M, other: M): M

  // TODO: Validate dimensions
  def dotProduct(matrix: M, other: M): Number

  // TODO: handle Numeric types other than Number
  def function(matrix: M, f: (Number) => Number): M
  def transpose(matrix: M): M
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
  def multiplyRow(matrix: M, rowIndex: Int, multiplier: Number): M

  def getElement(matrix: M, rowIndex: Int, columnIndex: Int): Number

  def cofactorMatrix(matrix: M): M
  def adjoint(matrix: M): M = transpose(cofactorMatrix(matrix))
  def inverse(matrix: M): M = function(adjoint(matrix), e => e * (1 / det(matrix)))

  def det(matrix: M): Number
  def identity(size :Int): M
}

object MatrixOps {
  implicit class ExposedMatrixOps[M](m: M)(implicit ops: Matrix[M]) {
    def +(other: M): M = ops.add(m, other)
    def -(other: M): M = ops.-(m, other)
    def *(other: M): M = ops.multiply(m, other)
    def dotProduct(other: M): Number = ops.dotProduct(m, other)
    def function(f: (Number) => Number): M = ops.function(m, f)
    def transpose(): M = ops.transpose(m)
    def isEqual(other: M): Boolean = ops.isEqual(m, other)

    def numColumns(): Int = ops.numColumns(m)
    def numRows(): Int = ops.numRows(m)

    def getColumn(columnIndex: Int): M = ops.getColumn(m, columnIndex)
    def getRow(rowIndex: Int): M = ops.getRow(m, rowIndex)

    def removeRow(rowIndex: Int): M = ops.removeRow(m, rowIndex)
    def removeColumn(columnIndex: Int): M = ops.removeColumn(m, columnIndex)

    def multiplyRow(rowIndex: Int, multiplier: Number): M = ops.multiplyRow(m, rowIndex, multiplier)


    def getElement(rowIndex: Int, columnIndex: Int): Number = ops.getElement(m, rowIndex, columnIndex)

    def det(): Number = ops.det(m)

    def identity(size: Int): M = ops.identity(size)
    def inverse(): M = ops.inverse(m)
  }
}

