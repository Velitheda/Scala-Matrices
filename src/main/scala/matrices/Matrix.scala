package matrices

import spire.math._
import spire.implicits._
import spire.algebra._

trait Matrixable {
  type Num <: Number
  implicit def numberOps: Numeric[Num]
}

trait Matrix[M <: Matrixable] {
  // Should this be 'apply a function to corresponding elements on two matrices'?
  def add(matrix: M, other: M): M
  def -(m: M, other: M)(implicit ev: Numeric[M#Num]): M = add(m, function(other, (e) => ev.times(e, ev.multiplicative.empty)))
  def multiply(matrix: M, other: M): M

  // TODO: Validate dimensions
  def dotProduct(matrix: M, other: M): M#Num

  def function(matrix: M, f: (M#Num) => M#Num): M
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
  def multiplyRow(matrix: M, rowIndex: Int, multiplier: M#Num): M

  def getElement(matrix: M, rowIndex: Int, columnIndex: Int): M#Num

  def cofactorMatrix(matrix: M): M
  def adjoint(matrix: M): M = transpose(cofactorMatrix(matrix))

  def inverse(matrix: M)(implicit ev: Numeric[M#Num]): M = {
    val detInverse = ev.multiplicative.inverse(det(matrix))
    function(adjoint(matrix), e => ev.times(e, detInverse))
  }

  def det(matrix: M): M#Num
  def identity(size :Int): M
}

object MatrixOps {
  implicit class ExposedMatrixOps[M <: Matrixable](m: M)(implicit ops: Matrix[M]) {
    def +(other: M): M = ops.add(m, other)
    def -(other: M)(implicit ev: Numeric[M#Num]): M = ops.-(m, other)
    def *(other: M): M = ops.multiply(m, other)
    def dotProduct(other: M): M#Num = ops.dotProduct(m, other)
    def function(f: (M#Num) => M#Num): M = ops.function(m, f)
    def transpose(): M = ops.transpose(m)
    def isEqual(other: M): Boolean = ops.isEqual(m, other)

    def numColumns(): Int = ops.numColumns(m)
    def numRows(): Int = ops.numRows(m)

    def getColumn(columnIndex: Int): M = ops.getColumn(m, columnIndex)
    def getRow(rowIndex: Int): M = ops.getRow(m, rowIndex)

    def removeRow(rowIndex: Int): M = ops.removeRow(m, rowIndex)
    def removeColumn(columnIndex: Int): M = ops.removeColumn(m, columnIndex)

    def multiplyRow(rowIndex: Int, multiplier: M#Num): M = ops.multiplyRow(m, rowIndex, multiplier)


    def getElement(rowIndex: Int, columnIndex: Int): M#Num = ops.getElement(m, rowIndex, columnIndex)

    def det(): M#Num = ops.det(m)

    def identity(size: Int): M = ops.identity(size)
    def inverse()(implicit ev: Numeric[M#Num]): M = ops.inverse(m)
  }
}

