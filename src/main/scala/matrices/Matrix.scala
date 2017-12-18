package matrices

trait Matrix {
  val body: Array[Array[Int]]
  def get(row: Int, column: Int): Int
  def set(row: Int, column: Int)(value: Int): Matrix
  def add(matrix: Matrix): Matrix
  def subtract(matrix: Matrix): Matrix
  def getRows: Int
  def getColumns: Int
  def toString: String
  def equals(matrix: Matrix): Boolean
}
