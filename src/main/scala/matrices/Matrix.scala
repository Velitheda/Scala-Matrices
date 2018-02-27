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


implicit class MatrixOps[M](matrix: Matrix)(implicit evidence: Matrix[M]) {
  def +(other: Matrix) = evidence.add(other)//How do I add the other?
}

def add[M :Matrix](matrix: Matrix)