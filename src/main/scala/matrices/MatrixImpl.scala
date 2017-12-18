package matrices


class MatrixImpl(bdy: Array[Array[Int]]) extends BasicOperations with Utilities {
  val body: Array[Array[Int]] = bdy

  // not needed?
  def this(rows: Int, columns: Int){
    this(Array.tabulate(rows, columns)((a, b) => 1))
  }

}

