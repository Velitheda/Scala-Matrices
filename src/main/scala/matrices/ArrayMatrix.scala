package matrices

import MatrixOps._

case class ArrayMatrix(rows: Array[Array[Double]]){
  override def toString: String = {
    "[" + this.rows.map(row => row.mkString(", ")).mkString("]\n")
  }
}

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

    def getRow(matrix: ArrayMatrix, rowIndex: Int): ArrayMatrix =
      ArrayMatrix(Array(matrix.rows(rowIndex)))

    def setRow(matrix: ArrayMatrix, rowIndex: Int, newRow: ArrayMatrix): ArrayMatrix = {
      ArrayMatrix(Array.tabulate(matrix.rows.length) { i =>
        if (i == rowIndex) {
          // could this introduce weird bugs with size?
          newRow.rows.headOption.getOrElse(Array())
        } else {
          matrix.rows(i)
        }
      })
    }

    def swapRows(matrix: ArrayMatrix, startIndex: Int, destinationIndex: Int): ArrayMatrix = {
      val rowA = getRow(matrix, startIndex)
      val rowB = getRow(matrix, destinationIndex)
      val rowASet = setRow(matrix, destinationIndex, rowA)
      setRow(rowASet, startIndex, rowB)
    }

    def multiplyRow(matrix: ArrayMatrix, rowIndex: Int, multiplier: Double): ArrayMatrix = {
      val multipliedRow = function(getRow(matrix, rowIndex), (e) => e * multiplier)
      setRow(matrix, rowIndex, multipliedRow)
    }

    // TODO: rename
    def addMultipliedRowToOtherRow(matrix: ArrayMatrix, startIndex: Int, destinationIndex: Int, multiplier: Double): ArrayMatrix = {
      val rowA = function(getRow(matrix, startIndex), (e) => e * multiplier)
      val rowOther = getRow(matrix, destinationIndex)
      setRow(setRow(matrix, destinationIndex, rowA), startIndex, rowOther)
    }

    def removeRow(matrix: ArrayMatrix, rowIndex: Int): ArrayMatrix =
      ArrayMatrix(matrix.rows.view.zipWithIndex.filter(_._2 != rowIndex).map(_._1).toArray)

    def getElement(matrix: ArrayMatrix, rowIndex: Int, columnIndex: Int): Double = matrix.rows(rowIndex)(columnIndex)

    def det(matrix: ArrayMatrix): Double = {
      if(isOneByOne(matrix)) {
        matrix.rows(0)(0)
      }
      else if(isTwoByTwo(matrix)){
        twoByTwoDet(matrix)
      } else {
        // cofactor expansion along the first row
        matrix.rows.head.view.zipWithIndex.map { case(e, i) =>
          getElement(matrix, 0, i) * det(minor(matrix, 0, i)) * cofactorSign(0, i)
        }.sum
      }
    }

    def cofactorSign(rowIndex: Int, columnIndex: Int): Int = {
      ((rowIndex + 1) + (columnIndex + 1)) % 2 match {
        case 0 => 1 //even: +
        case 1 => -1 //odd: -
      }
    }

    // messy, fix how an empty matrix could get one row
    def isEmptyMatrix(matrix: ArrayMatrix): Boolean = numRows(matrix) == 0 || numColumns(matrix) == 0
    def isOneByOne(matrix: ArrayMatrix): Boolean = numRows(matrix) == 1 && numColumns(matrix) == 1
    def isTwoByTwo(matrix: ArrayMatrix): Boolean = numRows(matrix) == 2 && numColumns(matrix) == 2

    def isSquare(matrix: ArrayMatrix): Boolean = numRows(matrix) == numColumns(matrix)

    def twoByTwoDet(matrix: ArrayMatrix): Double = {
      getElement(matrix, 0, 0) * getElement(matrix, 1, 1) - getElement(matrix, 1, 0) * getElement(matrix, 0, 1)
    }

    //rename: this is not the minor.
    def minor(matrix: ArrayMatrix, rowIndex: Int, columnIndex: Int): ArrayMatrix = {
      // TODO handle 1x1 or empty
      removeRow(removeColumn(matrix, columnIndex), rowIndex)
    }

    def cofactorMatrix(matrix: ArrayMatrix): ArrayMatrix = {
      ArrayMatrix(matrix.rows.zipWithIndex.map { case(row, rowIndex) =>
        row.zipWithIndex.map { case(element, columnIndex) =>
          det(minor(matrix, rowIndex, columnIndex)) * cofactorSign(rowIndex, columnIndex)
        }
      })
    }

    def identity(size: Int): ArrayMatrix = {
      ArrayMatrix(Array.tabulate(size)(rowIndex => Array.tabulate(size)(columnIndex => if (rowIndex == columnIndex) 1 else 0)))
    }

  }
}
