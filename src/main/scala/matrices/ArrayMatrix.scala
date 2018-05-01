package matrices

import spire.math._
import spire.implicits._
import spire.algebra._

import MatrixOps._

case class ArrayMatrix[T <: Number](rows: Array[Array[T]])(implicit override val numberOps: Numeric[T]) extends Matrixable {
  override final type Num = T

  override def toString: String = {
    "\n[" + this.rows.map(row => row.mkString(",")).mkString("]\n[") + "]\n"
  }
}

object ArrayMatrix {

  implicit object ArrayMatrixImpl extends Matrix[ArrayMatrix] {
    def add[Num](matrix: ArrayMatrix[Num], other: ArrayMatrix[Num])(implicit ev: Numeric[Num]) = {
      ArrayMatrix(matrix.rows zip other.rows map {
        case(row, otherRow) => row zip otherRow map {
          case(element, otherElement) => ev.plus(element, otherElement)
        }
      })
    }

    def multiply[Num](matrix: ArrayMatrix[Num], other: ArrayMatrix[Num]) = {
      //each row takes the dot product with each column to get a set of new rows
      val otherColumns = transpose(other).rows
      val result = matrix.rows.map(row => otherColumns.map (column =>
        dotProduct(ArrayMatrix(Array(row)), transpose(ArrayMatrix(Array(column))))
      ))
      ArrayMatrix(result)
    }

    def dotProduct[Num](rowMatrix: ArrayMatrix[Num], columnMatrix: ArrayMatrix[Num])(implicit ev: Numeric[Num]): Num = {
      val row = rowMatrix.rows.headOption.getOrElse(Array())
      val column = transpose(columnMatrix).rows.headOption.getOrElse(Array())
      val s = row zip column map { t => ev.times(t._1, t._2) }
      s.foldLeft(ev.zero)((acc, next) => ev.plus(acc, next))
    }

    //TODO: Should the function use Num?
    def function[Num](matrix: ArrayMatrix[Num], f: (Num) => Num): ArrayMatrix[Num] = {
      ArrayMatrix(matrix.rows.map(_.map(f)))
    }

    def transpose[Num](matrix: ArrayMatrix[Num]): ArrayMatrix[Num] = {
      //TODO: enforce these are rectangular.
      ArrayMatrix[Num](
        Array.tabulate(numColumns(matrix)){ columnIndex =>
          matrix.rows.map(row => row(columnIndex))
        }
      )
    }

    def isEqual[Num](matrix: ArrayMatrix[Num], other: ArrayMatrix[Num]) = {
      val a = matrix.rows zip other.rows flatMap {
        case(row, otherRow) => row zip otherRow map { case(element, otherElement) => element == otherElement }
      }
      !a.toSet.contains(false)
    }

    def numColumns[Num](matrix: ArrayMatrix[Num]): Int = matrix.rows.headOption.getOrElse(Array()).length
    def numRows[Num](matrix: ArrayMatrix[Num]): Int = matrix.rows.length

    def getRow[Num](matrix: ArrayMatrix[Num], rowIndex: Int): ArrayMatrix[Num] =
      ArrayMatrix[Num](Array(matrix.rows(rowIndex)))

    def setRow[Num](matrix: ArrayMatrix[Num], rowIndex: Int, newRow: ArrayMatrix[Num]): ArrayMatrix[Num] = {
      ArrayMatrix[Num](Array.tabulate(matrix.rows.length) { i =>
        if (i == rowIndex) {
          // could this introduce weird bugs with size?
          newRow.rows.headOption.getOrElse(Array())
        } else {
          matrix.rows(i)
        }
      })
    }

    def swapRows[Num](matrix: ArrayMatrix[Num], startIndex: Int, destinationIndex: Int): ArrayMatrix[Num] = {
      val rowA = getRow(matrix, startIndex)
      val rowB = getRow(matrix, destinationIndex)
      val rowASet = setRow(matrix, destinationIndex, rowA)
      setRow(rowASet, startIndex, rowB)
    }

    def multiplyRow[Num](matrix: ArrayMatrix[Num], rowIndex: Int, multiplier: Num)(implicit ev: Numeric[Num]): ArrayMatrix[Num] = {
      val multipliedRow = function(getRow(matrix, rowIndex), (e: Num) => ev.times(e, multiplier))
      setRow(matrix, rowIndex, multipliedRow)
    }

    // TODO: rename
    def addMultipliedRowToOtherRow[Num](matrix: ArrayMatrix[Num], startIndex: Int, destinationIndex: Int, multiplier: Num)
                                       (implicit ev: Numeric[Num]): ArrayMatrix[Num] = {
      val rowA = function(getRow(matrix, startIndex), (e: Num) => e * multiplier)
      val rowOther = getRow(matrix, destinationIndex)
      setRow(setRow(matrix, destinationIndex, rowA), startIndex, rowOther)
    }

    def removeRow[Num](matrix: ArrayMatrix[Num], rowIndex: Int): ArrayMatrix[Num] =
      ArrayMatrix(matrix.rows.view.zipWithIndex.filter(_._2 != rowIndex).map(_._1).toArray)

    def getElement[Num](matrix: ArrayMatrix[Num], rowIndex: Int, columnIndex: Int): Num = matrix.rows(rowIndex)(columnIndex)

    def det[Num](matrix: ArrayMatrix[Num])(implicit ev: Numeric[Num]): Num = {
      if(isOneByOne(matrix)) {
        matrix.rows(0)(0)
      }
      else if(isTwoByTwo(matrix)){
        twoByTwoDet(matrix)
      } else {
        // cofactor expansion along the first row
        matrix.rows.head.view.zipWithIndex.map { case(e, i) =>
          ev.times(getElement(matrix, 0, i), ev.times(det(removeRowAndColumn(matrix, 0, i)), cofactorSign(0, i)))
        }.foldLeft(ev.zero)(ev.plus)
      }
    }

    def cofactorSign(rowIndex: Int, columnIndex: Int): Int = {
      ((rowIndex + 1) + (columnIndex + 1)) % 2 match {
        case 0 => 1 //even: +
        case 1 => -1 //odd: -
      }
    }

    // messy, fix how an empty matrix could get one row
    def isEmptyMatrix[Num](matrix: ArrayMatrix[Num]): Boolean = numRows(matrix) == 0 || numColumns(matrix) == 0
    def isOneByOne[Num](matrix: ArrayMatrix[Num]): Boolean = numRows(matrix) == 1 && numColumns(matrix) == 1
    def isTwoByTwo[Num](matrix: ArrayMatrix[Num]): Boolean = numRows(matrix) == 2 && numColumns(matrix) == 2

    def isSquare(matrix: ArrayMatrix): Boolean = numRows(matrix) == numColumns(matrix)

    def twoByTwoDet[Num](matrix: ArrayMatrix[Num])(implicit ev: Numeric[Num]): Num = {
      ev.minus(ev.times(getElement(matrix, 0, 0), getElement(matrix, 1, 1)), ev.times(getElement(matrix, 1, 0), getElement(matrix, 0, 1)))
    }

    def removeRowAndColumn[Num](matrix: ArrayMatrix[Num], rowIndex: Int, columnIndex: Int)(implicit ev: Numeric[Num]): ArrayMatrix[Num] = {
      // TODO handle 1x1 or empty
      removeRow(removeColumn(matrix, columnIndex), rowIndex)
    }

    def cofactorMatrix[Num](matrix: ArrayMatrix[Num])(implicit ev: Numeric[Num]): ArrayMatrix[Num] = {
      ArrayMatrix(matrix.rows.zipWithIndex.map { case(row, rowIndex) =>
        row.zipWithIndex.map { case(element, columnIndex) =>
          ev.times(det(removeRowAndColumn(matrix, rowIndex, columnIndex)), cofactorSign(rowIndex, columnIndex))
        }
      })
    }

    def identity[Num](size: Int)(implicit ev: Numeric[Num]): ArrayMatrix[Num] = {
      ArrayMatrix[Num](Array.tabulate(size)(rowIndex => Array.tabulate(size)(columnIndex => if (rowIndex == columnIndex) ev.one else ev.zero)))
    }

  }
}
