package Hitori

import Hitori.HitoriSolver._
import Hitori.HitoriIsValid._

object PreFilters {

  def fixObviousWhite() {
    Squares.filter(sq => fixObviousWhite_noDupNum(Pos(sq.x, sq.y))).foreach(sq => sq.setColor_fix('W'))
  }

  def fixObviousWhite_noDupNum(pos: Pos): Boolean = {

    val x: Int = pos.x
    val y: Int = pos.y

    val curRow = getRow(y)
    val curCol = getColumn(x)

    val curSqVal = getSqValue(x, y)
    val curSqIdx = getSquare(x, y).idx
    val noDupNumInRow: Boolean = curRow.filterNot(_.idx == curSqIdx).forall(_.value != curSqVal)
    val noDupNumInCol: Boolean = curCol.filterNot(_.idx == curSqIdx).forall(_.value != curSqVal)

    if (noDupNumInRow && noDupNumInCol)
      true
    else
      false
  }

  def fixSandwichPair() {

    (1 to mapLength).foreach(f = i => {
      val curRow: List[Square] = getRow(i)
      val curCol: List[Square] = getColumn(i)
      fixSandwichPair_line(curRow)
      fixSandwichPair_line(curCol)
    })

  }

  def fixSandwichPair_line(curLine: List[Square]) {
    (1 to mapLength - 2).foreach(f = i => {
      val firSq: Square = curLine(i - 1)
      val secSq: Square = curLine(i)
      val thiSq: Square = curLine(i + 1)
      if (firSq.value == thiSq.value && firSq.value != secSq.value) {
        secSq.setColor_fix('W')
      }
    })
  }

  def fixPairIsolation(): Unit = {
    (1 to mapLength).foreach(f = i => {
      val curRow: List[Square] = getRow(i)
      val curCol: List[Square] = getColumn(i)
      fixPairIsolation_line(curRow)
      fixPairIsolation_line(curCol)
    })
  }


  case class tripleSqs(firstSq: Square, secondSq: Square, thirdSq: Square)

  case class quadSqs(firstSq: Square, secondSq: Square, thirdSq: Square, fourthSq: Square)

  def fixPairIsolation_line_isNeighborSqsDiffVal(curSq: Square, curLine: List[Square]): Boolean = {
    val prevSqIdx: Int = curSq.idx - 1
    val nextSqIdx: Int = curSq.idx + 1

    if (curLine.isDefinedAt(prevSqIdx) && curSq.value == getSquare(prevSqIdx).value)
      return false

    if (curLine.isDefinedAt(nextSqIdx) && curSq.value == getSquare(nextSqIdx).value)
      return false
    true
  }

  def fixPairIsolation_line_firstSq(curLine: List[Square]): Unit = {

    val firstSq: Square = curLine(0)
    val secondSq: Square = curLine(1)
    val thridSq: Square = curLine(2)

    if (firstSq.value == secondSq.value && firstSq.value != thridSq.value) {
      for (idx <- 3 to mapLength - 1) {
        val curSq: Square = curLine(idx)
        if (firstSq.value == curSq.value &&
          fixPairIsolation_line_isNeighborSqsDiffVal(curSq, curLine))
          curSq.setColor_fix('B')
      }
    }

  }

  def fixPairIsolation_line(curLine: List[Square]): Unit = {

    (0 until curLine.length).foreach {
      case 0 => fixPairIsolation_line_firstSq(curLine)
      case x if x > 0 => fixPairIsolation_line_otherSqs(x, curLine)

    }
  }

  def fixPairIsolation_line_otherSqs(curIdx: Int, curLine: List[Square]): Unit = {

    val prevSq: Square = getSquare(curIdx - 1)
    val curSq_line: Square = getSquare(curIdx)
    val nextSq: Square = getSquare(curIdx + 1)
    val nextx2Sq: Square = getSquare(curIdx + 2)

    if (curSq_line.value == nextSq.value && curSq_line.value != prevSq.value && curSq_line.value != nextx2Sq.value) {
      for (idx <- curIdx + 3 to curLine.length - 1) {

        val curSq: Square = getSquare(idx)
        if (curSq_line.value == curSq.value &&
          fixPairIsolation_line_isNeighborSqsDiffVal(curSq, curLine))
          curSq.setColor_fix('B')

      }

    }


  }


}
