package Hitori

import Hitori.HitoriSolver._
import Hitori.blackAndWhiteCycle.cycleChangedSqs

/*
 rule1 : no duplicated number in same row and column
 rule2 : no black squares are connected
 rule3 : all white squares are connected
*/

object HitoriIsValid {

  case class Pos(x: Int, y: Int)

  //whiteSquares : to check if it's one white chain or not
  var whiteSquares: List[Square] = Nil

  //index -> Posisiton
  def idxToPos(idx: Int): Pos = Pos(idx % mapLength + 1, idx / mapLength + 1)

  //Position -> index
  def posToIdx(pos: Pos): Int = (pos.x - 1) + (pos.y - 1) * mapLength

  //drop one element from list
  //ex) dropIndex(List(1,2,3,4), 2) = List(1,2,4)
  def dropIndex[T](list: List[T], idx: Int): List[T] =
    list.zipWithIndex.filter(_._2 != idx).map(_._1)

  //check whole map is valid or not checking above three rules
  def isValid_map(): Boolean = {
    if (!noDupNum_map() || !isBlackAlone_map() ||
      !isWhiteNotAlone_map() || !isOneWhiteChain())
      return false

    true
  }


  // apply noDumNum function to whole map
  def noDupNum_map() = cycleChangedSqs.forall(sq => noDupNum(sq.idx))

  //rule1 : no duplicated number in same row and column
  def noDupNum(idx: Int): Boolean = {

    val curSq = getSquare(idx)

    if (curSq.color == 'W') {
      val curRow = getRow(curSq.y)
      val curCol = getColumn(curSq.x)

      val noDupNum_row: Boolean = curRow.filter(sq => sq.color != 'B' && sq.idx != curSq.idx).forall(_.value != curSq.value)

      if (!noDupNum_row)
        return false

      val noDupNum_col: Boolean = curCol.filter(sq => sq.color != 'B' && sq.idx != curSq.idx).forall(_.value != curSq.value)

      if (!noDupNum_col)
        return false
    }
    true
  }

  // apply isBlackAlone_sq function to whole map
  def isBlackAlone_map(): Boolean = cycleChangedSqs.forall(sq => isBlackAlone_sq(sq))

  //  rule2 : no black squares are connected
  def isBlackAlone_sq(curSq: Square): Boolean = {
    if (curSq.color == 'B') {
      val x: Int = curSq.x
      val y: Int = curSq.y

      val neighborSqsPos: List[Pos] = List(
        Pos(x - 1, y),
        Pos(x, y - 1),
        Pos(x + 1, y),
        Pos(x, y + 1)
      )

      val neighborSqsPos_filtered: List[Pos] = neighborSqsPos.filter(pos => boundaryCheck(pos))

      for (neiSqPos <- neighborSqsPos_filtered) {
        if (getSquare(neiSqPos).color == 'B') {
          return false
        }
      }
    }

    true
  }

  def isWhiteNotAlone_map(): Boolean = cycleChangedSqs.forall(sq => isWhiteNotAlone_sq(sq))

  /*
  rule3 : all white squares are connected, cheap version(imperfect)
  because checking one white chain is very expensive(using flood fill), firstly check
  each square has any white neighbor square
  */
  def isWhiteNotAlone_sq(curSq: Square): Boolean = {
    if (curSq.color == 'W') {
      val x: Int = curSq.x
      val y: Int = curSq.y

      val neighborSqsPos: List[Pos] = List(
        Pos(x - 1, y),
        Pos(x, y - 1),
        Pos(x + 1, y),
        Pos(x, y + 1)
      )

      val neighborSqsPos_filtered: List[Pos] = neighborSqsPos.filter(pos => boundaryCheck(pos))

      for (neiSqPos <- neighborSqsPos_filtered) {
        val neiSq: Square = getSquare(neiSqPos)
        if (neiSq.color == 'W' || neiSq.color == 'G') {
          //curSq('W') is connected to other white square
          return true
        }
      }

      //curSq('W') is not connected to any other white squares
      return false
    }

    //if curSq.color != 'W'
    true
  }

  // rule3 : all white squares are connected, expensive version
  def isOneWhiteChain(): Boolean = {

    val numOfWhiteSqs: Int = Squares.count(_.color != 'B')
    if (numOfWhiteSqs == getWhiteChain().length) {
      return true
    }

    false
  }


  def getFirstWhiteSq(): Square = {
    for (idx <- 0 to mapAreaSize) {

      val curSq = getSquare(idx)
      val curSqColor = curSq.color
      if (curSqColor != 'B') {

        return curSq
      }
    }
    List[Square]().head
  }


  def getWhiteChain(): List[Square] = {

    val firstWhiteSq: Square = getFirstWhiteSq()
    getWhiteChain_helper(firstWhiteSq)

    val rtvSquares: List[Square] = whiteSquares

    whiteSquares = Nil
    Squares.foreach(_.isVisited_floodFill = false)

    rtvSquares
  }

  def getWhiteChain_helper(nextSq: Square): Unit = {
    val curSq: Square = nextSq
    val curSqX = curSq.x
    val curSqY = curSq.y
    curSq.isVisited_floodFill = true
    whiteSquares = whiteSquares :+ curSq

    val nextSqsPos: List[Pos] = List(
      Pos(curSqX - 1, curSqY),
      Pos(curSqX, curSqY - 1),
      Pos(curSqX + 1, curSqY),
      Pos(curSqX, curSqY + 1)
    )

    val nextSqsPos_filtered = nextSqsPos.filter(pos => boundaryCheck(pos)).filterNot(pos => getSquare(pos).color == 'B')

    for (nextSqPos <- nextSqsPos_filtered) {
      if (!getSquare(nextSqPos).isVisited_floodFill) {
        getWhiteChain_helper(getSquare(nextSqPos))
      }
    }
  }


}
