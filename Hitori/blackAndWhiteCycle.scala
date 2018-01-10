package Hitori


import Hitori.HitoriIsValid._
import Hitori.HitoriSolver._


object blackAndWhiteCycle {

  var cycleChangedSqs: List[Square] = Nil

  //def setBlack : find fixed(set) 'B' sq >> make surrounding square white
  def setBlack() = {
    val blackSqs: List[Square] = getPossibleList().filter(sq => sq.color == 'B' && sq.smartIsFixed)

    for (blackSq <- blackSqs) {
      val curPos: Pos = Pos(blackSq.x, blackSq.y)

      val neiSqsPos: List[Pos] = List(
        Pos(curPos.x - 1, curPos.y),
        Pos(curPos.x + 1, curPos.y),
        Pos(curPos.x, curPos.y - 1),
        Pos(curPos.x, curPos.y + 1)
      )

      val neiSqsPos_filtered: List[Pos] = neiSqsPos.filter(pos => boundaryCheck(pos) && !getSquare(pos).smartIsFixed && !getSquare(pos).isFixed)

      for (neiSqPos_filtered <- neiSqsPos_filtered) {
        val curSq: Square = getSquare(neiSqPos_filtered)
        curSq.setColorToWhite_guess()

        cycleChangedSqs = cycleChangedSqs :+ curSq
      }
    }

  }


  //def setWhite : find fixed(set) 'W' sq >> make dup squares black
  def setWhite() = {
    val whiteSqs: List[Square] = getPossibleList().filter(sq => sq.color == 'W' && sq.smartIsFixed)

    for (whiteSq <- whiteSqs) {
      val curPos: Pos = Pos(whiteSq.x, whiteSq.y)
      val curSqIdx: Int = getSquare(curPos).idx
      val curSqVal: Int = getSquare(curPos).value

      val curRow: List[Square] = getRow(curPos.y).filter(!_.isFixed)
      val curCol: List[Square] = getColumn(curPos.x).filter(!_.isFixed)

      curRow.filter(sq => sq.value == curSqVal && !sq.smartIsFixed && sq.idx != curSqIdx).foreach { sq =>
        sq.setColorToBlack_guess()
        cycleChangedSqs = cycleChangedSqs :+ sq
      }

      curCol.filter(sq => sq.value == curSqVal && !sq.smartIsFixed && sq.idx != curSqIdx).foreach { sq =>
        sq.setColorToBlack_guess()
        cycleChangedSqs = cycleChangedSqs :+ sq
      }


    }
  }


  //get unfixed squares
  def getPossibleList(): List[Square] = Squares.filter(!_.isFixed)

  //check if solving is or not(check all squares are fixed)
  def isFinished: Boolean = {
    Squares.forall(_.isFixed)
  }

  /*
  1. set target square to white (result1)
  2. set target square to black (result2)
        result1 result2
           F       F    : error state(target square has to have at least one solution)
           F       T    : target square has to be 'B'(black)
           T       F    : target square has to be 'W'(white)
           T       T    : can not fix target square to certain color
   */
  def smartBruteForce(): Unit = {
    val possList: List[Square] = getPossibleList()

    for (sq <- possList) {

      if (!sq.isFixed) {


        //guess curSquare as white and see if it satisfied the rules
        sq.setColorToWhite_guess()
        cycleChangedSqs = cycleChangedSqs :+ sq

        //curSqToWhite : result1
        val curSqToWhite: Boolean = setBWCycle()
        val curSqToWhite_changedSqsIdx: List[Int] = cycleChangedSqs.map(_.idx)
        val curSqToWhite_changedSqsColor: List[Char] = cycleChangedSqs.map(_.color)
        val curSqToWhite_changedSqs: Map[Int, Char] = (curSqToWhite_changedSqsIdx zip curSqToWhite_changedSqsColor).toMap

        cycleChangedSqs.foreach(_.color = 'G')
        cycleChangedSqs.foreach(_.smartIsFixed = false)
        cycleChangedSqs = Nil

        //guess curSquare as black and see if it satisfied the rules
        sq.setColorToBlack_guess()
        cycleChangedSqs = cycleChangedSqs :+ sq

        //curSqToWhite : result2
        val curSqToBlack: Boolean = setBWCycle()
        val curSqToBlack_changedSqsIdx: List[Int] = cycleChangedSqs.map(_.idx)
        val curSqToBlack_changedSqsColor: List[Char] = cycleChangedSqs.map(_.color)
        val curSqToBlack_changedSqs: Map[Int, Char] = (curSqToBlack_changedSqsIdx zip curSqToBlack_changedSqsColor).toMap

        cycleChangedSqs.foreach(_.color = 'G')
        cycleChangedSqs.foreach(_.smartIsFixed = false)
        cycleChangedSqs = Nil

        //decision tree(using result1, result2)
        val smartBF_decisionNode: List[Boolean] = List(curSqToWhite, curSqToBlack)
        smartBF_decisionNode match {
          case List(true, true) => {
          }
          case List(true, false) => {
            curSqToWhite_changedSqs.foreach(sq => getSquare(sq._1).setColor_fix(sq._2))
          }
          case List(false, true) => {
            curSqToBlack_changedSqs.foreach(sq => getSquare(sq._1).setColor_fix(sq._2))
          }
          case List(false, false) => {
            println("error state!")
          }
        }

      }
    }

    if (!isFinished) {
      smartBruteForce()
    }
  }


  //apply setBlack() & setWhite() until no change find
  def setBWCycle(): Boolean = {

    while (true) {
      val cntChanged: Int = cycleChangedSqs.length

      setBlack()
      setWhite()

      val isValid = isValid_map()

      if (cycleChangedSqs.length == cntChanged || !isValid) {
        return isValid
      }
    }

    true


  }

}


