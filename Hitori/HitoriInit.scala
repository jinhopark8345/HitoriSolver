package Hitori

import java.io.File

import Hitori.HitoriSolver.{Squares, mapAreaSize, mapLength, valList}

object HitoriInit {

  //read puzzle and making valList(have only square numbers), mapLength, mapAreaSize
  def initSolving_readPuzzle(inputPath: String) {

    val inputFile = scala.io.Source.fromFile(new File(inputPath))
    val lines = inputFile.mkString.split("\n")

    for (curLine <- lines) {
      val curStrWithNoTap = curLine.replaceAll("\t", " ")
      val curStr = curStrWithNoTap.replaceAll("\r", "")
      val cleanCurStr = curStr.split(" ")
      for (curStrVal <- cleanCurStr if curStrVal != "") {
        val curIntVal = curStrVal.toInt
        valList = valList ::: List(curIntVal)
      }
    }
    mapLength = lines.length
    mapAreaSize = mapLength * mapLength

    inputFile.close()
  }

  //
  def initSolving_printPuzzle() {
    println("\nprinting the initial file")

    valList.zipWithIndex.foreach {
      case (sqNum, index) =>
        if ((index + 1) % mapLength == 0)
          printf(sqNum + " " + "\n")
        else
          printf(sqNum + " ")
    }
  }

  //making squares(preparing solving)
  def initSolving_makeSquares() {
    for (y <- 1 to mapLength; x <- 1 to mapLength) {
      val idx = (x - 1) + (y - 1) * mapLength
      val value = valList(idx)
      val s = new Square(
        sqX = x,
        sqY = y,
        sqIdx = idx,
        sqVal = value,
        sqColor = 'G')
      Squares = Squares :+ s
    }
  }

  //init file
  def initSolving(inputPath: String) {

    initSolving_readPuzzle(inputPath)
    initSolving_printPuzzle()
    initSolving_makeSquares()

  }


}
