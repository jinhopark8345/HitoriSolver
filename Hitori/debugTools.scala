package Hitori


import Hitori.HitoriIsValid.Pos
import Hitori.HitoriSolver._

object debugTools {

  def debug_printMapColor() {
    println(" debug_printMapColor : ")

    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curSqColor = HitoriSolver.getSquare(x, y).color
        printf(curSqColor + " ")
      }
      println()
    }
    printf(" mapSize : " + mapAreaSize + " num of fixed sqs : " + Squares.filter(_.isFixed).length)
    println()
  }

  def debug_printMap_IsFixed() {
    println(" debug_printMap_IsFixed : ")
    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curSqColor = HitoriSolver.getSquare(x, y).isFixed
        printf(curSqColor + " ")
      }
      println()
    }
    printf(" mapSize : " + mapAreaSize + " num of fixed sqs : " + Squares.filter(_.isFixed).length)
    println()
  }

  def debug_printMap_smartIsFixed() {
    println(" debug_printMap_smartIsFixed : ")
    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curSqColor = HitoriSolver.getSquare(x, y).smartIsFixed
        printf(curSqColor + " ")
      }
      println()
    }
    println()
  }

  def debug_printMap_isVisitedFloodFill() {
    println(" debug_printMap_isVisitedFloodFill : ")
    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curSqColor = HitoriSolver.getSquare(x, y).isVisited_floodFill
        printf(curSqColor + " ")
      }
      println()
    }
    println()
  }


  def debug_noDupNum() {
    println(" debug_noDupNum : ")

    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curNoDupNumTest = HitoriIsValid.noDupNum(getSquare(x, y).idx)
        if (curNoDupNumTest) {
          printf(curNoDupNumTest + "  ")

        }
        else {
          printf(curNoDupNumTest + " ")

        }
      }
      println()
    }
    println()
  }

  def debug_isBlackAlone() {
    println(" debug_isBlackAlone : ")

    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curSqBlackNeighborTest = HitoriIsValid.isBlackAlone_map()

        if (curSqBlackNeighborTest) {
          printf(curSqBlackNeighborTest + "  ")

        }
        else {
          printf(curSqBlackNeighborTest + " ")

        }

      }
      println()
    }
    println()

  }

  def debug_isWhiteNotAlone_map(): Unit = {
    println(" debug_isWhiteNotAlone_map : ")

    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curisWhiteNotAlone_map_test = HitoriIsValid.isWhiteNotAlone_map()

        if (curisWhiteNotAlone_map_test) {
          printf(curisWhiteNotAlone_map_test + "  ")

        }
        else {
          printf(curisWhiteNotAlone_map_test + " ")

        }

      }
      println()
    }
    println()


  }

}
