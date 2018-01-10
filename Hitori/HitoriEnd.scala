
package Hitori

import java.io.{File, PrintWriter}

import Hitori.HitoriSolver.{mapLength, _}

object HitoriEnd {

  //end solving
  def endSolving(outputPath: String) {
    endSolving_writeSolution(outputPath)
    endSolving_printSolution()
  }

  //print solution file on console
  def endSolving_printSolution() {
    println("\nprinting the solution file")
    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curSqColor = getSquare(x, y).color
        printf(curSqColor + " ")
      }
      println()
    }
    println()

  }


  //write solution file on ../output.txt
  def endSolving_writeSolution(outputPath: String) {

    val writer = new PrintWriter(new File(outputPath), "UTF-8")

    for (y <- 1 to mapLength) {
      for (x <- 1 to mapLength) {
        val curSq: Square = getSquare(x, y)
        writer.print(curSq.color + " ")
      }
      writer.println()
    }
    writer.println()


    writer.close()


  }
}
