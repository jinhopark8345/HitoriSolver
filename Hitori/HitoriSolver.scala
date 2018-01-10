package Hitori

import java.util
import java.util.Properties

import Hitori.HitoriIsValid._
import Hitori.debugTools._


object HitoriSolver extends App {

  //ex) 5x5 map : mapLength = 5, mapAreaSize = 25
  var mapLength: Int = _
  var mapAreaSize: Int = _

  var Squares: List[Square] = List[Square]()
  var valList: List[Int] = Nil

  def getSquare(x: Int, y: Int): Square = Squares.filter(_.x == x).filter(_.y == y).head

  def getSquare(pos: Pos): Square = Squares.filter(_.x == pos.x).filter(_.y == pos.y).head

  def getSquare(idx: Int): Square = Squares.filter(_.idx == idx).head

  def getSqColor(x: Int, y: Int): Char = getSquare(x, y).color


  def getSqColor(idx: Int): Char = getSquare(idx).color

  def getSqValue(x: Int, y: Int): Int = getSquare(x, y).value

  def getSqValue(idx: Int): Int = getSquare(idx).value

  //  (x,y) => getcolumn(x)
  def getColumn(x: Int): List[Square] = Squares.filter(_.x == x)

  //  (x,y) => getRow(y)
  def getRow(y: Int): List[Square] = Squares.filter(_.y == y)

  def boundaryCheck(x: Int, y: Int): Boolean = {
    if (x > 0 && x <= mapLength && y > 0 && y <= mapLength) {
      return true
    }
    false
  }

  def boundaryCheck(pos: Pos): Boolean = {
    boundaryCheck(pos.x, pos.y)
  }

  def boundaryCheck(idx: Int): Boolean = {
    val x: Int = idx % mapLength + 1
    val y: Int = idx / mapLength + 1
    boundaryCheck(x, y)
  }

  //  def main(args: Array[String]) {

  val t0 = System.nanoTime()

  val inputPath: String = args(0)
  val outputPath: String = args(1)
  val currentDirectory = new java.io.File(".").getCanonicalPath

  println("current directory : "+ currentDirectory)
  printf("inputPath : %s\n", inputPath)
  printf("outputPath : %s\n", outputPath)


  HitoriInit.initSolving(inputPath)

  //apply preFilters
  PreFilters.fixObviousWhite()
  PreFilters.fixSandwichPair()
  PreFilters.fixPairIsolation()

  blackAndWhiteCycle.smartBruteForce()
  HitoriEnd.endSolving(outputPath)

  //to make sure finish after smartBruteforce
  println()


  val t1 = System.nanoTime()
  val totTime_nanosec = t1 - t0
  val totTime_microsec = totTime_nanosec / 1000
  val totTime_millisec = totTime_microsec / 1000
  val totTime_sec = totTime_millisec / 1000

  println("Elapsed time: " + totTime_nanosec + " ns")
  println("Elapsed time: " + totTime_microsec + " ms")
  println("Elapsed time: " + totTime_millisec + " millisec")
  println("Elapsed time: " + totTime_sec + " sec")

}

