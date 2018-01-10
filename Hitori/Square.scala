package Hitori

class Square(sqX: Int, sqY: Int, sqIdx: Int, sqVal: Int, sqColor: Char) {
  val x: Int = sqX
  val y: Int = sqY
  val idx: Int = sqIdx
  val value: Int = sqVal

  var isVisited_floodFill: Boolean = false
  //flag for pre-filters
  var isFixed: Boolean = false
  //flag for smart brute force
  var smartIsFixed: Boolean = false
  //cell color
  var color: Char = sqColor

  def setColorToGray_guess() {
    if (!isFixed && !smartIsFixed) {
      this.color = 'G'
      this.smartIsFixed = true
    }
  }

  def setColorToBlack_guess() {
    if (!isFixed && !smartIsFixed) {
      this.color = 'B'
      this.smartIsFixed = true
    }
  }

  def setColorToWhite_guess() {
    if (!isFixed && !smartIsFixed) {
      this.color = 'W'
      this.smartIsFixed = true
    }
  }


  def setColor_fix(color: Char) {
    if (!isFixed) {
      this.color = color
      this.isFixed = true
    }

  }

}
