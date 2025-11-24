
object DecentNumber {

  def calculate(digits: Int): Tuple2[Int, Int] = {
    var fives: Int = (digits / 3) * 3
    var threes: Int = digits % 3

    while (threes % 5 != 0 && fives > 0) {
      fives -= 3
      threes = digits - fives
    }

    (fives, threes)
  }


  def isValid(threes: Int): Boolean = {
    if (threes % 5 == 0) true
    else false
  }


  def expand(fives: Int, threes: Int): String = {
    if ( isValid(threes) ) {
      val solution: StringBuilder = new StringBuilder()

      for (_ <- 1 to fives) {
        solution.append("5")
      }

      for (_ <- 1 to threes) {
        solution.append("3")
      }

      solution.toString()
    }
    else {
      "-1"
    }
  }


  def test(): Unit = {
    assert( calculate(12) == (12,  0) )
    assert( calculate(13) == ( 3, 10) )
    assert( calculate(14) == ( 9,  5) )
    assert( calculate(15) == (15,  0) )
    assert( calculate(16) == ( 6, 10) )
    assert( calculate(17) == (12,  5) )
  }

}


object Solution {

  /**
   * readLine | toInt | calculate | expand | println
   */
  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine

    val numberOfTests: Int = readLine.toInt

    for (_ <- 0 until numberOfTests) {
      val digits: Int = readLine.toInt

      val (fives, threes) = DecentNumber.calculate(digits)

      val solution = DecentNumber.expand(fives, threes)
      println(solution)
    }
  }

}


/**
 * run in the REPL with `:load SherlockBeast.sc`
Solution.main( Array() )
1
14

DecentNumber.test()
 */
