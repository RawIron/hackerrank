object Solution {

  def buyBars(cash: Int, pricePerBar: Int, wrappersPerBar: Int): Int = {
    var bars: Int = cash / pricePerBar
    var wrappers: Int = bars
    
    while (wrappers >= wrappersPerBar) {
      val exchanged: Int = wrappers / wrappersPerBar
      wrappers = (wrappers % wrappersPerBar) + exchanged
      bars += exchanged
    }

    bars
  }

  def main(args: Array[String]) {
    import scala.io.StdIn.readLine

    val numberOfTests: Int = readLine().toInt
  
    for (_ <- 0 until numberOfTests) {
      val test: Array[Int] = readLine().split(" ").map(_.toInt)
      val (cash, price, wrappers) = (test(0), test(1), test(2))

      println( buyBars(cash, price, wrappers) )
    }
  }

}