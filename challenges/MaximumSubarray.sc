object Solution {

  def loop(population: Array[Int]): (Int, Int) = {
    var sum: Int = 0
    var maxSumSlice: Int = Int.MinValue
    var maxSumSubset: Int = 0

    for (i <- 0 until population.length) {
      sum += population(i)

      if (population(i) > 0) {
        maxSumSubset = maxSumSubset + population(i)
      }

      if (sum > maxSumSlice) {
        maxSumSlice = sum
      }

      if (sum < 0) {
        sum = 0
      }
    }

    if (maxSumSubset == 0) {
      // all numbers <= 0
      maxSumSubset = maxSumSlice
    }
    
    (maxSumSlice, maxSumSubset)
  }


  def test(): Unit = {
    val population: Array[Int] = "2 45 -50 23 -4 12 -65 343 14 -11 -4".split(" ").map(_.toInt)
    val expected: (Int, Int) = (357, 439)
    val have: (Int, Int) = loop(population)
    assert(have._1 == expected._1)
    assert(have._2 == expected._2)
  }


  def solve(): Unit = {
    import scala.io.StdIn.readLine
    val numberOfTests: Int = readLine().toInt
    for (i <- 1 to numberOfTests) {
        val populationLength: Int = readLine().toInt
        val population: Array[Int] = readLine().split(" ").map(_.toInt)

        val (slice, subset) = loop(population)
        println(s"${slice} ${subset}")
    }
  }


  def main(args: Array[String]): Unit = {
    //test()
    solve()
  }
}