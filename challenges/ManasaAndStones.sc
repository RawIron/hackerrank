object Solution {
    def main(args: Array[String]) {
        val numberOfTests: Int = readLine().toInt      
        for (test <- 0 until numberOfTests) {
            val stones: Int = readLine().toInt
            val a: Int = readLine().toInt
            val b: Int = readLine().toInt
                
            var sums: Set[Int] = Set.empty
            for (i <- 0 to stones-1) {
                sums += (i * a) + (stones - 1 - i) * b
            }
            
            sums.toList.sorted.foreach(x => print(x + " "))
            println("")
            }
        }
}