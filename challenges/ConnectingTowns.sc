object Solution {

    def allRoutes(routes: Array[Int]): Int = {
        // avoid overflow by using a property of multiplication in modular arithmetic
        val MOD: Int = 1234567
        routes.foldLeft(1) {(total, route) => (total % MOD) * (route % MOD)} % MOD
    }
    
    def main(args: Array[String]): Unit = {
        import scala.io.StdIn.readLine
        val tests = readLine().toInt
        for (test <- 0 until tests) {
            val towns = readLine().toInt
            val routes: Array[Int] = readLine().split(" ").map(_.toInt)

            println(allRoutes(routes))
        }
    }
}