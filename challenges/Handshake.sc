object Solution {
    // interesting read:
    // https://stackoverflow.com/questions/3102872/what-is-the-fastest-way-to-sum-a-collection-in-scala

    implicit def pimp(n: Int) = new { def handshakes = (0 /: (1 to n)) ( _ + _ ) }
    
    def main(args: Array[String]) {
        import scala.io.StdIn.readLine
        val numberOfTests = readLine().toInt
        for (_ <- 0 until numberOfTests) {
            val members:Int = readLine().toInt

            if (members < 2) println(0)
            else println((members-1).handshakes)
        }
    }
}