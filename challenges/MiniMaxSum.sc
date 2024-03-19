object Solution {
    def main(args: Array[String]) {
        val numbers: Seq[Long] = scala.io.StdIn.readLine().split(" ").map(_.toLong).sorted
        
        val mini = numbers.take(4).sum
        val maxi = numbers.takeRight(4).sum
        
        println(f"$mini%d $maxi%d")
    }
}
