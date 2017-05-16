//import results.Results
import scala.collection.immutable.HashMap
import scala.io.Source

object Simulation {
  def createLD(language: String, sentencesFileName: String): Vector[Vector[String]] = {
    val languageHash = HashMap("english" -> "611",
      "french" -> "584", "german" -> "2253", "japanese" -> "3856")
    val langNum = languageHash.get(language)

    return Source
            .fromFile("EngFrJapGerm.txt")
            .getLines.map{line => line.split('\t').toVector}
            .filter(l => l(0) == "611")
            .toVector
  }

  val test1 = (i: Int) => {i + 1}

  val test2 = (i: Int) => {i * 4}

  def main(args: Array[String]) {
    try {
      val numLearners = args(0).toInt
      val numSentences = args(1).toInt
      val language = args(2).toLowerCase()

      if (numLearners < 0 || numSentences < 0){
        println("Arguments one and two must be positive integers")
        System.exit(1)
      }

      val sentences = createLD("english", "EngFrJapGerm.txt")
      //val res = new Results(s"$language-simulation-output.csv")

      println("Starting simulation")

      val x = 1
      println((test1 andThen test2)(x))
      //for (i <- 0 to numLearners)
      //  println(i)
    }

    catch { case e: java.lang.NumberFormatException =>
      println("Arguments one and two must be positive, valid integers"); System.exit(1)}
  }
}
