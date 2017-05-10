import scala.collection.immutable.HashMap
import scala.io.Source

object Simulation {
  def createLD(language: String, sentencesFileName: String): Unit = {
    val languageHash = HashMap("english" -> "611",
      "french" -> "584", "german" -> "2253", "japanese" -> "3856")
    val langNum = languageHash.get(language)

    Source
      .fromFile("files/ChargeNames")
      .getLines
      .map { line =>
        //do stuff with line like
        line.split('\t').filter(l => l(0) == langNum)
      }
      .foreach(println)

    //return Source.fromFile(sentencesFileName).getLines().split('\t').toArray
    //return Source.fromFile(sentencesFileName).getLines().split('\t').filter(l => l.0 == langNum)
  }

  def main(args: Array[String]) = {
    try {
      val numLearners = args(0).toInt
      val numSentences = args(1).toInt
      val language = args(2).toLowerCase()

      if (numLearners < 0 || numSentences < 0){
        println("Arguments one and two must be positive integers")
        System.exit(1)
      }

      println(createLD("english", "EngFrJapGerm.txt"))
    }

    catch { case e: java.lang.NumberFormatException =>
      println("Arguments one and two must be positive, valid integers"); System.exit(1)}
  }
}
