//import results.Results
import scala.collection.immutable.HashMap
import scala.io.Source
import scala.util.Random
import Sentence.Sentence
import Child.Child

object Simulation {
  val rand = new Random(System.currentTimeMillis())
  val learningrate = 0.002
  val conservativerate = 0.001

  def createLD(language: String, sentencesFileName: String): Vector[Vector[String]] = {
    val languageHash = HashMap("english" -> "611",
      "french" -> "584", "german" -> "2253", "japanese" -> "3856")
    val langNum = languageHash.get(language)

    return Source
            .fromFile("EngFrJapGerm.txt")
            .getLines.map{line => line.split('\t').toVector}
            .filter(l => l(0) == "611")
            .map(l => Sentence(l))
            .toVector
  }

  def makeGrammars(i: Int, numLearners: Int, numSentences: Int, allSentences: Vector[Sentence]): List[Vector[Double]] = {
    if (i == numLearners) { List() }
    else {
      // Get random sentences for Child
      val sentences = Random.shuffle(allSentences).take(numSentences)
      val c = Child(learningrate, conservativerate, sentences)
      c.consumeSentences(0, Vector.fill(12)(0.5)) :+ makeGrammars(i+1, numLearners)
    }
  }

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

      val results = makeGrammars(0, numLearners, numSentences, sentences)
    }

    catch { case e: java.lang.NumberFormatException =>
      println("Arguments one and two must be positive, valid integers"); System.exit(1)}
  }
}
