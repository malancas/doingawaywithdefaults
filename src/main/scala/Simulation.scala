import Results.Results
import scala.collection.immutable.HashMap
import scala.io.Source
import scala.util.Random
import Sentence.Sentence
import Child.Child

object Simulation {
  val rand = new Random(System.currentTimeMillis())
  val learningrate = 0.002
  val conservativerate = 0.001

  def createLD(language: String): Vector[Sentence] = {
    val languageHash = HashMap("english" -> "611",
      "french" -> "584", "german" -> "2253", "japanese" -> "3856")
    val langNum = languageHash.get(language)

    return Source
            .fromFile("EngFrJapGerm.txt")
            .getLines.map{line => line.split('\t').toVector}
            .filter(l => l(0) == "611")
            .map(l => new Sentence(l))
            .toVector
  }

  def makeGrammars(numLearners: Int, numSentences: Int, sentences: Vector[Sentence]):
  Vector[Vector[Double]] = {
      val c = new Child(learningrate, conservativerate)
      Vector.fill(numLearners)(c.consumeSentences(0, Vector.fill(12)(0.5),
      Random.shuffle(sentences).take(numSentences)))
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

      val sentences = createLD("english")
      //val res = new Results(s"$language-simulation-output.csv")

      println("Starting simulation")

      val results = makeGrammars(numLearners, numSentences, sentences)
    }

    catch { case e: java.lang.NumberFormatException =>
      println("Arguments one and two must be positive, valid integers"); System.exit(1)}
  }
}
