package Results

import java.io._
import com.github.tototoshi.csv._

class Results(file: String) {
  val fileName: String = file

  def writeHeader(language: String, numLearners: Int, numSentences: Int): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(language)
    bw.write(s"$numLearners eChildren")
    bw.write(s"$numSentences sentences")
    bw.write("SP", " ", "HIP", " ", "HCP", " ", "OPT", " ", "NS",
              " ", "NT", " ", "WHM", " ", "PI", " ", "TM", " ",
              "VtoI", " ", "ItoC", " ", "AH", " ", "QInv", " ")
    bw.close()
  }

  def writeResults(grammars: Vector[Vector[Double]]): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
  }
}
