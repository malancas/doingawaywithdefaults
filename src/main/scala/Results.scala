import com.github.tototoshi.csv._

class Results(file: String) {
  val fileName: String = file

  def writeHeader(language: String, numLearners: Int, numSentences: Int): Unit = {
    val writer = CSVWriter.open(fileName)
    writer.writeRow(List(language))
    writer.writeRow(List(s"$numLearners eChildren"))
    writer.writeRow(List(s"$numSentences sentences"))
    writer.writeRow(List("SP", " ", "HIP", " ", "HCP", " ", "OPT", " ", "NS",
                          " ", "NT", " ", "WHM", " ", "PI", " ", "TM", " ",
                          "VtoI", " ", "ItoC", " ", "AH", " ", "QInv", " "))
    writer.close()
  }

  def writeResults(grammars: List[String]): Unit = {
    val writer = CSVWriter.open(fileName)
  }
}