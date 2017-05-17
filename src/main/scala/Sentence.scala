package Sentence

class Sentence(sentenceInfo: Vector[String]) {
  var language:String = _
  var inflection:String = _
  var sentenceStr:String = _
  var sentenceVec:Vector[String] = _

  // Checks if something other than subject has been  topicalized ie. moved
  // out of canonical argument order. Not checking for presence of Adv
  // topicalized, maybe add later
  // (this is sufficient but Adv could be informative for longitudinal study)
  def outOblique(): Option[Boolean] = {
    val O1index = sentenceVec.indexWhere(_.contains("O1"))
    val O2index = sentenceVec.indexWhere(_.contains("O2"))
    val Pindex = sentenceVec.indexWhere(_.contains("P"))
    val O3index = sentenceVec.indexWhere(_.contains("O3"))

    if (O1index != -1 && O1index < O2index && O2index < Pindex && O3index == Pindex+1) {
      Some(false)
    }
    else if (O3index != -1 && O3index < O2index && O2index < O1index && Pindex == O3index+1) {
      Some(false)
    }
    else if (O1index != -1 && O2index != -1 && Pindex != -1 && O3index != -1) {
      Some(true)
    }
    // Should never reach here
    else { None }
  }
}

object Sentence {
    def apply(sentenceInfo: Vector[String]): Sentence = {
        var s = new Sentence
        s.language = sentenceInfo.head
        s.inflection = sentenceInfo(1)
        s.sentenceStr = sentenceInfo(2)
        s.sentenceVec = sentenceInfo(2).split(" ").toVector
        s
    }
}
