class Sentence(sentenceInfo: List[String]) {
  val language:String = sentenceInfo[0]
  val inflection:String = sentenceInfo[1]
  val sentenceStr:String = sentenceInfo[2]
  val sentenceList:List[String] = sentenceInfo[2].split()

  // Checks if something other than subject has been  topicalized ie. moved
  // out of canonical argument order. Not checking for presence of Adv
  // topicalized, maybe add later
  // (this is sufficient but Adv could be informative for longitudinal study)
  def outOblique(): Boolean = {
    O1index = sentenceList.indexOf("O1")
    O2index = sentenceList.indexOf("O2")
    Pindex = sentenceList.indexOf("P")
    O3index = sentenceList.indexOf("O3")

    if (O1index != -1 && O1index < O2index < Pindex && O3index == Pindex+1) {
      return false
    }
    else if (O3index != -1 && O3index < O2index < O1index && Pindex == O3index+1) {
      return false
    }
    else if (O1index != -1 && O2index != -1 && Pindex != -1 && O3index != -1) {
      return true
    }
  }
}
