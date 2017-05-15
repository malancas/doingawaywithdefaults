import Sentence

class Child(lr: Double, conslr: Double) {
  val learningrate:Double = lr
  val conslearningrate: Double = conslr

  // child is fed a list containing [lang, inflec, sentencestring]
  def consumeSentence(sentence: String):Vector[Vector[Double], Vector[Double]] = {
    spEtrigger(s)    // parameter 1
    hipEtrigger(s)   // parameter 2
    hcpEtrigger(s)   // parameter 3
    // optEtrigger(s) parameter 4
    nsEtrigger(s)    // parameter 5
    ntEtrigger(s)    // parameter 6
    whmEtrigger(s)   // parameter 7
    piEtrigger(s)    // parameter 8
    tmEtrigger(s)    // parameter 9
    VtoIEtrigger(s)  // parameter 10
    // ItoCEtrigger(s) parameter 11
    ahEtrigger(s)    // parameter 12
    // QInvEtrigger(s) parameter 13
  }

  // etriggers for parameters
  // first parameter Subject Position
  def spEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    // Check if O1 and S are in the sentence and sent is declarative
    if (s.sentenceList.contains("01") && s.sentenceList.contains("S") && s.inflection == "DEC"){
      val O1index = s.sentenceList.indexOf("O1")
      // Make sure O1 is non-sentence-initial and before S
      if (O1index > 0 && O1index < s.sentenceList.indexOf("S")){
        // set towards Subject final
        adjustweight(grammar, 0, 1, learningrate)
      }
      // S occurs before 01
      else if (O1index > 0 && O1index > s.sentenceList.indexOf("S")){
        // set towards Subject initial
        adjustweight(grammar, 0, 0, learningrate)
      }
    }
  }

  // second parameter Head IP, VP, PP, etc
  def hipEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.sentenceList.contains("03") && s.sentenceList.contains("P")){
      val O3index = s.sentenceList.index("O3")
      val Pindex = s.sentenceList.index("P")
      // O3 followed by P and not topicalized
      if (O3index > 0 && Pindex == O3index + 1){
        adjustweight (grammar, 1, 1, r)
      }
      else if (O3index > 0 && Pindex == O3index - 1){
        adjustweight (grammar, 1, 0, r)
      }
    }


    // If imperative, make sure Verb directly follows O1
    else if (s.inflection == "IMP" && s.sentenceList.contains("O1") and s.sentenceList.contains("Verb") {
      if (s.sentenceList.indexOf("O1") == s.sentenceList.indexOf("Verb") - 1){
        adjustweight (grammar, 1, 1, r)
      }
      else if (s.sentenceList.indexOf("Verb") == (s.sentenceList.indexOf("O1") - 1)){
        adjustweight(grammar, 1, 0, r)
      }
    }
  }

  def hcpEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.inflection == "Q"){
      val kaIndex = s.sentenceList.indexOf("ka")
      if (s.sentenceList[-1] == "ka" || (kaIndex == -1 && s.sentenceList[-1] == "Aux")){
        adjustweight(grammar, 2, 1, r)
      }
      else if (s.sentenceList[0] == "ka" || (kaIndx == -1 && s.sentenceList[0] == "Aux")){
        adjustweight(grammar, 2, 1, r)
      }
    }
  }

  def nsEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val outObliqueResult = s.outOblique()
    val sInSentence = s.sentenceStr.contains("S")
    if (s.inflection == "DEC" && (!sInSentence && outObliqueResult)){
      adjustweight(grammar, 4, 1, r)
    }
    else if (s.inflection == "DEC" && (sInSentence && outObliqueResult)){
      adjustweight(grammar, 4, 0, conservativerate)
    }
  }

  def ntEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val O2inSentence = s.sentenceStr.contains("O2")
    val O1inSentence = s.sentenceStr.contains("O1")

    if (s.inflection == "DEC" && (O2inSentence && !O1inSentence)){
      adjustweight(grammar, 5, 1, self.r)
    }
    else if (s.inflection == "DEC" && (O2inSentence && O1inSentence) &&
      s.sentenceStr.contains("O3") && s.sentenceStr.contains("S") &&
      s.sentenceStr.contains("Adv")){
        adjustweight(grammar, 5, 0, conservativerate)
      }
  }

  def whmEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.inflection == "Q" && s.sentenceStr.contains("+WH")){
      if (s.sentenceList[0].contains("+WH") ||
        (s.sentenceList[0] == "P" && s.sentenceList[1] == "O3[+WH]")){
          adjustweight(grammar, 6, 1, conservativerate)
        }
      else {
        adjustweight(grammar, 6, 0, r)
      }
    }
  }

  def piEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val pIndex = s.sentenceList.indexOf("P")
    val O3index = s.sentenceList.indexOf("O3")

    if (pIndex >= 0 && O3index >= 0){
      if (abs(pIndex - O3index) > 1){
        adjustweight(grammar, 7, 1, r)
      }
      else if (pIndex + O3index == 1){
        adjustweight(grammar, 7, 0, r)
      }
    }
  }

  def tmEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.sentenceStr.contains("[+WA]")){
      adjustweight(grammar, 8, 1, r)
    }
    else {
      val O1index = s.sentenceList.indexOf("O1")
      val O2index - s.sentenceList.indexOf("O2")

      if (O1index >= 0 && O2index >= 0 && (abs(O1index - O2index) > 1)) {
        adjustweight(grammar, 8, 0, r)
      }
    }
  }

  def VtoIEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val verbIndex = s.sentenceList.indexOf("O1")
    val O1index = s.sentenceList.indexOf("O2")

    if (verbIndex >= 0 && O1index >= 0){
      if (O1index != 0 && abs(verbIndex - O1index) > 1){
        adjustweight(adjustweight(grammar, 9, 1, r), 11, 0, r)
      }
    }
    else if (s.sentenceList.contains("Aux")){
      adjustweight(grammar, 9, 0, conservativerate)
    }
  }

  def ItoCEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val sp = self.grammar[0]
    val hip = self.grammar[1]
    val hcp = self.grammar[2]

    if (sp < 0.5 && hip < 0.5) {
      val sIndex = s.sentenceList.indexOf("S")
      if ((sIndex > 0 && s.inflection == "DEC") && s.sentenceList[sIndex + 1] == "Aux"){
          adjustweight(grammar, 10, 0, r)
      }
    }
    else if (sp > 0.5 && hip > 0.5){
      if (s.inflection == "DEC"){
        val auxIndex = s.sentenceList.indexOf("Aux")
        if (auxIndex > 0 && s.sentenceList[auxIndex + 1] == "S"){
          adjustweight(grammar, 10, 0, r)
        }
      }
    }
    else if (sp > 0.5 && hip < 0.5 && hcp > 0.5 && s.inflection == "DEC"){
      if (s.sentenceList.indexOf("Verb") == s.sentenceList.indexOf("Aux") + 1){
        adjustweight(grammar, 10, 0, r)
      }
    }
    else if (sp < 0.5 && hip > 0.5 && hcp < 0.5 && s.inflection == "DEC"){
      if (s.sentenceList.indexOf("Aux") == s.sentenceList.indexOf("Verb") + 1){
        adjustweight(grammar, 10, 0, r)
      }
    }
    else if (sp > 0.5 && hip < 0.5 && hcp < 0.5 && s.sentenceStr.contains("ka")){
      if (s.inflection == "DEC" && !s.sentenceStr.contains("Aux")){
        if (s.sentenceList.indexOf("Verb") == s.sentenceList.indexOf("Never") + 1){
          adjustweight(grammar, 10, 0, r)
        }
      }
    }
    else if (sp < 0.5 && hip > 0.5 && hcp > 0.5 and s.sentenceStr.contains("ka")){
      if (s.inflection == "DEC" && !s.sentenceStr.contains("Aux")){
        if (s.sentenceList.indexOf("Never") == s.sentenceList.indexOf("Verb") + 1){
          adjustweight(grammar, 10, 0, r)
        }
      }
    }
  }

  def adjustweight(grammar: Vector[Double], paramIndex:String, direction:Int, rate:Double): Vector[Double] = {
    if (direction == 0) {
      val new_value = grammar[paramIndex] - rate * grammar[paramIndex]
      grammar.updated(paramIndex, new_value)
    }
    else { // direction == 1
      val new_value = grammar[paramIndex] + rate * (1 - grammar[paramIndex])
      grammar.updated(paramIndex, new_value)
    }
  }
}
