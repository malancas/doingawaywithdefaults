import Sentence.Sentence

class Child(lr: Double, conslr: Double) {
  val learningrate:Double = lr
  val conservativerate: Double = conslr

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
    if (s.sentenceVec.contains("01") && s.sentenceVec.contains("S") && s.inflection == "DEC"){
      val O1index = s.sentenceVec.indexOf("O1")
      // Make sure O1 is non-sentence-initial and before S
      if (O1index > 0 && O1index < s.sentenceVec.indexOf("S")){
        // set towards Subject final
        adjustweight(grammar, 0, 1, learningrate)
      }
      // S occurs before 01
      else if (O1index > 0 && O1index > s.sentenceVec.indexOf("S")){
        // set towards Subject initial
        adjustweight(grammar, 0, 0, learningrate)
      }
      else { grammar }
    }
    else { grammar }
  }

  // second parameter Head IP, VP, PP, etc
  def hipEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.sentenceVec.contains("03") && s.sentenceVec.contains("P")){
      val O3index = s.sentenceVec.indexOf("O3")
      val Pindex = s.sentenceVec.indexOf("P")
      // O3 followed by P and not topicalized
      if (O3index > 0 && Pindex == O3index + 1){
        adjustweight (grammar, 1, 1, learningrate)
      }
      else if (O3index > 0 && Pindex == O3index - 1){
        adjustweight (grammar, 1, 0, learningrate)
      }
      else { grammar }
    }
    // If imperative, make sure Verb directly follows O1
    else if (s.inflection == "IMP" && s.sentenceVec.contains("O1") && s.sentenceVec.contains("Verb")) {
      if (s.sentenceVec.indexOf("O1") == s.sentenceVec.indexOf("Verb") - 1){
        adjustweight (grammar, 1, 1, learningrate)
      }
      else if (s.sentenceVec.indexOf("Verb") == (s.sentenceVec.indexOf("O1") - 1)){
        adjustweight(grammar, 1, 0, learningrate)
      }
      else { grammar }
    }
    else { grammar }
  }

  def hcpEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.inflection == "Q"){
      val kaIndex = s.sentenceVec.indexOf("ka")
      if (s.sentenceVec.last == "ka" || (kaIndex == -1 && s.sentenceVec.last == "Aux")){
        adjustweight(grammar, 2, 1, learningrate)
      }
      else if (s.sentenceVec(0) == "ka" || (kaIndex == -1 && s.sentenceVec(0) == "Aux")){
        adjustweight(grammar, 2, 1, learningrate)
      }
      else { grammar }
    }
    else { grammar }
  }

  def nsEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val outObliqueResult = s.outOblique()
    if (outObliqueResult == None) {
      println("Sentence is illegal. Invalid out oblique option"); System.exit(1)
    }
    val sInSentence = s.sentenceStr.contains("S")
    if (s.inflection == "DEC" && (!sInSentence && outObliqueResult)){
      adjustweight(grammar, 4, 1, learningrate)
    }
    else if (s.inflection == "DEC" && (sInSentence && outObliqueResult)){
      adjustweight(grammar, 4, 0, conservativerate)
    }
    else { grammar }
  }

  def ntEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val O2inSentence = s.sentenceStr.contains("O2")
    val O1inSentence = s.sentenceStr.contains("O1")

    if (s.inflection == "DEC" && (O2inSentence && !O1inSentence)){
      adjustweight(grammar, 5, 1, learningrate)
    }
    else if (s.inflection == "DEC" && (O2inSentence && O1inSentence) &&
      s.sentenceStr.contains("O3") && s.sentenceStr.contains("S") &&
      s.sentenceStr.contains("Adv")){
        adjustweight(grammar, 5, 0, conservativerate)
      }
    else { grammar }
  }

  def whmEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.inflection == "Q" && s.sentenceStr.contains("+WH")){
      if (s.sentenceVec.head.contains("+WH") ||
        (s.sentenceVec.head == "P" && s.sentenceVec(1) == "O3[+WH]")){
          adjustweight(grammar, 6, 1, conservativerate)
        }
      else {
        adjustweight(grammar, 6, 0, learningrate)
      }
    }
    else { grammar }
  }

  def piEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val pIndex = s.sentenceVec.indexWhere(_.contains("P"))
    val O3index = s.sentenceVec.indexWhere(_.contains("O3"))

    if (pIndex >= 0 && O3index >= 0){
      if (math.abs(pIndex - O3index) > 1){
        adjustweight(grammar, 7, 1, learningrate)
      }
      else if (pIndex + O3index == 1){
        adjustweight(grammar, 7, 0, learningrate)
      }
      else { grammar }
    }
    else { grammar }
  }

  def tmEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.sentenceStr.contains("[+WA]")){
      adjustweight(grammar, 8, 1, learningrate)
    }
    else {
      val O1index = s.sentenceVec.indexOf("O1")
      val O2index = s.sentenceVec.indexOf("O2")

      if (O1index > -1 && O2index > -1 && math.abs(O1index - O2index) > 1) {
        adjustweight(grammar, 8, 0, learningrate)
      }
      else { grammar }
    }
  }

  def VtoIEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val verbIndex = s.sentenceVec.indexWhere(_.contains("Verb"))
    val O1index = s.sentenceVec.indexWhere(_.contains("O1"))

    if (verbIndex >= 0 && O1index >= 0){
      if (O1index != 0 && math.abs(verbIndex - O1index) > 1){
        adjustweight(adjustweight(grammar, 9, 1, learningrate), 11, 0, learningrate)
      }
      else { grammar }
    }
    else if (s.sentenceVec.contains("Aux")){
      adjustweight(grammar, 9, 0, conservativerate)
    }
    else { grammar }
  }

  def ItoCEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    val sp = grammar.head
    val hip = grammar(1)
    val hcp = grammar(2)

    if (sp < 0.5 && hip < 0.5) {
      val sIndex = s.sentenceVec.indexOf("S")
      if ((sIndex > 0 && s.inflection == "DEC") && s.sentenceVec(sIndex + 1) == "Aux"){
          adjustweight(grammar, 10, 0, learningrate)
      }
      else { grammar }
    }
    else if (sp > 0.5 && hip > 0.5){
      if (s.inflection == "DEC"){
        val auxIndex = s.sentenceVec.indexOf("Aux")
        if (auxIndex > 0 && s.sentenceVec(auxIndex + 1) == "S"){
          adjustweight(grammar, 10, 0, learningrate)
        }
        else { grammar }
      }
      else { grammar }
    }
    else if (sp > 0.5 && hip < 0.5 && hcp > 0.5 && s.inflection == "DEC"){
      if (s.sentenceVec.indexOf("Verb") == s.sentenceVec.indexOf("Aux") + 1){
        adjustweight(grammar, 10, 0, learningrate)
      }
      else { grammar }
    }
    else if (sp < 0.5 && hip > 0.5 && hcp < 0.5 && s.inflection == "DEC"){
      if (s.sentenceVec.indexOf("Aux") == s.sentenceVec.indexOf("Verb") + 1){
        adjustweight(grammar, 10, 0, learningrate)
      }
      else { grammar }
    }
    else if (sp > 0.5 && hip < 0.5 && hcp < 0.5 && s.sentenceStr.contains("ka")){
      if (s.inflection == "DEC" && !s.sentenceStr.contains("Aux")){
        if (s.sentenceVec.indexOf("Verb") == s.sentenceVec.indexOf("Never") + 1){
          adjustweight(grammar, 10, 0, learningrate)
        }
        else { grammar }
      }
      else { grammar }
    }
    else if (sp < 0.5 && hip > 0.5 && hcp > 0.5 && s.sentenceStr.contains("ka")){
      if (s.inflection == "DEC" && !s.sentenceStr.contains("Aux")){
        if (s.sentenceVec.indexOf("Never") == s.sentenceVec.indexOf("Verb") + 1){
          adjustweight(grammar, 10, 0, learningrate)
        }
        else { grammar }
      }
      else { grammar }
    }
    else { grammar }
  }

  def ahEtrigger(grammar: Vector[Double], s: Sentence): Vector[Double] = {
    if (s.inflection == "DEC" || s.inflection == "Q") {
      if (!s.sentenceStr.contains("Aux") && s.sentenceStr.contains("Never")){
        if (s.sentenceStr.contains("Verb") && s.sentenceStr.contains("O1")) {
          val neverPos =  s.sentenceVec.indexWhere(_.contains("Never"))
          val verbPos = s.sentenceVec.indexWhere(_.contains("Verb"))
          val O1Pos = s.sentenceVec.indexWhere(_.contains("O1"))

          if ((neverPos > -1 && verbPos == neverPos + 1 && O1Pos == verbPos + 1) ||
            (O1Pos > -1 && verbPos == O1Pos + 1 && neverPos == verbPos + 1)) {
              adjustweight(adjustweight(grammar, 11, 1, learningrate), 9, 0, learningrate)
            }
          else { grammar }
        }
        else { grammar }
      }
      else { grammar }
    }
    else if (s.sentenceStr.contains("Aux") && grammar(11) <= 0.5) {
      adjustweight(grammar, 11, 0, conservativerate)
    }
    else { grammar }
  }

  def adjustweight(grammar: Vector[Double], paramIndex:Int, direction:Int, rate:Double): Vector[Double] = {
    val paramVal = grammar(paramIndex)
    if (direction == 0) {
      val new_value = paramVal - rate * paramVal
      grammar.updated(paramIndex, new_value)
    }
    else { // direction == 1
      val new_value = paramVal + rate * (1 - paramVal)
      grammar.updated(paramIndex, new_value)
    }
  }
}
