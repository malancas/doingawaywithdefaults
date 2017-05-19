package Child

import Sentence.Sentence

class Child(lr: Double, conslr: Double) {
  var learningrate:Double = _
  var conservativerate:Double = _

  // child is fed a list containing [lang, inflec, sentencestring]
  def consumeSentences(i: Int, grammar: Vector[Double],
    sentences: Vector[Sentence]): Vector[Double] = {
    if (i == sentences.length) { grammar }
    else {
      val newGrammar = updateGrammar(grammar, sentences(i))
      consumeSentences(i+1, newGrammar, sentences)
    }
  }

  def updateGrammar(currGrammar: Vector[Double], s: Sentence): Vector[Double] = {
    (spEtrigger.tupled andThen hipEtrigger.tupled andThen hcpEtrigger.tupled andThen
      nsEtrigger.tupled andThen ntEtrigger.tupled andThen whmEtrigger.tupled andThen
      piEtrigger.tupled andThen tmEtrigger.tupled andThen VtoIEtrigger.tupled andThen
      ahEtrigger.tupled)(currGrammar, s)
  }

  // etriggers for parameters
  // first parameter Subject Position
  val spEtrigger = (grammar: Vector[Double], s: Sentence) => {
    // Check if O1 and S are in the sentence and sent is declarative
    if (s.sentenceVec.contains("01") && s.sentenceVec.contains("S") && s.inflection == "DEC"){
      val O1index = s.sentenceVec.indexOf("O1")
      // Make sure O1 is non-sentence-initial and before S
      if (O1index > 0 && O1index < s.sentenceVec.indexOf("S")){
        // set towards Subject final
        (adjustweight(grammar, 0, 1, learningrate), s)
      }
      // S occurs before 01
      else if (O1index > 0 && O1index > s.sentenceVec.indexOf("S")){
        // set towards Subject initial
        (adjustweight(grammar, 0, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
    else { (grammar, s) }
  }

  // second parameter Head IP, VP, PP, etc
  val hipEtrigger = (grammar: Vector[Double], s: Sentence) => {
    if (s.sentenceVec.contains("03") && s.sentenceVec.contains("P")){
      val O3index = s.sentenceVec.indexOf("O3")
      val Pindex = s.sentenceVec.indexOf("P")
      // O3 followed by P and not topicalized
      if (O3index > 0 && Pindex == O3index + 1){
        (adjustweight(grammar, 1, 1, learningrate), s)
      }
      else if (O3index > 0 && Pindex == O3index - 1){
        (adjustweight(grammar, 1, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
    // If imperative, make sure Verb directly follows O1
    else if (s.inflection == "IMP" && s.sentenceVec.contains("O1") &&
    s.sentenceVec.contains("Verb")) {
      if (s.sentenceVec.indexOf("O1") == s.sentenceVec.indexOf("Verb") - 1){
        (adjustweight(grammar, 1, 1, learningrate), s)
      }
      else if (s.sentenceVec.indexOf("Verb") == (s.sentenceVec.indexOf("O1") - 1)){
        (adjustweight(grammar, 1, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
    else { (grammar, s) }
  }

  val hcpEtrigger = (grammar: Vector[Double], s: Sentence) => {
    if (s.inflection == "Q"){
      val kaIndex = s.sentenceVec.indexOf("ka")
      if (s.sentenceVec.last == "ka" || (kaIndex == -1 && s.sentenceVec.last == "Aux")){
        (adjustweight(grammar, 2, 1, learningrate), s)
      }
      else if (s.sentenceVec(0) == "ka" || (kaIndex == -1 && s.sentenceVec(0) == "Aux")){
        (adjustweight(grammar, 2, 1, learningrate), s)
      }
      else { (grammar, s) }
    }
    else { (grammar, s) }
  }

  val nsEtrigger = (grammar: Vector[Double], s: Sentence) => {
    val outObliqueResult = s.outOblique()
    if (outObliqueResult.isEmpty) {
      println("Illegal sentence structure"); System.exit(1)
    }
    val sInSentence = s.sentenceStr.contains("S")

    if (s.inflection == "DEC" && (!sInSentence && outObliqueResult.getOrElse(true))){
      (adjustweight(grammar, 4, 1, learningrate), s)
    }
    else if (s.inflection == "DEC" && (sInSentence && outObliqueResult.getOrElse(true))){
      (adjustweight(grammar, 4, 0, conservativerate), s)
    }
    else { (grammar, s) }
  }

  val ntEtrigger = (grammar: Vector[Double], s: Sentence) => {
    val O2inSentence = s.sentenceStr.contains("O2")
    val O1inSentence = s.sentenceStr.contains("O1")

    if (s.inflection == "DEC" && (O2inSentence && !O1inSentence)){
      (adjustweight(grammar, 5, 1, learningrate), s)
    }
    else if (s.inflection == "DEC" && (O2inSentence && O1inSentence) &&
      s.sentenceStr.contains("O3") && s.sentenceStr.contains("S") &&
      s.sentenceStr.contains("Adv")){
        (adjustweight(grammar, 5, 0, conservativerate), s)
      }
    else { (grammar, s) }
  }

  val whmEtrigger = (grammar: Vector[Double], s: Sentence) => {
    if (s.inflection == "Q" && s.sentenceStr.contains("+WH")){
      if (s.sentenceVec.head.contains("+WH") ||
        (s.sentenceVec.head == "P" && s.sentenceVec(1) == "O3[+WH]")){
          (adjustweight(grammar, 6, 1, conservativerate), s)
        }
      else {
        (adjustweight(grammar, 6, 0, learningrate), s)
      }
    }
    else { (grammar, s) }
  }

  val piEtrigger = (grammar: Vector[Double], s: Sentence) => {
    val pIndex = s.sentenceVec.indexWhere(_.contains("P"))
    val O3index = s.sentenceVec.indexWhere(_.contains("O3"))

    if (pIndex >= 0 && O3index >= 0){
      if (math.abs(pIndex - O3index) > 1){
        (adjustweight(grammar, 7, 1, learningrate), s)
      }
      else if (pIndex + O3index == 1){
        (adjustweight(grammar, 7, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
    else { (grammar, s) }
  }

  val tmEtrigger = (grammar: Vector[Double], s: Sentence) => {
    if (s.sentenceStr.contains("[+WA]")){
      (adjustweight(grammar, 8, 1, learningrate), s)
    }
    else {
      val O1index = s.sentenceVec.indexOf("O1")
      val O2index = s.sentenceVec.indexOf("O2")

      if (O1index > -1 && O2index > -1 && math.abs(O1index - O2index) > 1) {
        (adjustweight(grammar, 8, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
  }

  val VtoIEtrigger = (grammar: Vector[Double], s: Sentence) => {
    val verbIndex = s.sentenceVec.indexWhere(_.contains("Verb"))
    val O1index = s.sentenceVec.indexWhere(_.contains("O1"))

    if (verbIndex >= 0 && O1index >= 0){
      if (O1index != 0 && math.abs(verbIndex - O1index) > 1){
        (adjustweight(adjustweight(grammar, 9, 1, learningrate),
         11, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
    else if (s.sentenceVec.contains("Aux")){
      (adjustweight(grammar, 9, 0, conservativerate), s)
    }
    else { (grammar, s) }
  }

  val ItoCEtrigger = (grammar: Vector[Double], s: Sentence) => {
    val sp = grammar.head
    val hip = grammar(1)
    val hcp = grammar(2)

    if (sp < 0.5 && hip < 0.5) {
      val sIndex = s.sentenceVec.indexOf("S")
      if ((sIndex > 0 && s.inflection == "DEC") && s.sentenceVec(sIndex + 1) == "Aux"){
          (adjustweight(grammar, 10, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
    else if (sp > 0.5 && hip > 0.5){
      if (s.inflection == "DEC"){
        val auxIndex = s.sentenceVec.indexOf("Aux")
        if (auxIndex > 0 && s.sentenceVec(auxIndex + 1) == "S"){
          (adjustweight(grammar, 10, 0, learningrate), s)
        }
        else { (grammar, s) }
      }
      else { (grammar, s) }
    }
    else if (sp > 0.5 && hip < 0.5 && hcp > 0.5 && s.inflection == "DEC"){
      if (s.sentenceVec.indexOf("Verb") == s.sentenceVec.indexOf("Aux") + 1){
        (adjustweight(grammar, 10, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
    else if (sp < 0.5 && hip > 0.5 && hcp < 0.5 && s.inflection == "DEC"){
      if (s.sentenceVec.indexOf("Aux") == s.sentenceVec.indexOf("Verb") + 1){
        (adjustweight(grammar, 10, 0, learningrate), s)
      }
      else { (grammar, s) }
    }
    else if (sp > 0.5 && hip < 0.5 && hcp < 0.5 && s.sentenceStr.contains("ka")){
      if (s.inflection == "DEC" && !s.sentenceStr.contains("Aux")){
        if (s.sentenceVec.indexOf("Verb") == s.sentenceVec.indexOf("Never") + 1){
          (adjustweight(grammar, 10, 0, learningrate), s)
        }
        else { (grammar, s) }
      }
      else { (grammar, s) }
    }
    else if (sp < 0.5 && hip > 0.5 && hcp > 0.5 && s.sentenceStr.contains("ka")){
      if (s.inflection == "DEC" && !s.sentenceStr.contains("Aux")){
        if (s.sentenceVec.indexOf("Never") == s.sentenceVec.indexOf("Verb") + 1){
          (adjustweight(grammar, 10, 0, learningrate), s)
        }
        else { (grammar, s) }
      }
      else { (grammar, s) }
    }
    else { (grammar, s) }
  }

  val ahEtrigger = (grammar: Vector[Double], s: Sentence) => {
    if (s.inflection == "DEC" || s.inflection == "Q") {
      if (!s.sentenceStr.contains("Aux") && s.sentenceStr.contains("Never")){
        if (s.sentenceStr.contains("Verb") && s.sentenceStr.contains("O1")) {
          val neverPos =  s.sentenceVec.indexWhere(_.contains("Never"))
          val verbPos = s.sentenceVec.indexWhere(_.contains("Verb"))
          val O1Pos = s.sentenceVec.indexWhere(_.contains("O1"))

          if ((neverPos > -1 && verbPos == neverPos + 1 && O1Pos == verbPos + 1) ||
            (O1Pos > -1 && verbPos == O1Pos + 1 && neverPos == verbPos + 1)) {
              adjustweight(adjustweight(grammar, 11, 1, learningrate),
               9, 0, learningrate)
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

  def adjustweight(grammar: Vector[Double], paramIndex:Int, direction:Int,
    rate:Double): Vector[Double] = {

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
