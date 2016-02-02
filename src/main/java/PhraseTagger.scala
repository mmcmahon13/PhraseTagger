/**
  * Created by Molly on 1/21/2016.
  */

import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.nlp.lemma.{LowercaseLemmatizer, Lemmatizer}
import cc.factorie.app.nlp.lexicon.TriePhraseLexicon
import cc.factorie.app.strings.{nonWhitespaceClassesSegmenter, StringSegmenter}
import cc.factorie.variable.{CategoricalDomain, CategoricalVariable}

import scala.collection.mutable.Map
import scala.io.Source

class PhraseTagger {

  def countAndScoreTokens(filename: String, delta: Double): Map[(String, String), Double] = {
    val corefCounts = Map[(String, String), Int]().withDefaultValue(0)
    val wordCounts = Map[String, Int]().withDefaultValue(0)
    var words: Array[String] = null
    var curPhrase: (String, String) = null


    for (line <- Source.fromFile(filename, "ISO-8859-1").getLines) {
      words = line.split(' ')
      for (i <- 0 until words.length - 1) {
        wordCounts.update(words(i), wordCounts(words(i)) + 1)
        curPhrase = (words(i), words(i + 1))
        corefCounts.update(curPhrase, corefCounts(curPhrase) + 1)
      }
    }

    var word1:String = null
    var word2:String = null
    var score:Double = 0

    val scores = Map[(String, String), Double]().withDefaultValue(0)

    for ((wordTup, count) <- corefCounts){
      word1 = wordTup._1
      word2 = wordTup._2
      score = (count - delta)/(wordCounts(word1)*wordCounts(word2))
      scores(wordTup) = score

    }

    for ((k,v) <- scores) printf("key: %s, value: %s\n", k, v)

    //    for ((k,v) <- corefCounts) printf("key: %s, value: %s\n", k, v)
    //    for ((k,v) <- wordCounts) printf("key: %s, value: %s\n", k, v)

    return scores
  }

  // add all phrases with scores above the chosen threshold to the lexicon
  def buildLexicon(scores:Map[(String, String), Double], threshold:Double) = {
    for ((k,v) <- scores) {if(v >= threshold){PhraseLexicon += (k._1 + " " + k._2)}}
  }

  // for each sentence in the document, tag the tokens with a  PhraseType (CategoricalVariable[String])
  def tagText(text:Document) = {
    for (sentence <- text.sentences)
      PhraseLexicon.tagLemmatizedText(sentence.tokens,)
  }

  /** A categorical variable, associated with a token, holding whether and where it occurs in a phrase.  */
  class phraseTypeTag(token:Token, initialIndex:Int)
    extends CategoricalVariable[String](initialIndex) {
    def this(token:Token, initialCategory:String) = this(token, PhraseTagDomain.index(initialCategory))
    final def domain = PhraseTagDomain
    def isBeginningPhrase = PhraseTagDomain.isBeginningPhrase(categoryValue)
    def isInPhrase = PhraseTagDomain.isInPhrase(categoryValue)
    def isEndPhrase = PhraseTagDomain.isEndPhrase(categoryValue)
    def isNotPhrase = PhraseTagDomain.isNotPhrase(categoryValue)
    def isUnitPhrase = PhraseTagDomain.isUnitPhrase(categoryValue)
  }

  object PhraseTagDomain extends CategoricalDomain[String] {
    this ++= Vector(
      "B-Phrase",
      "I-Phrase",
      "L-Phrase",
      "O-Phrase",
      "U-Phrase"
    )

    def isBeginningPhrase(tag:String): Boolean = {tag=="B-Phrase"}
    def isInPhrase(tag:String): Boolean = {tag=="I-Phrase"}
    def isEndPhrase(tag:String): Boolean = {tag=="L-Phrase"}
    def isNotPhrase(tag:String): Boolean = {tag=="O-Phrase"}
    def isUnitPhrase(tag:String): Boolean = {tag=="U-Phrase"}

  }

  // lexicon to hold phrases
  object PhraseLexicon extends TriePhraseLexicon("Phrases", nonWhitespaceClassesSegmenter, LowercaseLemmatizer)

}
