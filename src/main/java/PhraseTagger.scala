/**
  * Created by Molly on 1/21/2016.
  */

import cc.factorie.app.nlp.lemma.LowercaseLemmatizer
import cc.factorie.app.nlp.{DocumentAnnotator, Token, Document}
import cc.factorie.app.nlp.lexicon.TriePhraseLexicon
import cc.factorie.app.strings.{nonWhitespaceClassesSegmenter, StringSegmenter}
import cc.factorie.variable.{CategoricalDomain, CategoricalVariable}

import scala.io.Source

class PhraseTagger extends DocumentAnnotator{
  // lexicon to hold phrases
  object PhraseLexicon extends TriePhraseLexicon("Phrases", nonWhitespaceClassesSegmenter, LowercaseLemmatizer)

  // read in phrases from lexicon file and build PhraseLexicon
  def buildLexicon(lexFile: String) = {
    for (line <- Source.fromFile(lexFile, "ISO-8859-1").getLines) {
      PhraseLexicon += line
    }
  }

  def process(document:Document): Document = {
    //TODO: tag the text
    return document
  }

  // for each sentence in the document, tag the tokens with a  PhraseType (CategoricalVariable[String])
  def tagText(text:Document) = {
//      def featureFunction(word:Token, label:String): CategoricalVariable[String] = {
//        return new PhraseTypeTag(word, label+"-Phrase")
//      }
      for (sentence <- text.sentences)
        PhraseLexicon.labelLemmatizedText(sentence.tokens, (t:Token, s:String)=>new PhraseTypeTag(t,s+"-Phrase"))
  }

  /** A categorical variable, associated with a token, holding whether and where it occurs in a phrase.  */
  class PhraseTypeTag(token:Token, initialIndex:Int)
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

  override def prereqAttrs: Iterable[Class[_]] = ???

  override def postAttrs: Iterable[Class[_]] = ???

  override def tokenAnnotationString(token: Token): String = ???
}
