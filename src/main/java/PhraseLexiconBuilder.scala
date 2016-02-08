
import java.io.{File, PrintWriter}
import scala.collection.mutable.Map
import scala.io.Source

/**
  * Created by Molly on 2/8/2016.
  */
object PhraseLexiconBuilder {

  //TODO: add command line interface for running this

  def countAndScoreTokens(filename: String, delta: Double): Map[(String, String), Double] = {
    val corefCounts = Map[(String, String), Int]().withDefaultValue(0)
    val wordCounts = Map[String, Int]().withDefaultValue(0)
    var words: Array[String] = null
    var curPhrase: (String, String) = null

    // count word occurrences, word pair occurrences
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

  // add all phrases with scores above the chosen threshold to the lexicon file
  def buildLexicon(corpus: String, lexFile: String, delta:Double, threshold:Double) = {
    val scores = countAndScoreTokens(corpus, delta)
    //for ((k,v) <- scores) {if(v >= threshold){PhraseLexicon += (k._1 + " " + k._2)}}
    val pw = new PrintWriter(new File(lexFile))
    for ((k,v) <- scores) {
      if(v >= threshold) {
        pw.write(k._1 + " " + k._2 + "\n")
      }
    }
    pw.close()
  }

}
