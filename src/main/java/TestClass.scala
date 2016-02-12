
/**
  * Created by Molly on 1/20/2016.
  */

import cc.factorie.app.nlp.Document

object TestClass {
  def main(args: Array[String]): Unit = {
    PhraseLexiconBuilder.buildLexicon("test.txt","phrases.txt", 0.1, 0.5)
    val lines = scala.io.Source.fromFile("test.txt").mkString
    val testDoc = new Document(lines)
    PhraseTagger.process(testDoc)
  }

}
