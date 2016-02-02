import scala.collection.mutable.Map

/**
  * Created by Molly on 1/20/2016.
  */
object TestClass {
  def main(args: Array[String]): Unit = {
    val tagger:PhraseTagger = new PhraseTagger()
    tagger.countAndScoreTokens("test.txt", 0.01)
  }

}
