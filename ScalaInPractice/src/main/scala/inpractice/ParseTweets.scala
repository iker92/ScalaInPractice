package inpractice

object ParseTweets {
  import scala.util.parsing.json._

  private def getList[T](s: String): List[T] =
    JSON.parseFull(s).get.asInstanceOf[List[T]]

  private def getMap(s: String): Map[String, Any] =
    JSON.parseFull(s).get.asInstanceOf[Map[String, Any]]

  private def getTweets(user: String, json: String): List[Tweet] =
    for (map <- getList[Map[String, Any]](json)) yield {
      val text = map("text")
      val retweets = map("retweet_count")
      new Tweet(user, text.toString.toLowerCase, retweets.toString.toDouble.toInt)
    }

  private def getTweetData(user: String, json: String): List[Tweet] = {
    // is list
    val l = getList[Map[String, Any]](json)
    for (map <- l) yield {
      val text = map("text")
      val retweets = map("retweets")
      new Tweet(user, text.toString.toLowerCase, retweets.toString.toDouble.toInt)
    }
  }

    val sites = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")

    private val gizmodoTweets = getTweetData("gizmodo", TweetData.gizmodo)
    private val techCrunchTweets = getTweetData("TechCrunch", TweetData.TechCrunch)
    private val engadgetTweets = getTweetData("engadget", TweetData.engadget)
    private val amazondealsTweets = getTweetData("amazondeals", TweetData.amazondeals)
    private val cnetTweets = getTweetData("CNET", TweetData.CNET)
    private val gadgetlabTweets = getTweetData("gadgetlab", TweetData.gadgetlab)
    private val mashableTweets = getTweetData("mashable", TweetData.mashable)

    private val sources = List(gizmodoTweets, techCrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets, mashableTweets)

    val tweetMap: Map[String, List[Tweet]] =
    Map() ++ Seq((sites(0) -> gizmodoTweets),
    (sites(1) -> techCrunchTweets),
    (sites(2) -> engadgetTweets),
    (sites(3) -> amazondealsTweets),
    (sites(4) -> cnetTweets),
    (sites(5) -> gadgetlabTweets),
    (sites(6) -> mashableTweets))

}
