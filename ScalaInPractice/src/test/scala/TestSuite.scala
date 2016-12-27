import inpractice.Exercise._
import inpractice.{Exercise, ParseTweets, Tweet}
import org.scalatest.{FlatSpec, Matchers}

class TweetTest extends FlatSpec with Matchers {

  val testTweet = Tweet(user = "emrata", text = "this a super hot song. Top!", retweets = 12412)
  val testTweets = List(
    Tweet("boh", "apple apple", 3),
    Tweet("boh", "mario mario", 11),
    Tweet("boh", "mario iphone", 3),
    Tweet("boh", "nexus borgs", 55),
    Tweet("boh", "android  android", 2),
    Tweet("boh", "mela androide", 51)
  )

  "allTweets" should "return 700 tweets" in {
    allTweets().length shouldBe 700
  }

  "word in tweet" should "be able to tell if a tweet contains at least one of the provided words" in {
    wordInTweet(testTweet, List("hot", "Top")) shouldBe true
    wordInTweet(testTweet, List("Top")) shouldBe true
    wordInTweet(testTweet, Nil) shouldBe false
  }

  "filtered tweets" should "return only the tweets that contain one of the provided words" in {
    filteredTweets(testTweets, allWords).length shouldBe 4
    filteredTweets(testTweets, googleWords).length shouldBe 2
    filteredTweets(testTweets, appleWords).length shouldBe 2

    filteredTweets(testTweets, allWords).forall(tweet => wordInTweet(tweet, Exercise.allWords)) shouldBe true
    filteredTweets(testTweets, googleWords).forall(tweet => wordInTweet(tweet, Exercise.googleWords)) shouldBe true
    filteredTweets(testTweets, appleWords).forall(tweet => wordInTweet(tweet, Exercise.appleWords)) shouldBe true
  }

  "retweetsSumFor" should "return the retweets sum for the tweets that contain the given keywords" in {
    retweetsSumFor(testTweets, allWords) shouldBe 63
  }

  "retweetsForApple" should "return retweets sum for the tweets that contain apple keywords" in {
    retweetsForApple(testTweets) shouldBe 6
  }

  "retweetsForGoogle" should "return retweets sum for the tweets that contain apple keywords" in {
    retweetsForGoogle(testTweets) shouldBe 57
  }

  "retweetCountMap" should "return a map with the retweet count for each company" in {
    retweetCountMap().keySet should contain theSameElementsAs ParseTweets.sites
  }

  "winner" should "be Apple" in {
    winner shouldBe "Apple"
  }

}