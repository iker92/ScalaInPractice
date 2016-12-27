package inpractice


// Implementate le seguenti funzioni per risolvere gli esercizi
object Exercise {

  val appleWords = List("apple", "iphone")
  val googleWords = List("android", "nexus")
  val allWords = appleWords ++ googleWords

  def wordsInTweet(tweet: Tweet) = tweet.text.split(Array(' ', '#', '!', '.')).toList

//  MANDATORY:

//  1) convert the Map obtainable from ParseTweets.tweetMap to an object of type List[Tweet] containing all the available tweets.
  def allTweets(): List[Tweet] = {
    val map = ParseTweets.tweetMap

    val res = map.values.toList.flatten

    return res
  }


//  2) wordInTweets should return true if at least one keyword in the list keywords can be found in the tweet's text.
//     Hint: use methods .contains and .exists
  def wordInTweet(tweet: Tweet, keywords: List[String]): Boolean = {

  val words = tweet.text.split("\\W")
  return (keywords.exists(words.contains))
  }


//  3) produce a filtered list of tweets containing only the ones that contain at least one of the given keywords.
//     Hint: use wordInTweets and .filter
  def filteredTweets(tweets: List[Tweet], keywords: List[String]): List[Tweet] = {

  val list = tweets.filter(tweet => wordInTweet(tweet,keywords))
  return list
  }

//  4) compute the sum of the retweets for all the tweets that contain one of the given keywords
  def retweetsSumFor(tweets: List[Tweet], keywords: List[String]): Int = {
    val list = filteredTweets(tweets,keywords)
    val sum =list.foldRight(0)((a,b) =>a.retweets + b)
    return sum
  }

//  EXTRA:
//  5) compute the total number of retweets for the Apple related tweets. use retweetSumFor and appleWords
  def retweetsForApple(tweets: List[Tweet]): Int = {
    return retweetsSumFor(tweets,appleWords)
  }


//  6) compute the total number of retweets for the Google related tweets. use retweetSumFor and googleWords
  def retweetsForGoogle(tweets: List[Tweet]): Int = {
    return retweetsSumFor(tweets,googleWords)
  }

//  SUPER:
//
//  7) convert tweetMap into a Ma[String, (Int, Int)] containing as keys the website names (As the original one),
//  and as values the sum of retweets for apple related tweets and google related tweets.
  def retweetCountMap(): Map[String, (Int, Int)] = {
    val map = ParseTweets.tweetMap map {
      case (site,tweet) => (site,(retweetsForApple(tweet),retweetsForGoogle(tweet)) )
    }
    return map
  }

//  8) use retweetCountMap to determine which company has had the largest number of retweets on most websites.
//  keep in mind that is not important the total number of retweets on all websites, you should consider on how many
//  of them the company related tweets have been the most retweeted. Hint: use .mapValues
  def winner(): String = {
    val map =retweetCountMap()
  val compareMap = map.mapValues({case (a,b) => if (a > b)"Apple" else "Google"})
  val resApple =compareMap.count( _._2 == "Apple")
  val resGoogle=compareMap.count( _._2 == "Google")
  if (resApple > resGoogle)
    return "Apple" else if (resGoogle > resApple ) return "Google" else "Equal"
  }

}
