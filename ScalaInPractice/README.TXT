Dear Scala Enthusiast,

this is the evaluation project to obtain the 1CFU for the seminar Scala in Practice.

This is a simple project that contains some fake Twitter data that we would like to process.
Your target is to implement the functions defined in file Exercise.scala.
You should remove the ??? (that throws a NotImplementedError) and substitute it with the function implementation.
You must not modify the function signature to not invalidate the automatic tests used to evaluate your work.

Please open the terminal / shell.
Navigate your file system to reach the ScalaInPractice folder.
Run sbt
Type compile and press enter to compile
Type test to run the tests.

Please be aware that this project includes only part of the tests. Another set of them is used to the final evaluation.
By the way if the tests available to you do not pass, the solution is to be considered wrong and needs corrected.

Follows the description of the exercises.

MANDATORY:
1) convert the Map obtainable from ParseTweets.tweetMap to an object of type List[Tweet] containing all the available tweets.
2) wordInTweets should return true if at least one keyword in the list keywords can be found in the tweet's text. Hint: use methods .contains and .exists
3) produce a filtered list of tweets containing only the ones that contain at least one of the given keywords. Hint: use wordInTweets and .filter
4) compute the sum of the retweets for all the tweets that contain one of the given keywords

EXTRA:
5) compute the total number of retweets for the Apple related tweets. use retweetSumFor and appleWords
6) compute the total number of retweets for the Google related tweets. use retweetSumFor and googleWords

SUPER:

7) convert tweetMap into a Ma[String, (Int, Int)] containing as keys the website names (As the original one),
and as values the sum of retweets for apple related tweets and google related tweets.

8) use retweetCountMap to determine which company has had the largest number of retweets on most websites.
keep in mind that is not important the total number of retweets on all websites, you should consider on how many
of them the company related tweets have been the most retweeted
