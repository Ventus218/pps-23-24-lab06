package ex2

import scala.collection.immutable.Map
import ex1.List.apply

trait ConferenceReviewing:
	import ConferenceReviewing.Question
	
	/**
	 * @param article
	 * @param scores
	 * loads a review for the specified article, with complete scores as a map
	 */
	def loadReview(article: Int, scores: Map[Question, Int]): Unit

	/**
	 * @param article
	 * @param relevance
	 * @param significance
	 * @param confidence
	 * @param fin
	 * loads a review for the specified article, with the 4 explicit scores
	 */
	def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

	/**
	 * @param article
	 * @param question
	 * @return the scores given to the specified article and specified question, as an (ascending-ordered) list 
	 */
	def orderedScores(article: Int, question: Question): List[Int]

	/**
	 * @param article
	 * @return the average score to question FINAL taken by the specified article
	 */
	def averageFinalScore(article: Int): Double

	/**
	 * An article is considered accept if its averageFinalScore (not weighted) is > 5, 
	 * and at least one RELEVANCE score that is >= 8.
	 * @return the set of accepted articles
	 */
	def acceptedArticles(): Set[Int]


	/**
	 * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
	 */
	def sortedAcceptedArticles(): List[(Int, Double)]

	/**
	 * @return a map from articles to their average "weighted final score", namely,
	 * the average value of CONFIDENCE*FINAL/10  
	 * Note: this method is optional in this exam
	 */
	def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
	/**
	 * For each article, the reviewer has to reply to all the following questions
	 */
	enum Question:
		case RELEVANCE      // ("È importante per questa conferenza?"),
		case SIGNIFICANCE 	// ("Produce contributo scientifico?"),
		case CONFIDENCE   	// ("Ti senti competente a commentarlo?");
		case FINAL        	// ("É un articolo da accettare?")

	def apply(): ConferenceReviewing = ConferenceReviewingImpl()

	private class ConferenceReviewingImpl extends ConferenceReviewing:
		private case class Review(val article: Int, val relevance: Int, val significance: Int, val confidence: Int, val `final`: Int):
			def scoreFor(question: Question): Int = question match
				case Question.RELEVANCE => relevance
				case Question.SIGNIFICANCE => significance
				case Question.CONFIDENCE => confidence
				case Question.FINAL => `final`

		private var reviews = Map[Int, List[Review]]()

		override def averageWeightedFinalScoreMap(): Map[Int, Double] = ???

		override def sortedAcceptedArticles(): List[(Int, Double)] =
			acceptedArticlesWithAverageFinalScore().toList.sortWith((before, after) => before._2 < after._2)

		override def acceptedArticles(): Set[Int] =
			acceptedArticlesWithAverageFinalScore().map((article, _) => article).toSet

		private def acceptedArticlesWithAverageFinalScore(): Iterable[(Int, Double)] =
			reviews.map((article, articleReviews) => (article, articleReviews, averageFinalScore(article)))
				.filter((article, articleReviews, score) => score > 5 && articleReviews.exists(_.relevance >= 8))
				.map((article, articleReviews, score) => (article, score))

		override def orderedScores(article: Int, question: Question): List[Int] =
			assert(reviews.contains(article))
			reviews.getOrElse(article, List()).map(_.scoreFor(question)).sorted(Ordering.Int)

		override def averageFinalScore(article: Int): Double =
			assert(reviews.contains(article))
			val articleReviews = reviews.getOrElse(article, List())
			articleReviews.map(_.scoreFor(Question.FINAL)).sum / articleReviews.length.toDouble

		override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, `final`: Int): Unit =
			val articleReviews = reviews.getOrElse(article, List())
			reviews = reviews + (article -> articleReviews.appended(Review(article, relevance = relevance, significance = significance, confidence = confidence, `final` = `final`)))

		override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
			val relevance = scores.get(Question.RELEVANCE)
			val significance = scores.get(Question.SIGNIFICANCE)
			val confidence = scores.get(Question.CONFIDENCE)
			val `final` = scores.get(Question.FINAL)
			assert(relevance.isDefined || significance.isDefined || confidence.isDefined || `final`.isDefined)
			loadReview(article, relevance = relevance.get, significance = significance.get, confidence = confidence.get, `final` = `final`.get)
