package it.unibo.pps.ex2

import it.unibo.pps.ex2

enum Question:
  case RELEVANCE
  case SIGNIFICANCE
  case CONFIDENCE
  case FINAL

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance:Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews: Map[Int, List[Map[Question, Int]]] = Map.empty

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    this.reviews = this.reviews + (article -> (scores :: this.reviews.getOrElse(article, List.empty)))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = {
    loadReview(article, Map(
      Question.RELEVANCE -> relevance,
      Question.SIGNIFICANCE -> significance,
      Question.CONFIDENCE -> confidence,
      Question.FINAL -> fin
    ))
  }

  override def orderedScores(article: Int, question: Question): List[Int] =
    this.reviews.getOrElse(article, List.empty).map(review => review(question)).sorted()

  override def averageFinalScore(article: Int): Double =
    val finalScores = orderedScores(article, Question.FINAL)
    if finalScores.isEmpty then 0.0 else finalScores.sum.toDouble / finalScores.size

  override def acceptedArticles(): Set[Int] = ???

  override def sortedAcceptedArticles(): List[(Int, Double)] = ???

  override def averageWeightedFinalScoreMap(): Map[Int, Double] = ???
