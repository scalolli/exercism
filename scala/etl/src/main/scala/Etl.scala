object Etl {
  def transform(pointsToAplhabets: Map[Int, Seq[String]]): Map[String, Int] =
    pointsToAplhabets.flatMap {
      case (key, values) =>
        values.map(_.toLowerCase -> key)
    }
}
