object Etl {
  def transform(pointsToAplhabets: Map[Int, Seq[String]]): Map[String, Int] =
    pointsToAplhabets.flatMap {
      case (key, value) =>
        value.map(x => x.toLowerCase -> key)
    }
}
