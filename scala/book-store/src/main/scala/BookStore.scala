object BookStore {

  def total(quantityOfBooks: List[Int]): Int =
    if (quantityOfBooks.isEmpty) 0
    else
      groupBooks(quantityOfBooks)
        .map(_.books
          .map(l => price(l.size))
          .sum).min

  private val bookPrice = 800

  def price(size: Int): Int =
    (size match {
      case 1 => bookPrice
      case 2 => (2 * bookPrice * 0.95)
      case 3 => (3 * bookPrice * 0.90)
      case 4 => (4 * bookPrice * 0.80)
      case 5 => (5 * bookPrice * 0.75)
    }).toInt

  case class Group(books: List[List[Int]])

  def groupBooks(books: List[Int]): List[Group] = {
    def internal(books: List[Int],
                 sizeLimit: Int,
                 acc: List[Int],
                 remaining: List[Int],
                 group: List[List[Int]]): Group = {
      if ((books.isEmpty && remaining.isEmpty))
        Group(group :+ acc)
      else if (acc.size == sizeLimit)
        internal(remaining ++ books,
                 sizeLimit,
                 List.empty,
                 List.empty,
                 group :+ acc)
      else if (books.isEmpty)
        internal(remaining, sizeLimit, List.empty, List.empty, group :+ acc)
      else {
        val head = books.head
        if (!acc.contains(head))
          internal(books.tail, sizeLimit, acc :+ head, remaining, group)
        else
          internal(books.tail, sizeLimit, acc, remaining :+ head, group)
      }
    }

    (1 to 5)
      .map(x => internal(books, x, List.empty, List.empty, List.empty))
      .toList
  }

}
