
object lists {

  //01 Find the last element of a list.
  def getLast[X](xs: List[X]): X = xs match {
    case x :: Nil => x
    case _ :: xs => getLast(xs)
  }

  //02 Find the last but one element of a list.
  def getLastButOne[X](xs: List[X]): X = xs match {
    case x :: _ :: Nil => x
    case _ :: xs => getLastButOne(xs)
  }

  //03 Find the Kth element of a list.
  def nth[X](n: Int, xs: List[X]): X = (n, xs) match {
    case (0, y :: _) => y
    case (n, _ :: tail) => nth(n-1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  
}