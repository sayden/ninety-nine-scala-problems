
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

  //04 Find the number of elements of a list.
  def length[X](xs: List[X]): Int = xs match {
    case Nil => 0
    case _ :: tail => length(tail) + 1
  }

  //05 Reverse a list
  def reverse[X](xs: List[X]): List[X] = xs match {
    case Nil => Nil
    case x :: tail => reverse(tail) ::: List(x)
  }

  //06 Find out whether a list is a palindrome.
  def isPalindrome[X](xs: List[X]): Boolean = {
    xs == this.reverse(xs)
  }

  //07 Flatten a nested list structure.
  def flatten(xs: List[Any]): List[Any] = {
    xs flatMap {
      case x:List[_] => flatten(x)
      case x => List(x)
    }
  }
}