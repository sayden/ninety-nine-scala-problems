
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

  //08 Eliminate consecutive duplicates of list elements.
  def compress[X](xs: List[X]): List[X] = xs match {
    case Nil => Nil
    case x :: tail => List(x) ::: compress(tail.dropWhile(_ == x))
  }

  //09 Pack consecutive duplicates of list elements into sublists.
  def pack[X](xs: List[X]): List[List[X]] = xs match {
    case List() => List()
    case x :: tail => List(xs.takeWhile(_ == x)) ::: pack(xs.dropWhile(_ == x))
  }

  //10 Run-length encoding of a list.
  def encode[X](xs: List[X]): List[(X, Int)] = xs match {
    case Nil => Nil
    case xs => this.pack(xs).map(x => (x.head, x.length))
  }

  //11 Modified run-length encoding.
  def modifiedEncode[X](xs: List[X]): List[Any] = {
    this.encode(xs).map(x => if (x._2 == 1) x._1 else x)
  }

  //12 Decode a run-length encoded list.
  def repeathNth[A](a: A, n:Int): List[A] = (a, n) match {
    case (a, 0) => List()
    case (_, n) => List(a) ::: repeathNth(a, n-1)
  }
  def decode[A](xs: List[(A, Int)]): List[A] = {
    xs.flatMap(x => this.repeathNth(x._1, x._2))
  }

  //13 Run-length encoding of a list (direct solution).
  def encodeOptimized[X](xs: List[X]): List[(X, Int)] = xs match {
    case Nil => Nil
    case _ => {
      val (a, b) = xs.span { _ == xs.head }
      List((a.head, a.length)) ::: encodeOptimized(b)
    }
  }

  //14 Duplicate the elements of a list.
  def duplicate[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case x :: tail => List(x,x) ::: duplicate(tail)
  }
}