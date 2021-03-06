
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

  //15 Duplicate the elements of a list a given number of times
  def repeatItem[X](times:Int, x:X): List[X] = (times, x) match {
    case (0, x) => List()
    case (n, x) => List(x) ::: repeatItem(times - 1, x)
  }

  def duplicateN[X](n: Int, xs: List[X]): List[X] = (n, xs) match {
    case (_, Nil) => Nil
    case (0, xs) => xs
    case (n, xs) => xs flatMap { x => this.repeatItem(n, x) }
  }

  //16 Drop every Nth element from a list.
  def drop[X](n:Int, xs: List[X]): List[X] = {
    def innerDrop[X](y: Int, list: List[X]): List[X] = (y, list) match {
      case (_, Nil) => Nil
      case (1, _ :: tail) => innerDrop(n, tail)
      case (_, z :: tail) => z :: innerDrop(y - 1, tail)
    }

    innerDrop(n, xs)
  }

  //17 Split a list into two parts.
  def split[X](n: Int, xs: List[X]): (List[X], List[X]) = (n, xs) match {
    case (_, Nil) => (Nil, Nil)
    case (0, xs) => (Nil, xs)
    case (n, h :: xs) => {
      val (a, z) = split(n - 1, xs)
      (h :: a, z)
    }
  }

  //18 Extract a slice from a list.
  def slice[X](s: Int, e: Int, xs: List[X]): List[X] = {
    val (_, tail) = split(s, xs)
    split(e-s, tail)._1
  }

  // 19 Rotate a list N places to the left.
  def rotate[X](n: Int, xs: List[X]): List[X] = {
    val (head, tail) = split(n, xs)
    tail ::: head
  }

  // 20 Remove the Kth element from a list.
  def removeAt[X](n: Int, xs: List[X]): (List[X], X) = {
    val (head, r :: tail) = split(n, xs)
    (head ::: tail, r)
  }

  // 21 Insert an element at a given position into a list.
  def insertAt[X](element: X, n: Int, xs: List[X]): List[X] = {
    val (head, tail) = split(n, xs)
    head ::: List(element) ::: tail
  }

  // 22 Create a list containing all integers within a given range.
  def range(s: Int, e: Int): List[Int] = {
    def innerRange(i: Int, e: Int): List[Int] = i match {
      case `e` => List(e)
      case _      => List(i) ::: innerRange(i+1, e)
    }

    innerRange(s, e)
  }

  // 23 Extract a given number of randomly selected elements from a list.
  def randomSelect[X](n: Int, xs: List[X]): List[X] = n match {
    case 0 => List()
    case n => {
      val element = this.removeAt((new util.Random).nextInt(xs.length), xs)
      element._2 :: randomSelect(n-1, element._1)
    }
  }

  // 24 Lotto: Draw N different random numbers from the set 1..M.
  def lotto(n: Int, set: Int): List[Int] = n match {
    case 0 => List()
    case n => {
      val range = this.range(1, set)
      val random = (new util.Random).nextInt(range.length)
      val element = this.removeAt(random, range)
      element._2 :: lotto(n - 1, set - 1)
    }
  }

  // 25 Generate a random permutation of the elements of a list.
  def randomPermute[X](xs: List[X]): List[X] = {
    this.randomSelect(xs.length, xs)
  }


}