package native.collections

trait Set extends (String => Boolean) {

  def add(input: String): Set
  def remove(input: String): Set
  def union(that: Set): Set
  def intersection(that: Set): Set
  def difference(that: Set): Set

  //   final def isSubsetOf(that: Set): Boolean = ???

}

object Set {

  private final case class NonEmpty(element: String, otherElements: Set)
      extends Set {

    final override def apply(input: String): Boolean =
      input == element || otherElements(input)

    final override def add(input: String): Set =
      if (input == element) // this check if we are adding a duplicate element
        this
      else
        NonEmpty(input, otherElements.add(element))

    final override def remove(input: String): Set =
      if (input == element)
        otherElements
      else
        NonEmpty(element, otherElements.remove(input))

    final override def union(that: Set): Set = {
      otherElements.union(that.add(element))
    }

    final override def intersection(that: Set): Set = {
      val intersectionsOfOthers = otherElements.intersection(that)
      if (that(element))
        intersectionsOfOthers.add(element)
      else
        intersectionsOfOthers
    }

    final override def difference(that: Set): Set = {
      val differenceOfOthers = otherElements.difference(that)
      if (that(element))
        differenceOfOthers
      else
        differenceOfOthers.add(element)
    }

  }

  private object Empty extends Set {

    final override def apply(input: String): Boolean = false

    final override def add(input: String): Set = NonEmpty(input, Empty)

    final override def remove(input: String): Set = this

    final override def union(that: Set): Set = that

    final override def intersection(that: Set): Set = this

    final override def difference(that: Set): Set = this
  }

  val empty: Set = Empty

}
