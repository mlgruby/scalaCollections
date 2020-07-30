package native.collections

trait OldSet extends (String => Boolean) {

  final def add(input: String): OldSet =
    element => element == input || this(element)

  final def remove(input: String): OldSet =
    element => element != input && this(input)

  final def union(that: OldSet): OldSet =
    element => this(element) || that(element)

  final def intersection(that: OldSet): OldSet =
    element => this(element) && that(element)

  final def difference(that: OldSet): OldSet =
    element => this(element) && !that(element)

  final def isSubOldSetOf(that: OldSet): Boolean = ???

}

object OldSet {
  val empty: OldSet = input => false
}
