package native.collections

sealed trait Set extends (String => Boolean) {
  import Set._

  final def apply(input: String): Boolean = {
    var result = false
    foreach { current =>
      result = result || current == input
    }
    result
  }

  final def add(input: String): Set = {
    var result = NonEmpty(input, Empty)
    foreach { current =>
      if (current != input)
        result = NonEmpty(current, result)
    }
    result
  }

  final def remove(input: String): Set = {
    var result = empty
    foreach { current =>
      if (current != input)
        result = NonEmpty(current, result)
    }
    result
  }

  final def union(that: Set): Set = {
    var result = that
    foreach { current =>
      result = result.add(current)
    }
    result
  }

  final def intersection(that: Set): Set = {
    var result = empty
    foreach { current =>
      if (that(current))
        result = result.add(current)
    }
    result
  }

  final def difference(that: Set): Set = {
    var result = empty
    foreach { current =>
      if (!that(current))
        result = result.add(current)
    }
    result
  }

  final def isSubSetOf(that: Set): Boolean = {
    var result = true
    foreach { current =>
      result = result && that(current)
    }
    result

  }

  final def isSuperSetOf(that: Set): Boolean =
    that.isSubSetOf(this)

  // override here because of already implemented equality from case class
  final override def equals(other: Any): Boolean =
    other match {
      case that: Set => this.isSubSetOf(that) && that.isSubSetOf(this)
      case _         => false
    }

  final def size: Int = {
    var result = 0
    foreach { current =>
      result = result + 1
    }
    result
  }

  final def isEmpty: Boolean =
    this eq Set.empty

  final def isNonEmpty: Boolean = !isEmpty

  def isSingleton: Boolean

  def sample: Option[String]

  def foreach(function: String => Unit): Unit
}

object Set {

  // adding singnature apply(element, otherelement) makes Set() not to compile
  def apply(element: String, otherElements: String*): Set = {
    var result: Set = empty.add(element)
    otherElements.foreach(current => result = result.add(current))
    result
  }

  private final case class NonEmpty(element: String, otherElements: Set)
      extends Set {

    final override def hashCode: Int =
      element.hashCode + otherElements.hashCode

    final override def isSingleton: Boolean =
      otherElements.isEmpty

    final override def sample: Option[String] = Some(element)

    final override def foreach(function: String => Unit): Unit = {
      function(element)
      otherElements.foreach(function)
    }

  }

  private object Empty extends Set {

    final override def isSingleton: Boolean = false

    final override def sample: Option[String] = None

    final override def foreach(function: String => Unit): Unit = ()
  }

  val empty: Set = Empty

}
