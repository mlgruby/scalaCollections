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

  final override def hashCode: Int = {
    if (isEmpty)
      41
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements

      element.hashCode + otherElements.hashCode
    }

  }

  final def size: Int = {
    var result = 0
    foreach { _ =>
      result = result + 1
    }
    result
  }

  final def isEmpty: Boolean =
    this eq Set.empty

  final def isNonEmpty: Boolean = !isEmpty

  final def isSingleton: Boolean =
    if (isEmpty)
      false
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty]
      val otherElements = nonEmptySet.otherElements

      otherElements.isEmpty
    }

  def sample: Option[String] =
    if (isEmpty)
      None
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty]
      val element = nonEmptySet.element
      Some(element)
    }

  final def foreach(function: String => Unit): Unit = {
    if (isNonEmpty) {
      //   val NonEmpty(element, otherElements) = this // equality is expansive for bigger Set

      val nonEmptySet = this.asInstanceOf[NonEmpty]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements

      function(element)
      otherElements.foreach(function)
    }
  }

}

object Set {

  // adding singnature apply(element, otherelement) makes Set() not to compile
  def apply(element: String, otherElements: String*): Set = {
    var result: Set = empty.add(element)
    otherElements.foreach(current => result = result.add(current))
    result
  }

  private final case class NonEmpty(element: String, otherElements: Set)
      extends Set

  private object Empty extends Set

  val empty: Set = Empty

}
