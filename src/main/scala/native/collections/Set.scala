package native.collections

import scala.collection.View.Elems
import java.lang.annotation.ElementType

sealed trait Set[Element] extends (Element => Boolean) {
  import Set._

  final def apply(input: Element): Boolean = {
    var result = false
    foreach { current =>
      result = result || current == input
    }
    result
  }

  final def add(input: Element): Set[Element] = {
    var result = NonEmpty(input, empty[Element])
    foreach { current =>
      if (current != input)
        result = NonEmpty(current, result)
    }
    result
  }

  final def remove(input: Element): Set[Element] = {
    var result = empty[Element]
    foreach { current =>
      if (current != input)
        result = NonEmpty(current, result)
    }
    result
  }

  final def union(that: Set[Element]): Set[Element] = {
    var result = that
    foreach { current =>
      result = result.add(current)
    }
    result
  }

  final def intersection(that: Set[Element]): Set[Element] = {
    var result = empty[Element]
    foreach { current =>
      if (that(current))
        result = result.add(current)
    }
    result
  }

  final def difference(that: Set[Element]): Set[Element] = {
    var result = empty[Element]
    foreach { current =>
      if (!that(current))
        result = result.add(current)
    }
    result
  }

  final def isSubSetOf(that: Set[Element]): Boolean = {
    var result = true
    foreach { current =>
      result = result && that(current)
    }
    result

  }

  final def isSuperSetOf(that: Set[Element]): Boolean =
    that.isSubSetOf(this)

  // override here because of already implemented equality from case class
  final override def equals(other: Any): Boolean =
    other match {
      case that: Set[Element] => this.isSubSetOf(that) && that.isSubSetOf(this)
      case _                  => false
    }

  final override def hashCode: Int = {
    if (isEmpty)
      41
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
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
    //this eq empty // TODO fix me
    this.isInstanceOf[Empty[Element]]

  final def isNonEmpty: Boolean = !isEmpty

  final def isSingleton: Boolean =
    if (isEmpty)
      false
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val otherElements = nonEmptySet.otherElements

      otherElements.isEmpty
    }

  def sample: Option[Element] =
    if (isEmpty)
      None
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      Some(element)
    }

  final def foreach[T](function: Element => T): Unit = {
    if (isNonEmpty) {
      //   val NonEmpty(element, otherElements) = this // equality is expansive for bigger Set

      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements

      function(element)
      otherElements.foreach(function)
    }
  }

  final def map[T](function: Element => T): Set[T] = {
    var result = empty[T]

    foreach { current =>
      result = result.add(function(current))
    }
    result
  }

  // this implimentation is not a optimised one as we are wrapping all elements in
  // Set which is a expensive operation
//   final def map[T](function: Element => T): Set[T] = {
//     flatMap { current =>
//       Set(function(current))
//     }
//   }

  final def flatMap[T](function: Element => Set[T]): Set[T] = {
    var result = empty[T]

    foreach { outerCurrent =>
      function(outerCurrent).foreach { innerCurrent =>
        result = result.add(innerCurrent)
      }
    }
    result
  }

}

object Set {

  // adding singnature apply(element, otherelement) makes Set() not to compile
  def apply[T](element: T, otherElements: T*): Set[T] = {
    var result: Set[T] = empty[T].add(element)
    otherElements.foreach(current => result = result.add(current))
    result
  }

  private final case class NonEmpty[T](
      element: T,
      otherElements: Set[T]
  ) extends Set[T]

  private class Empty[T] extends Set[T]

  def empty[T]: Set[T] = new Empty[T]

}
