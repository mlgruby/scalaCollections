package native.collections

import scala.collection.View.Elems
import java.lang.annotation.ElementType

sealed trait Set[Element] extends (Element => Boolean) {
  import Set._

  final def apply(input: Element): Boolean = {
    fold(false) { (acc, current) =>
      acc || current == input
    }
  }

  @scala.annotation.tailrec
  final def fold[T](seed: T)(function: (T, Element) => T): T = {
    if (isEmpty)
      seed
    else {
      //   val NonEmpty(element, otherElements) = this // equality is expansive for bigger Set

      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements

      //   val elementResult: T = function(seed, element) // this line for readability
      otherElements.fold(function(seed, element))(function)
    }
  }

  final def add(input: Element): Set[Element] =
    // var result = NonEmpty(input, empty[Element])
    // foreach { current =>
    //   if (current != input)
    //     result = NonEmpty(current, result)
    // }
    // result
    fold(NonEmpty(input, empty[Element])) { (acc, current) =>
      if (current != input)
        NonEmpty(current, acc)
      else
        acc
    }

  final def remove(input: Element): Set[Element] =
//     var result = empty[Element]
//     foreach { current =>
//       if (current != input)
//         result = NonEmpty(current, result)
//     }
//     result
    fold(empty[Element]) { (acc, current) =>
      if (current != input)
        NonEmpty(current, acc)
      else
        acc
    }

  final def union(that: Set[Element]): Set[Element] =
    // var result = that
    // foreach { current =>
    //   result = result.add(current)
    // }
    // result
    fold(that) { (acc, current) => acc.add(current) }

  final def intersection(that: Set[Element]): Set[Element] =
    // var result = empty[Element]
    // foreach { current =>
    //   if (that(current))
    //     result = result.add(current)
    // }
    // result
    fold(empty[Element]) { (acc, current) =>
      if (that(current))
        acc.add(current)
      else
        acc

    }

  final def difference(that: Set[Element]): Set[Element] =
    // var result = empty[Element]
    // foreach { current =>
    //   if (!that(current))
    //     result = result.add(current)
    // }
    // result
    fold(empty[Element]) { (acc, current) =>
      if (!that(current))
        acc.add(current)
      else
        acc
    }

  final def isSubSetOf(that: Set[Element]): Boolean =
    // var result = true
    // foreach { current =>
    //   result = result && that(current)
    // }
    // result
    fold(true) { (acc, current) => acc && that(current) }

  final def isSuperSetOf(that: Set[Element]): Boolean =
    that.isSubSetOf(this)

  // override here because of already implemented equality from case class
  final override def equals(other: Any): Boolean =
    other match {
      case that: Set[Element] => this.isSubSetOf(that) && that.isSubSetOf(this)
      case _                  => false
    }

  final override def hashCode: Int =
    // var result = 41

    // foreach { current =>
    //   result = result + current.hashCode
    // }
    // result
    fold(41) { (acc, current) => acc + current.hashCode }

  final def size: Int =
    // var result = 0
    // foreach { _ =>
    //   result = result + 1
    // }
    // result
    fold(0) { (acc, _) => acc + 1 }

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

  final def foreach[T](function: Element => T): Unit =
//       if (isNonEmpty) {
//       //   val NonEmpty(element, otherElements) = this // equality is expensive for bigger Set

//       val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
//       val element = nonEmptySet.element
//       val otherElements = nonEmptySet.otherElements

//       function(element)
//       otherElements.foreach(function)
//     }
    fold(()) { (_, current) => function(current) }

  final def map[T](function: Element => T): Set[T] =
    // var result = empty[T]

    // foreach { current =>
    //   result = result.add(function(current))
    // }
    // result
    fold(empty[T]) { (acc, current) => acc.add(function(current)) }

  // this implimentation is not a optimised one as we are wrapping all elements in
  // Set which is a expensive operation
//   final def map[T](function: Element => T): Set[T] = {
//     flatMap { current =>
//       Set(function(current))
//     }
//   }

  final def flatMap[T](function: Element => Set[T]): Set[T] =
    // var result = empty[T]

    // foreach { outerCurrent =>
    //   function(outerCurrent).foreach { innerCurrent =>
    //     result = result.add(innerCurrent)
    //   }
    // }
    // result
    fold(empty[T]) { (outerAcc, outerCurrent) =>
      function(outerCurrent).fold(outerAcc) { (innerAcc, innerCurrent) =>
        innerAcc.add(innerCurrent)
      }
    }

}

object Set {

  // adding singnature apply(element, otherelement) makes Set() not to compile
  def apply[T](element: T, otherElements: T*): Set[T] =
    // var result: Set[T] = empty[T].add(element)
    // otherElements.foreach(current => result = result.add(current))
    // result
    otherElements.foldLeft(empty[T].add(element)) { (acc, current) =>
      acc.add(current)
    }

  private final case class NonEmpty[T](element: T, otherElements: Set[T])
      extends Set[T]

  private class Empty[T] extends Set[T]

  def empty[T]: Set[T] = new Empty[T]

}
