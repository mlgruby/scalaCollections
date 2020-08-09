import native.collections._

import org.scalatest._
import org.scalatest.time.Second

class SetSuite extends FunSuite with Matchers {
  test("apply on an empty Set should yield false") {
    Set.empty(randomElement) shouldBe false
    Set.empty.size shouldBe 0
  }

  test("add on an empty Set should yield a new Set with one element") {
    val first = randomElement
    val second = randomElement

    first should not be second

    val set = Set(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("add on a non empty Set should yield a new Set with two elements") {
    val first = randomElement
    val second = randomElement

    first should not be second

    val set = Set(first, second)

    set(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove on an empty Set should yield an empty Set") {
    val element = randomElement
    val stillEmpty = Set.empty.remove(element)
    stillEmpty(element) shouldBe false
  }

  test(
    "remove on a non empty Set should yield a new Set withiout the element"
  ) {
    val first = randomElement

    val setWithElement = Set(first)
    val setWithoutElement = setWithElement.remove(first)

    setWithoutElement(first) shouldBe false
  }

  test("remove removes only the element in question") {
    val first = randomElement
    val second = randomElement

    first should not be second

    val setWithElements = Set(first, second)
    val setWithoutElements = setWithElements.remove(second)

    setWithoutElements(first) shouldBe true
    setWithoutElements(second) shouldBe false
  }

  test("remove removes only the element in question 2") {
    val first = randomElement
    val second = randomElement

    first should not be second

    val setWithElements = Set(first, second)
    val setWithoutElements = setWithElements.remove(first)

    setWithoutElements(first) shouldBe false
    setWithoutElements(second) shouldBe true
  }

  test("add/remove combo should ensure that all elements are distinct") {
    val element = randomElement

    val set = Set(element, element).remove(element)

    set(element) shouldBe false
  }

  test("union on empty Set should yield an empty Set") {
    Set.empty.union(Set.empty)(randomElement) shouldBe false
  }

  test(
    "union on a non empty Set with an empty Set should yield the original Set untouched"
  ) {
    val first = randomElement
    val second = randomElement

    val emptySet = Set.empty[String]
    val setWithElement = Set(first, second)

    setWithElement.union(emptySet)(first) shouldBe true
    setWithElement.union(emptySet)(second) shouldBe true

    emptySet.union(setWithElement)(first) shouldBe true
    emptySet.union(setWithElement)(second) shouldBe true
  }

  test("union on two non empty Set should yield thier union") {
    val a = randomElement
    val b = randomElement
    val c = randomElement
    val d = randomElement

    val left = Set(a, b)
    val right = Set.empty.add(c).add(d)

    left.union(right) shouldBe Set(a, b, c, d)
    right.union(left) shouldBe Set(a, b, c, d)
  }

  test("intersection on empty Set should yield an empty Set") {
    Set.empty.intersection(Set.empty)(randomElement) shouldBe false
  }

  test(
    "intersection on a non empty Set with an empty Set should yield an empty Set"
  ) {
    val first = randomElement
    val second = randomElement

    val emptySet = Set.empty[String]
    val setWithElement = Set(first, second)

    setWithElement.intersection(emptySet)(first) shouldBe false
    setWithElement.intersection(emptySet)(second) shouldBe false

    emptySet.intersection(setWithElement)(first) shouldBe false
    emptySet.intersection(setWithElement)(second) shouldBe false
  }

  test("intersection on two non empty Set should yield thier intersection") {
    val a = randomElement
    val b = randomElement
    val c = randomElement
    val d = randomElement

    val left = Set(a, b, c)
    val right = Set(b, c, d)

    val leftIntersection = left.intersection(right)

    left.intersection(right) shouldBe Set(b, c)
    right.intersection(left) shouldBe Set(b, c)
  }

  test("difference on empty Set should yield an empty Set") {
    Set.empty.difference(Set.empty)(randomElement) shouldBe false
  }

  test(
    "difference on a non empty Set with an empty Set should yield an empty set"
  ) {
    val first = randomElement
    val second = randomElement

    val emptySet = Set.empty[String]
    val setWithElement = Set(first, second)

    setWithElement.difference(emptySet)(first) shouldBe true
    setWithElement.difference(emptySet)(second) shouldBe true

    emptySet.difference(setWithElement)(first) shouldBe false
    emptySet.difference(setWithElement)(second) shouldBe false
  }

  test("difference on two non empty Set should yield thier difference") {
    val a = randomElement
    val b = randomElement
    val c = randomElement
    val d = randomElement

    val left = Set(a, b, c)
    val right = Set(b, c, d)

    val leftDifference = left.difference(right)

    left.difference(right) shouldBe Set(a)
    right.difference(left) shouldBe Set(d)
  }

  test("isSubSetOf on an empty Set should yield true") {
    Set.empty.isSubSetOf(Set.empty) shouldBe true
    Set.empty.isSubSetOf(Set(randomElement)) shouldBe true
  }

  test("isSubSetOf on itself should yield true") {
    val set = Set(randomElement)

    set.isSubSetOf(set) shouldBe true
  }

  test("isSubSetOf on a non empty Set should  yield false") {
    val a = randomElement
    val b = randomElement
    val c = randomElement

    val left = Set(a, b)
    val right = left.add(c)

    left.isSubSetOf(right) shouldBe true
    right.isSubSetOf(left) shouldBe false
  }

  test("isSuperSetOf on an empty Set should yield true") {
    Set.empty.isSuperSetOf(Set.empty) shouldBe true
    Set.empty.isSuperSetOf(Set(randomElement)) shouldBe false
  }

  test("isSuperSetOf on itself should yield true") {
    val set = Set(randomElement)

    set.isSuperSetOf(set) shouldBe true
  }

  test("isSuperSetOf on a non empty Set should yield false") {
    val a = randomElement
    val b = randomElement
    val c = randomElement

    val left = Set(a, b)
    val right = left.add(c)

    left.isSuperSetOf(right) shouldBe false
    right.isSuperSetOf(left) shouldBe true
  }

  test("hashCode on an empty Set should not be random") {
    Set.empty.hashCode shouldBe Set.empty.hashCode

    val element = randomElement

    // hashCode here working because of case class we have for non emepty set
    Set.empty.add(element).hashCode shouldBe Set.empty.add(element).hashCode
  }

  test("hashCode on an empty Set should not be 0") {
    Set.empty.hashCode should not be 0
  }

  test(
    "hashCode on a non empty Set should be the sum of all the hashCodes and the hashCode of the empty Set"
  ) {
    val empty = Set.empty

    val first = randomElement
    val second = randomElement

    val expected = Set.empty.hashCode + first.hashCode + second.hashCode

    Set(first, second).hashCode shouldBe expected
  }

  test("size of an empty Set should be 0") {
    Set.empty.size shouldBe 0
  }

  test("Size of a non empty Set with 2 distinct elements should be 2") {
    val first = randomElement
    val second = randomElement

    first should not be second

    Set(first, second).size shouldBe 2
  }

  test("size of a non empty Set with 2 equal elements added should be 1") {
    val element = randomElement

    Set(element, element).size shouldBe 1
  }

  test("isEmpty on an empty Set should yield false") {
    Set.empty.isEmpty shouldBe true
    Set.empty.isNonEmpty shouldBe false
  }

  test("isEmpty on a non empty Set should yield false") {
    Set(randomElement).isEmpty shouldBe false
    Set(randomElement).isNonEmpty shouldBe true
  }

  test("isSingleton on an empty Set should yield false") {
    Set.empty.isSingleton shouldBe false
  }

  test("isSingleton on a Set with more than one element should yield false") {
    val first = randomElement
    val second = randomElement

    first should not be second

    Set(first, second).isSingleton shouldBe false
  }

  test("isSingleton on a Set with a single element should yield true") {
    Set(randomElement).isSingleton shouldBe true
  }

  test("sample should yield a random element from the Set") {
    Set.empty.sample shouldBe None

    val a = randomElement
    Set.empty.add(a).sample shouldBe Some(a)

    val b = randomElement
    Set(a, b).sample should contain oneOf (a, b)
  }

  test("Set() should not compile") {
    "Set()" shouldNot compile
  }

  test(
    "calling the varargs apply method on the Set comapnion object should yield a Set with all the arguments as elements"
  ) {
    val a = randomElement
    val b = randomElement
    val c = randomElement

    Set(a, b, c) shouldBe Set.empty.add(a).add(b).add(c)
  }

  test("foreach on an empty Set should not apply the function") {
    noException should be thrownBy Set
      .empty[String]
      .foreach(_ => sys.error("should not be thrown"))
  }

  test("forerach on non empty Set should apply the function") {
    var functionWasApplied = false

    Set(randomElement).foreach(_ => functionWasApplied = true)

    functionWasApplied shouldBe true
  }

  test("foreach should be able to calculate the size of given Set 0") {
    var size = 0

    val set = Set.empty

    set.foreach(_ => size += 1)

    size shouldBe 0
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of given Set 1") {
    var size = 0

    val set = Set(randomElement)

    set.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of given Set 2") {
    var size = 0

    val set = Set(randomElement).add(randomElement)

    set.foreach(_ => size += 1)

    size shouldBe 2
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of given Set 3") {
    var size = 0

    val element = randomElement

    val set = Set(element, element)

    set.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe set.size
  }

  test(
    "foreach should be paramatrize in the result of the argument function so that it does not produce warnings"
  ) {
    Set.empty[String].foreach(_ => 1)
  }

  test("map on an empty Set should not apply the function") {
    noException should be thrownBy Set
      .empty[String]
      .map(_ => sys.error("should not be thrown"))
  }

  test("map should produce a Set") {
    Set("hello", "world").map(_.reverse) shouldBe Set("dlrow", "olleh")
  }

  test(
    "map should be able to produce a Set of something else other than String"
  ) {
    Set("Hello", "Planet").map(_.size) shouldBe Set(5, 6)

    Set("Hello", "World").map(_.size) shouldBe Set(5)
  }

  test("flatMap should be able to produce a chessboard") {
    val characters = Set('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    val numbers = Set(1, 2, 3, 4, 5, 6, 7, 8)

    val chessboard: Set[(Char, Int)] =
      characters.flatMap { c =>
        numbers.map { n =>
          c -> n
        }
      }
    chessboard.size shouldBe 64
  }

  private def randomElement: String =
    scala.util.Random.alphanumeric.take(5).mkString

}
