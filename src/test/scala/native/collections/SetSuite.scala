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

    val set = Set.empty.add(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("add on a non empty Set should yield a new Set with two elements") {
    val first = randomElement
    val second = randomElement

    first should not be second

    val set = Set.empty.add(first).add(second)

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

    val setWithElement = Set.empty.add(first)
    val setWithoutElement = setWithElement.remove(first)

    setWithoutElement(first) shouldBe false
  }

  test("remove removes only the element in question") {
    val first = randomElement
    val second = randomElement

    first should not be second

    val setWithElements = Set.empty.add(first).add(second)
    val setWithoutElements = setWithElements.remove(second)

    setWithoutElements(first) shouldBe true
    setWithoutElements(second) shouldBe false
  }

  test("remove removes only the element in question 2") {
    val first = randomElement
    val second = randomElement

    first should not be second

    val setWithElements = Set.empty.add(first).add(second)
    val setWithoutElements = setWithElements.remove(first)

    setWithoutElements(first) shouldBe false
    setWithoutElements(second) shouldBe true
  }

  test("add/remove combo should ensure that all elements are distinct") {
    val element = randomElement

    val set = Set.empty.add(element).add(element).remove(element)

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

    val emptySet = Set.empty
    val setWithElement = emptySet.add(first).add(second)

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

    val left = Set.empty.add(a).add(b)
    val right = Set.empty.add(c).add(d)

    left.union(right) shouldBe Set.empty.add(a).add(b).add(c).add(d)
    right.union(left) shouldBe Set.empty.add(a).add(b).add(c).add(d)
  }

  test("intersection on empty Set should yield an empty Set") {
    Set.empty.intersection(Set.empty)(randomElement) shouldBe false
  }

  test(
    "intersection on a non empty Set with an empty Set should yield an empty Set"
  ) {
    val first = randomElement
    val second = randomElement

    val emptySet = Set.empty
    val setWithElement = emptySet.add(first).add(second)

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

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(b).add(c).add(d)

    val leftIntersection = left.intersection(right)

    left.intersection(right) shouldBe Set.empty.add(b).add(c)
    right.intersection(left) shouldBe Set.empty.add(b).add(c)
  }

  test("difference on empty Set should yield an empty Set") {
    Set.empty.difference(Set.empty)(randomElement) shouldBe false
  }

  test(
    "difference on a non empty Set with an empty Set should yield an empty set"
  ) {
    val first = randomElement
    val second = randomElement

    val emptySet = Set.empty
    val setWithElement = emptySet.add(first).add(second)

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

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(b).add(c).add(d)

    val leftDifference = left.difference(right)

    left.difference(right) shouldBe Set.empty.add(a)
    right.difference(left) shouldBe Set.empty.add(d)
  }

  test("isSubSetOf on an empty Set should yield true") {
    Set.empty.isSubSetOf(Set.empty) shouldBe true
    Set.empty.isSubSetOf(Set.empty.add(randomElement)) shouldBe true
  }

  test("isSubSetOf on itself should yield true") {
    val set = Set.empty.add(randomElement)

    set.isSubSetOf(set) shouldBe true
  }

  test("isSubSetOf on a non empty Set should  yield false") {
    val a = randomElement
    val b = randomElement
    val c = randomElement

    val left = Set.empty.add(a).add(b)
    val right = left.add(c)

    left.isSubSetOf(right) shouldBe true
    right.isSubSetOf(left) shouldBe false
  }

  test("isSuperSetOf on an empty Set should yield true") {
    Set.empty.isSuperSetOf(Set.empty) shouldBe true
    Set.empty.isSuperSetOf(Set.empty.add(randomElement)) shouldBe false
  }

  test("isSuperSetOf on itself should yield true") {
    val set = Set.empty.add(randomElement)

    set.isSuperSetOf(set) shouldBe true
  }

  test("isSuperSetOf on a non empty Set should yield false") {
    val a = randomElement
    val b = randomElement
    val c = randomElement

    val left = Set.empty.add(a).add(b)
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

    Set.empty.add(first).add(second).hashCode shouldBe expected
  }

  test("size of an empty Set should be 0") {
    Set.empty.size shouldBe 0
  }

  test("Size of a non empty Set with 2 distinct elements should be 2") {
    val first = randomElement
    val second = randomElement

    first should not be second

    Set.empty.add(first).add(second).size shouldBe 2
  }

  test("size of a non empty Set with 2 equal elements added should be 1") {
    val element = randomElement

    Set.empty.add(element).add(element).size shouldBe 1
  }

  test("isEmpty on an empty Set should yield false") {
    Set.empty.isEmpty shouldBe true
    Set.empty.isNonEmpty shouldBe false
  }

  test("isEmpty on a non empty Set should yield false") {
    Set.empty.add(randomElement).isEmpty shouldBe false
    Set.empty.add(randomElement).isNonEmpty shouldBe true
  }

  test("isSingleton on an empty Set should yield false") {
    Set.empty.isSingleton shouldBe false
  }

  test("isSingleton on a Set with more than one element should yield false") {
    val first = randomElement
    val second = randomElement

    first should not be second

    Set.empty.add(first).add(second).isSingleton shouldBe false
  }

  test("isSingleton on a Set with a single element should yield true") {
    Set.empty.add(randomElement).isSingleton shouldBe true
  }

  test("sample should yield a random element from the Set") {
    Set.empty.sample shouldBe None

    val a = randomElement
    Set.empty.add(a).sample shouldBe Some(a)

    val b = randomElement
    Set.empty.add(a).add(b).sample should contain oneOf (a, b)
  }

  private def randomElement: String =
    scala.util.Random.alphanumeric.take(5).mkString

}
