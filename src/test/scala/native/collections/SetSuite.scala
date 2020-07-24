import native.collections._

import org.scalatest._
import org.scalatest.time.Second

class SetSuite extends FunSuite with Matchers {
  test("apply on an empty Set should yield false") {
    Set.empty(randomElement) shouldBe false
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

  test("union on empty Set should yield an empty Set") {
    Set.empty.union(Set.empty)(randomElement) shouldBe false
  }

  test(
    "union on a non empty Set with an empty Set should yield the original Set untouched"
  ) {
    val first = randomElement
    val second = randomElement

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement.union(Set.empty)(first) shouldBe true
    setWithElement.union(Set.empty)(second) shouldBe true

    Set.empty.union(setWithElement)(first) shouldBe true
    Set.empty.union(setWithElement)(second) shouldBe true
  }

  test("union on two non empty Set should yield thier union") {
    val a = randomElement
    val b = randomElement
    val c = randomElement
    val d = randomElement

    val left = Set.empty.add(a).add(b)
    val right = Set.empty.add(c).add(d)

    val leftUnion = left.union(right)

    leftUnion(a) shouldBe true
    leftUnion(b) shouldBe true
    leftUnion(c) shouldBe true
    leftUnion(d) shouldBe true

    val rightUnion = left.union(right)

    rightUnion(a) shouldBe true
    rightUnion(b) shouldBe true
    rightUnion(c) shouldBe true
    rightUnion(d) shouldBe true

  }

  test("intersection on empty Set should yield an empty Set") {
    Set.empty.union(Set.empty)(randomElement) shouldBe false
  }

  test(
    "intersection on a non empty Set with an empty Set should yield an empty set"
  ) {
    val first = randomElement
    val second = randomElement

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement.intersection(Set.empty)(first) shouldBe false
    setWithElement.intersection(Set.empty)(second) shouldBe false

    Set.empty.intersection(setWithElement)(first) shouldBe false
    Set.empty.intersection(setWithElement)(second) shouldBe false
  }

  test("intersection on two non empty Set should yield thier intersection") {
    val a = randomElement
    val b = randomElement
    val c = randomElement
    val d = randomElement

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(b).add(c).add(d)

    val leftIntersection = left.intersection(right)

    leftIntersection(a) shouldBe false
    leftIntersection(b) shouldBe true
    leftIntersection(c) shouldBe true
    leftIntersection(d) shouldBe false

    val rightIntersection = left.intersection(right)

    rightIntersection(a) shouldBe false
    rightIntersection(b) shouldBe true
    rightIntersection(c) shouldBe true
    rightIntersection(d) shouldBe false

  }

  test("difference on empty Set should yield an empty Set") {
    Set.empty.difference(Set.empty)(randomElement) shouldBe false
  }

  test(
    "difference on a non empty Set with an empty Set should yield an empty set"
  ) {
    val first = randomElement
    val second = randomElement

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement.difference(Set.empty)(first) shouldBe true
    setWithElement.difference(Set.empty)(second) shouldBe true

    Set.empty.difference(setWithElement)(first) shouldBe false
    Set.empty.difference(setWithElement)(second) shouldBe false
  }

  test("difference on two non empty Set should yield thier difference") {
    val a = randomElement
    val b = randomElement
    val c = randomElement
    val d = randomElement

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(b).add(c).add(d)

    val leftDifference = left.difference(right)

    leftDifference(a) shouldBe true
    leftDifference(b) shouldBe false
    leftDifference(c) shouldBe false
    leftDifference(d) shouldBe false

    val rightDifference = right.difference(left)

    rightDifference(a) shouldBe false
    rightDifference(b) shouldBe false
    rightDifference(c) shouldBe false
    rightDifference(d) shouldBe true
  }

  test("isSubsetOf on an empty Set should yield true") {
    pending
    Set.empty.isSubsetOf(Set.empty) shouldBe true
    Set.empty.isSubsetOf(Set.empty.add(randomElement)) shouldBe true
  }

  private def randomElement: String =
    scala.util.Random.alphanumeric.take(5).mkString

}