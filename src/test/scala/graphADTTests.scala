package common
package test

import org.scalatest._
import org.scalactic.TypeCheckedTripleEquals
import graph2._

class GraphTests extends FunSuite with TypeCheckedTripleEquals {

  test("Besides before Single") {
    implicit val relation: Relation[Int] = (x, y) => x > y && y % 2 == 0
    assert(
      (Single(3) append (Single(1) append Single(2))) === Besides(Before(Single(3), Single(2)), Single(1))
    )
  }
  val nodes = Map(
    1 -> List(3),
    2 -> List(3),
    3 -> List(5),
    4 -> List(6),
    5 -> List(6),
    6 -> List()
  )

  /*
 
1 ---|
     |
     V
     3-->5
     ^   |
     |   |
2----|   | 
         V 
4------->6

*/

  implicit val relation: Relation[Int] = (x, y) => nodes(x).contains(y)

  test("add to empty") {
    assert(
      DAG.empty[Int].append(Single(1)) === Single(1)
    )
  }

  test("add related to Single") {
    assert(
      Single(1)
        .append(Single(2)) === Besides(Single(1), Single(2))
    )
  }

  test("add related to Besides") {
    assert(
      Single(1)
        .append(Single(2))
        .append(Single(3)) === Before(Besides(Single(1), Single(2)), Single(3))
    )
  }

  test("add non-related to Before") {
    assert(
      Single(1)
        .append(Single(2))
        .append(Single(3))
        .append(Single(4)) === Besides(Before(Besides(Single(1), Single(2)), Single(3)), Single(4))
    )
  }

  test("add partially-related to Besides") {
    assert(
      Single(1)
        .append(Single(2))
        .append(Single(3))
        .append(Single(4))
        .append(Single(5)) === Besides(Before(Before(Besides(Single(1), Single(2)), Single(3)), Single(5)), Single(4))
    )
  }

  test("add double-related to Besides") {
    assert(
      Single(1)
        .append(Single(2))
        .append(Single(3))
        .append(Single(4))
        .append(Single(5))
        .append(Single(6)) === Before(Besides(Before(Before(Besides(Single(1), Single(2)), Single(3)), Single(5)), Single(4)), Single(6))
    )
  }

  test("root") {
    assert(
      Single(1)
        .append(Single(2))
        .append(Single(3))
        .append(Single(4))
        .append(Single(5))
        .append(Single(6))
        .root === Besides(Besides(Single(1), Single(2)), Single(4))
    )
  }

  test("leaf") {
    assert(
      Single(1)
        .append(Single(2))
        .append(Single(3))
        .append(Single(4))
        .append(Single(5))
        .append(Single(6))
        .leaf === Single(6)
    )
  }
  test("Single connected") {
    assert(
      Single(1).connected(Single(3))
    )
  }
  test("Before connected") {
    assert(
      Single(1).append(Single(2)).connected(Single(3))
    )
  }
  test("generator") {
    assert(
      DAG(List(1, 2, 3, 4, 5, 6)) === Before(Besides(Before(Before(Besides(Single(1), Single(2)), Single(3)), Single(5)), Single(4)), Single(6))
    )
  }
}
