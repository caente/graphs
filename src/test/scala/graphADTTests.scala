package common
package test

import org.scalatest._
import org.scalactic.TypeCheckedTripleEquals
import graph2._

class GraphTests extends FunSuite with TypeCheckedTripleEquals {
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
    DAG.empty[Int].append(Single(1)) === Single(1)
  }

  test("add related to Single") {
    DAG.empty[Int]
      .append(Single(1))
      .append(Single(2)) === Besides(Single(1), Single(2))
  }

  test("add related to Besides") {
    DAG.empty[Int]
      .append(Single(1))
      .append(Single(2))
      .append(Single(3)) === Before(Besides(Single(1), Single(2)), Single(3))
  }

  test("add non-related to Before") {
    DAG.empty[Int]
      .append(Single(1))
      .append(Single(2))
      .append(Single(3))
      .append(Single(4)) === Besides(Before(Besides(Single(1), Single(2)), Single(3)), Single(4))
  }

  test("add partially-related to Besides") {
    DAG.empty[Int]
      .append(Single(1))
      .append(Single(2))
      .append(Single(3))
      .append(Single(4))
      .append(Single(5)) === Besides(Before(Before(Besides(Single(1), Single(2)), Single(3)), Single(5)), Single(4))
  }

  test("add double-related to Besides") {
    DAG.empty[Int]
      .append(Single(1))
      .append(Single(2))
      .append(Single(3))
      .append(Single(4))
      .append(Single(5))
      .append(Single(6)) === Before(Besides(Before(Before(Besides(Single(1), Single(2)), Single(3)), Single(5)), Single(4)), Single(6))
  }
}
