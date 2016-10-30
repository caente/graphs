package common

import org.joda.time._
import cats._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.instances.all._
import cats.data.Xor
import collection.immutable.{ HashMap, HashSet }
import cats.syntax.foldable._

package object graph {
  type Graph[A] = HashMap[A, Set[A]]
  type DAG[A] = Xor[Graph.Cycle[A], DirectedGraph[A]]

  object Graph {
    case class Cycle[A](values: List[A])

    def empty[A] = HashMap.empty[A, Set[A]]

  }

  object DirectedGraph {

    def empty[A: Eq] = unsafe(Graph.empty[A])

    private[graph] def unsafe[A: Eq](gr: Graph[A]): DirectedGraph[A] = new DirectedGraph(gr) {}

  }

  object DAG {
    def empty[A: Eq]: DAG[A] = Xor.right(DirectedGraph.empty)

    def apply[A: Eq](nodes: Seq[A])(relation: (A, A) => Boolean): DAG[A] = {
      nodes.foldLeft(DAG.empty[A]) {
        (xgr, x) =>
          nodes.foldLeft(xgr) {
            case (Xor.Right(gr), s) if x =!= s && relation(x, s) => gr.addEdge(x, s)
            case (grs, _) => grs
          }
      }
    }
  }

  sealed abstract case class DirectedGraph[A: Eq] private (val data: Graph[A]) {

    def adjacents(a: A): Set[A] = data.get(a).toSet.flatten

    val (
      initials: Set[A],
      finals: Set[A],
      nodes: Set[A]
      ) = data.keys.toList.foldLeft((HashSet.empty[A], HashSet.empty[A], HashSet.empty[A])) {
      case ((initials, finals, nodes), a) if adjacents(a).isEmpty =>
        (initials, finals + a, nodes + a)
      case ((initials, finals, nodes), a) if data.values.forall(!_.exists(_ === a)) =>
        (initials + a, finals, nodes + a)
      case ((initials, finals, nodes), a) =>
        (initials, finals, nodes + a)
    }

    val order: List[A] =
      nodes.foldLeft((List.empty[A], HashSet.empty[A])) {
        case ((sorted, visited), a) => fold(sorted)(Set(a), visited)(_ :: _)
      }._1

    def filter(f: A => Boolean): DirectedGraph[A] =
      DirectedGraph.unsafe(
        data.collect { case (a, as) if f(a) => a -> as.filter(f) }
      )

    def expand[B: Eq](f: A => Set[B]): DirectedGraph[B] =
      DirectedGraph.unsafe(
        nodes.foldLeft(Graph.empty[B]) {
          (gr, a) =>
            f(a).foldLeft(gr) {
              (gr2, b) => gr2.updated(b, data(a).flatMap(f))
            }
        }
      )

    def map[B: Eq](f: A => B): DirectedGraph[B] =
      DirectedGraph.unsafe(
        fold(Graph.empty[B])(nodes)((a, gr) => gr.updated(f(a), adjacents(a).map(f)))._1
      )

    private def fold[B](z: B)(ns: Set[A], vs: HashSet[A] = HashSet.empty)(f: (A, B) => B): (B, HashSet[A]) =
      if (ns.isEmpty) (z, vs)
      else if (vs.contains(ns.head)) fold(z)(ns.tail, vs)(f)
      else {
        val x = ns.head
        val xs = ns.tail
        val (result, visited) = fold(z)(adjacents(x), vs + x)(f)
        fold(f(x, result))(xs.toSet, visited)(f)
      }

    def connected(x: A, y: A): Boolean = path(x, y).nonEmpty

    def path(x: A, y: A): List[A] = {
      val nodesFromX = fold(List.empty[A])(Set(x))(_ :: _)._1
      if (nodesFromX.exists(_ === y)) nodesFromX.takeWhile(_ =!= y) :+ y
      else Nil
    }

    def fromNode(f: A => Boolean): DirectedGraph[A] = {
      DirectedGraph.unsafe(
        fold(Graph.empty[A])(nodes.filter(f))((a, gr) => gr.updated(a, adjacents(a)))._1
      )
    }

    def addEdge(start: A, end: A): DAG[A] =
      path(end, start) match {
        case Nil =>
          Xor.right(DirectedGraph.unsafe(data.updated(start, (data.getOrElse(start, HashSet.empty) + end))))
        case x :: Nil => Xor.right(this)
        case cycle => Xor.left(Graph.Cycle(start :: cycle))
      }

    override def toString = data.map {
      case (a, as) => s"$a -> ${as.mkString(", ")}"
    }.mkString("\n")
  }

  val ls = (1 to 10)
  val smallerAndEven: (Int, Int) => Boolean = (a1, a2) => a1 >= a2 && a2 % 2 == 0
  val gr = DAG(ls)(smallerAndEven).getOrElse(DirectedGraph.empty[Int])
  def time[A](f: => A): A = {
    val start = DateTime.now
    val r = f
    val end = DateTime.now
    println(new Duration(start, end))
    r
  }

}
