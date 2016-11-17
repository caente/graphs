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

    def create[A: Eq](nodes: Seq[A])(relation: (A, A) => Boolean): DirectedGraph[A] =
      DAG(nodes)(relation).getOrElse(empty)

    private[graph] def unsafe[A: Eq](gr: Graph[A]): DirectedGraph[A] = new DirectedGraph(gr) {}

  }

  object DAG {
    def empty[A: Eq]: DAG[A] = Xor.right(DirectedGraph.empty)
    def apply[A: Eq](nodes: Seq[A])(relation: (A, A) => Boolean): DAG[A] = {
      nodes.foldLeft(DAG.empty[A]) {
        (xgr, x) =>
          nodes.foldLeft(xgr.flatMap(_.addNode(x))) {
            case (Xor.Right(gr), s) if x =!= s && relation(x, s) => gr.addEdge(x, s)
            case (grs, _) => grs
          }
      }
    }
  }

  sealed abstract case class DirectedGraph[A: Eq] private (private val data: Graph[A]) {

    def adjacents(a: A): Set[A] = data.get(a).toSet.flatten

    val (
      roots: Set[A],
      leafs: Set[A],
      nodes: Set[A]
      ) = data.keys.toList.foldLeft((HashSet.empty[A], HashSet.empty[A], HashSet.empty[A])) {
      case ((roots, leafs, nodes), a) if adjacents(a).isEmpty =>
        (roots, leafs + a, nodes + a)
      case ((roots, leafs, nodes), a) if data.values.forall(!_.exists(_ === a)) =>
        (roots + a, leafs, nodes + a)
      case ((roots, leafs, nodes), a) =>
        (roots, leafs, nodes + a)
    }

    val order: List[A] =
      nodes.foldLeft((List.empty[A], HashSet.empty[A])) {
        case ((sorted, visited), a) => fold(Set(a), visited)(sorted)(_ :: _)
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
              (gr2, b) => gr2.updated(b, adjacents(a).flatMap(f))
            }
        }
      )

    def map[B: Eq](f: A => B): DirectedGraph[B] =
      DirectedGraph.unsafe(
        fold(nodes)(Graph.empty[B])((a, gr) => gr.updated(f(a), adjacents(a).map(f)))._1
      )

    def collect[B: Eq](f: PartialFunction[A, B]): DirectedGraph[B] =
      DirectedGraph.unsafe(
        fold(nodes)(Graph.empty[B]) {
        (a, gr) =>
          if (f.isDefinedAt(a)) gr.updated(f(a), adjacents(a).collect(f))
          else gr
      }._1
      )

    private def fold[B](ns: Set[A], vs: HashSet[A] = HashSet.empty)(z: B)(f: (A, B) => B): (B, HashSet[A]) =
      if (ns.isEmpty) (z, vs)
      else if (vs.contains(ns.head)) fold(ns.tail, vs)(z)(f)
      else {
        val x = ns.head
        val xs = ns.tail
        val (result, visited) = fold(adjacents(x), vs + x)(z)(f)
        fold(xs.toSet, visited)(f(x, result))(f)
      }

    def previous(a: A): List[A] =
      reverse.withRoot(_ === a).order match {
        case Nil => Nil
        case _ :: xs => xs
      }

    def reverse =
      nodes.foldLeft(DAG.empty[A]) {
        (gr, a) =>
          adjacents(a).foldLeft(gr) {
            (gra, x) => gra.flatMap(_.addEdge(x, a))
          }
      }.getOrElse(DirectedGraph.empty[A])

    def connected(x: A, y: A): Boolean = path(x, y).nonEmpty

    def path(x: A, y: A): List[A] = {
      val nodesFromX = fold(Set(x))(List.empty[A])(_ :: _)._1
      if (nodesFromX.exists(_ === y)) nodesFromX.takeWhile(_ =!= y) :+ y
      else Nil
    }

    def withRoot(f: A => Boolean): DirectedGraph[A] = {
      DirectedGraph.unsafe(
        fold(nodes.filter(f))(Graph.empty[A])((a, gr) => gr.updated(a, adjacents(a)))._1
      )
    }

    def addNode(node: A): DAG[A] =
      if (nodes.contains(node)) Xor.right(this)
      else Xor.right(DirectedGraph.unsafe(data.updated(node, Set.empty)))

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
