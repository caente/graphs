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

  object Graph {
    case class Cycle[A](values: List[A])

    def empty[A] = HashMap.empty[A, Set[A]]

    def addEdge[A](start: A, end: A, ds: Graph[A]): Graph[A] =
      ds.updated(start, (ds.getOrElse(start, HashSet.empty) + end))

    def hasCycle[A: Eq](data: Graph[A]): List[A] = {
      def stoppedAtCycle(a: A, acc: List[A]): List[A] = {
        data.get(a).toList.flatten match {
          case x :: xs if acc.forall(_ =!= x) => stoppedAtCycle(x, x :: acc)
          case x :: xs => acc :+ a
          case Nil => Nil
        }
      }
      data.keys.map { a => stoppedAtCycle(a, List(a)) }.headOption.toList.flatten
    }
  }

  sealed abstract case class DirectedGraph[A: Eq] private (private val data: Graph[A]) {

    def adjacents(a: A): Set[A] = data.get(a).toSet.flatten

    val (initials: Set[A], finals: Set[A], nodes: Set[A]) = data.keys.toList.foldLeft((HashSet.empty[A], HashSet.empty[A], HashSet.empty[A])) {
      case ((initials, finals, nodes), a) if adjacents(a).isEmpty => (initials, finals + a, nodes + a)
      case ((initials, finals, nodes), a) if data.values.forall(!_.exists(_ === a)) => (initials + a, finals, nodes + a)
      case ((initials, finals, nodes), a) => (initials, finals, nodes + a)
    }

    val nodesSorted: List[A] =
      nodes.foldLeft((List.empty[A], HashSet.empty[A])) {
        case ((sorted, visited), a) => dfs(Set(a), sorted, visited)(_ :: _)
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
        dfs(nodes, Graph.empty[B])((a, gr) => gr.updated(f(a), adjacents(a).map(f)))._1
      )

    private def dfs[B](ns: Set[A], s: B, z: HashSet[A] = HashSet.empty)(f: (A, B) => B): (B, HashSet[A]) =
      if (ns.isEmpty) (s, z)
      else if (z.contains(ns.head)) dfs(ns.tail, s, z)(f)
      else {
        val x = ns.head
        val xs = ns.tail
        val (result, visited) = dfs(adjacents(x), s, z + x)(f)
        dfs(xs.toSet, f(x, result), visited)(f)
      }

    def connected(x: A, y: A): Boolean = path(x, y).nonEmpty

    def path(x: A, y: A): List[A] = {
      val nodesFromX = dfs(Set(x), List.empty[A])(_ :: _)._1
      if (nodesFromX.exists(_ === y)) nodesFromX.takeWhile(_ =!= y) :+ y
      else Nil
    }

    def fromNode(f: A => Boolean): DirectedGraph[A] = {
      DirectedGraph.unsafe(
        dfs(nodes.filter(f), Graph.empty[A])((a, gr) => gr.updated(a, adjacents(a)))._1
      )
    }

    override def toString = data.map {
      case (a, as) => s"$a -> ${as.mkString(", ")}"
    }.mkString("\n")
  }
  object DirectedGraph {

    def unsafe[A: Eq](gr: Graph[A]): DirectedGraph[A] = new DirectedGraph(gr) {}
    def apply[A: Eq](gr: Graph[A]): Xor[Graph.Cycle[A], DirectedGraph[A]] = {
      val cycle = Graph.hasCycle(gr)
      if (cycle.nonEmpty) Xor.left(Graph.Cycle(cycle))
      else Xor.right(new DirectedGraph(gr) {})
    }

    def apply[A: Eq](nodes: Seq[A])(relation: (A, A) => Boolean): Xor[Graph.Cycle[A], DirectedGraph[A]] = {
      def loop(els: List[A], acc: Graph[A]): Xor[Graph.Cycle[A], Graph[A]] = els match {
        case Nil => Xor.right(acc)
        case x :: xs =>
          val updated = acc.updated(x, nodes.toSet.filter(s => x =!= s && relation(x, s)))
          val cycle = Graph.hasCycle(updated)
          if (cycle.nonEmpty) Xor.left(Graph.Cycle(cycle))
          else loop(xs, updated)
      }
      loop(nodes.toList, Graph.empty).map(new DirectedGraph(_) {})
    }

  }

  val ls = (1 to 10)
  val smallerAndEven: (Int, Int) => Boolean = (a1, a2) => a1 >= a2 && a2 % 2 == 0
  val gr = DirectedGraph(ls)(smallerAndEven).getOrElse(throw new Exception())
  def time[A](f: => A): A = {
    val start = DateTime.now
    val r = f
    val end = DateTime.now
    println(new Duration(start, end))
    r
  }

}
