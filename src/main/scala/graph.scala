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
  type Graph[A] = HashMap[A, List[A]]

  object Graph {
    case class Cycle[A](values: List[A])

    def empty[A] = HashMap.empty[A, List[A]]

    def addEdge[A](start: A, end: A, ds: Graph[A]): Graph[A] =
      ds.updated(start, (ds.getOrElse(start, Nil) :+ end).distinct)

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

  final case class DirectedGraph[A: Eq] private (private val data: Graph[A]) {

    def adjacents(a: A): List[A] = data.get(a).toList.flatten

    val (initials, finals, nodes) = data.keys.toList.foldLeft((List.empty[A], List.empty[A], List.empty[A])) {
      case ((initials, finals, nodes), a) if adjacents(a).isEmpty => (initials, a :: finals, a :: nodes)
      case ((initials, finals, nodes), a) if data.values.forall(!_.exists(_ === a)) => (a :: initials, finals, a :: nodes)
      case ((initials, finals, nodes), a) => (initials, finals, a :: nodes)
    }

    val nodesSorted: List[A] = {
      val (sorted, _) = nodes.foldLeft((List.empty[A], HashSet.empty[A])) {
        case ((sorted, visited), a) => dfs(List(a), sorted, visited)(_ :: _)
      }
      sorted
    }

    def filter(f: A => Boolean): DirectedGraph[A] =
      DirectedGraph(
        data.collect { case (a, as) if f(a) => a -> as.filter(f) }
      )

    def expand[B: Eq](f: A => List[B]): DirectedGraph[B] =
      DirectedGraph(
        nodes.foldLeft(Graph.empty[B]) {
          (gr, a) =>
            f(a).foldLeft(gr) {
              (gr2, b) => gr2.updated(b, data(a).flatMap(f))
            }
        }
      )

    def map[B: Eq](f: A => B): DirectedGraph[B] =
      DirectedGraph(
        dfs(nodes, Graph.empty[B])((a, gr) => gr.updated(f(a), adjacents(a).map(f)))._1
      )

    private def dfs[B](ns: List[A], s: B, z: HashSet[A] = HashSet.empty)(f: (A, B) => B): (B, HashSet[A]) = {
      ns match {
        case Nil => (s, z)
        case x :: xs if z.contains(x) => dfs(xs, s, z)(f)
        case x :: xs =>
          val (result, visited) = dfs(adjacents(x), s, z + x)(f)
          dfs(xs, f(x, result), visited)(f)
      }
    }

    def connected(x: A, y: A): Boolean =
      fromNode(_ === x).nodes.exists(_ === y)

    def fromNode(f: A => Boolean): DirectedGraph[A] = {
      val (graph, _) = dfs(nodes.filter(f), Graph.empty[A])((a, gr) => gr.updated(a, adjacents(a)))
      DirectedGraph(graph)
    }

    override def toString = data.map {
      case (a, as) => s"$a -> ${as.mkString(", ")}"
    }.mkString("\n")
  }
  object DirectedGraph {

    def apply[A: Eq](nodes: List[A])(relation: (A, A) => Boolean): Xor[Graph.Cycle[A], DirectedGraph[A]] = {
      def loop(els: List[A], acc: Graph[A]): Xor[Graph.Cycle[A], Graph[A]] = els match {
        case Nil => Xor.right(acc)
        case x :: xs =>
          val updated = acc.updated(x, nodes.filter(s => x =!= s && relation(x, s)))
          val cycle = Graph.hasCycle(updated)
          if (cycle.nonEmpty) Xor.left[Graph.Cycle[A], Graph[A]](Graph.Cycle(cycle))
          else loop(xs, updated)
      }
      loop(nodes, Graph.empty).map(DirectedGraph(_))
    }

  }

  val ls = (1 to 10).toList
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
