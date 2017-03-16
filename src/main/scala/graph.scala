package common

import org.joda.time._
import collection.immutable.{ HashMap, HashSet }

import scalaz._, Scalaz._
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._

package object graph {
  sealed trait Graph[+N, +G]
  case class Node[N, G](node: N, graph: G) extends Graph[N, G]
  case object Empty extends Graph[Nothing, Nothing]
  object Graph {
    def empty[N, G]: Graph[N, G] = Empty
  }

  implicit def graphBiFunctor[N, G] = new scalaz.Bifunctor[Graph] {
    def bimap[N, G, C, D](fa: Graph[N, G])(f: N => C, g: G => D): Graph[C, D] =
      fa match {
        case Node(n, gr) => Node(f(n), g(gr))
        case Empty => Empty
      }
  }

  implicit def graphFunctor[N](implicit BF: scalaz.Bifunctor[Graph]): Functor[Graph[N, ?]] = BF.rightFunctor

  def exists[N](f: N => Boolean): Algebra[Graph[N, ?], Boolean] = {
    case Node(n, _) => f(n)
    case Empty => false
  }

  def gr[G](implicit G: Corecursive.Aux[G, Graph[Int, ?]]): G =
    G.embed(
      Node(
        node = 1,
        graph = G.embed(Node(
          node = 2,
          graph = G.embed(Empty)
        ))
      )
    )

  def existsInGraph[G, N](g: G)(f: N => Boolean)(implicit G: Recursive.Aux[G, Graph[N, ?]]): Boolean =
    G.cata(g)(exists(f))

  def main(args: Array[String]): Unit = {

    assert(existsInGraph(gr[Fix[Graph[Int, ?]]])((i: Int) => i == 3))
  }

  //def dfs[N, Z](z: Z)(f: (Z, N) => Z): Algebra[Graph[N, ?], Z] = {
  //  case Empty => z
  //  case EmptyNode(n) => f(z, n)
  //  case Node(n, adjacents, g) => z
  //}
}
/*

private def fold[B](ns: Set[A], vs: HashSet[A] = HashSet.empty)(z: B)(f: (A, B) => B): (B, HashSet[A]) =
      if (ns.isEmpty) (z, vs)
      else if (vs.contains(ns.head)) fold(ns.tail, vs)(z)(f)
      else {
        val x = ns.head
        val xs = ns.tail
        val (result, visited) = fold(adjacents(x), vs + x)(z)(f)
        fold(xs.toSet, visited)(f(x, result))(f)
      }

package object graph {

  type Graph[A] = HashMap[A, Set[A]]
  type DAG[A] = Xor[Graph.Cycle[A], DirectedGraph[A]]

  object Graph {
    case class Cycle[A](values: List[A])

    def empty[A] = HashMap.empty[A, Set[A]]

  }

  object DirectedGraph {

    def empty[A: Eq] = unsafe(Graph.empty[A])

    def apply[A: Eq](nodes: Seq[A])(relation: (A, A) => Boolean): DirectedGraph[A] =
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
              (gr2, b) => gr2.updated(b, adjacents(a).flatMap(n => f(n)))
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

    def ancestors(a: A): List[A] =
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
*/
