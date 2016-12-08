## graphs

Tiny library for exploring Graph implementations in Scala

### DirectedGraph
So far the only implementation is "Directed Acyclic Graph", and is implemented with "Deep First" approach for searching. The internal representation of the graph is a a `HashMap[A, Set[A]]`, where the key is the node, and the values are the nodes for which the key as an edge with.

#### Creating a `DirectedGraph`:

##### `addEdge(start: A, end: B):Xor[Graph.Cycle[A], DirectedGraph[A]]`

One way to create a DAG is starting from nothing and start adding edges.

```scala
scala> for {
     | gr1 <- DirectedGraph.empty[Int].addEdge(1,2)
     | gr2 <- gr1.addEdge(2, 4)
     | } yield gr2
res3: cats.data.Xor[common.graph.Graph.Cycle[Int],common.graph.DirectedGraph[Int]] =
Right(1 -> 2
2 -> 4)
```

In case a cycle is introduced, Xor.Left[Graph.Cycle] will be returned:

```scala
scala> for {
     | gr1 <- DirectedGraph.empty[Int].addEdge(1,2)
     | gr2 <- gr1.addEdge(2, 4)
     | gr3 <- gr2.addEdge(4, 1)
     | } yield gr3
res2: cats.data.Xor[common.graph.Graph.Cycle[Int],common.graph.DirectedGraph[Int]] = Left(Cycle(List(4, 1, 2, 4)))
```

##### `DAG(nodes: Seq[A])(relation: (A, A) => Boolean): Xor[Graph.Cycle[A], DirectedGraph[A]]`

Another way to create a `DirectedGraph` is by the `apply` method of `DAG`, which uses a list of nodes and a relationship between the nodes:

```scala
val smallerAndEven: (Int, Int) => Boolean = (a1, a2) => a1 >= a2 && a2 % 2 == 0
val gr = DAG((1 to 10))(smallerAndEven).getOrElse(DirectedGraph.empty[Int])
```

`gr` is a graph whose nodes are connected by the relation `smallerAndEven` -- i.e. a number is related with another if it's bigger or equals and the other number is even.

```scala
scala> gr.toString
res0: String =
5 -> 2, 4
10 -> 2, 4, 6, 8
1 ->
6 -> 2, 4
9 -> 2, 4, 6, 8
2 ->
7 -> 2, 4, 6
3 -> 2
8 -> 2, 4, 6
4 -> 2
```

#### Operations

##### `nodes:Set[A]`
All the nodes of this graph
```scala
scala> gr.nodes
res3: Set[Int] = Set(4, 8, 3, 7, 2, 9, 6, 1, 10, 5)
```

##### `roots:Set[A]`
All nodes without incoming edges
```scala
scala> gr.roots
res1: Set[Int] = Set(3, 7, 9, 10, 5)
```

##### `leafs:Set[A]`
All nodes without outgoing edges
```scala
scala> gr.leafs
res2: Set[Int] = Set(2, 1)
```


##### `order:List[A]`
All nodes topologically sorted
```scala
scala> gr.order
res4: List[Int] = List(5, 10, 1, 9, 7, 3, 8, 6, 4, 2)
```

##### `adjacents(a:A):Set[a]`
Nodes with an incoming edge from `a`
```scala
scala> gr.adjacents(10)
res5: Set[Int] = Set(2, 4, 6, 8)
```

##### `withRoot(f: A => Boolean):Graph[A]`
A new `DirectedGraph` with `roots` that fulfill the condition `f`
```scala
scala> gr.withRoot(_ === 10)
res6: common.graph.DirectedGraph[Int] =
10 -> 2, 4, 6, 8
6 -> 2, 4
2 ->
8 -> 2, 4, 6
4 -> 2
```


Now we can know the topological order for the graph that starts with `10`
```scala
scala> gr.withRoot(_ === 10).order
res0: List[Int] = List(10, 8, 6, 4, 2)
```

##### `connected(x: A, y: A):Boolean`
Returns `true` if there is a path from x to `y`, it doesn't matter if there is a path from `y` to `x` (it would be `false` in that case)
```scala
scala> gr.connected(8, 2)
res0: Boolean = true
scala> gr.connected(2, 8)
res1: Boolean = false
```

##### `path(x:A, y:A):List[A]`
Returns the trail of nodes from `x` to `y`
```scala
scala> gr.path(8, 4)
res0: List[Int] = List(8, 6, 4)
scala> gr.path(4, 8)
res1: List[Int] = List()
```

##### `reverse: DirectedGraph[A]`
The graph with all the edges inverted
```scala
scala> gr.reverse
res1: common.graph.DirectedGraph[Int] =
6 -> 10, 9, 7, 8
2 -> 5, 10, 6, 9, 7, 3, 8, 4
8 -> 10, 9
4 -> 5, 10, 6, 9, 7, 8
```

##### `ancestors(a: A): List[A]`
In a reversed graph, the `order` starting from `a`
```scala
scala> gr.ancestors(8)
res2: List[Int] = List(9, 10)
```

##### `map[B](f: A => B):DirectedGraph[B]`
Transforms every node of the `DirectedGraph` from `A` to `B`, preserving the structure
```scala
scala> gr.map(_ * 2)
res7: common.graph.DirectedGraph[Int] =
10 -> 4, 8
14 -> 4, 8, 12
20 -> 4, 8, 12, 16
6 -> 4
2 ->
12 -> 4, 8
18 -> 4, 8, 12, 16
16 -> 4, 8, 12
8 -> 4
4 ->
```

##### `filter(f: A => Boolean)`
Filters all elements of the `DirectedGraph` according with the predicate `f`, preserving structure
```scala
scala> gr.filter(_ % 3 >= 1)
res5: common.graph.DirectedGraph[Int] =
5 -> 2, 4
10 -> 2, 4, 8
1 ->
2 ->
7 -> 2, 4
8 -> 2, 4
4 -> 2
```


##### `collect[B: Eq](f: PartialFunction[A, B]): DirectedGraph[B]`
Similar to collect on other colletions
```scala
scala> gr.collect {
     | case n if n > 4 => n * 5
     | }
res0: common.graph.DirectedGraph[Int] =
25 ->
45 -> 30, 40
35 -> 30
50 -> 30, 40
40 -> 30
30 ->
```
