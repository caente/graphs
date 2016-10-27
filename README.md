## graphs

Tiny library for exploring Graph implementations in Scala

### DirectedGraph
So far the only implementation is "Directed Acyclic Graph", and is implemented with "Deep First" approach for searching. The internal representation of the graph is a a `HashMap[A, List[A]]`, where the key is the node, and the values are the nodes for which the key as an edge with.

#### Creating a `DirectedGraph`: 
For now the only way to create a `DirectedGraph` is by the `apply` method of its companion object:
```
object DirectedGraph{
  def apply[A: Eq](nodes: List[A])(relation: (A, A) => Boolean): Xor[Graph.Cycle[A], DirectedGraph[A]]
}
```

```scala
val ls = (1 to 10).toList
val smallerAndEven: (Int, Int) => Boolean = (a1, a2) => a1 >= a2 && a2 % 2 == 0 
val g = DirectedGraph(ls)(relation)
val gr = g.getOrElse(throw new Exception())
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

##### `nodes:List[A]` 
All the nodes of this graph
```scala
scala> gr.nodes
res3: List[Int] = List(4, 8, 3, 7, 2, 9, 6, 1, 10, 5)
```

##### `initials:List[A]` 
All nodes without incoming edges
```scala
scala> gr.initials
res1: List[Int] = List(3, 7, 9, 10, 5)
```

##### `finals:List[A]` 
All nodes without outgoing edges
```scala
scala> gr.finals
res2: List[Int] = List(2, 1)
```


##### `nodesSorted:List[A]`
All nodes topologically sorted
```scala
scala> gr.nodesSorted
res4: List[Int] = List(5, 10, 1, 9, 7, 3, 8, 6, 4, 2)
```

##### `adjacents(a:A):List[a]`
Nodes with an incoming edge from `a`
```scala
scala> gr.adjacents(10)
res5: List[Int] = List(2, 4, 6, 8)
```

##### `fromNode(f: A => Boolean):Graph[A]`
A new `DirectedGraph` with `initials` that fulfill the condition `f`
```scala
scala> gr.fromNode(_ === 10)
res6: common.graph.DirectedGraph[Int] =
10 -> 2, 4, 6, 8
6 -> 2, 4
2 ->
8 -> 2, 4, 6
4 -> 2
```

Now we can know the topological order for the graph that starts with `10`
```scala
scala> gr.fromNode(_ === 10).nodesSorted
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


