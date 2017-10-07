package algorithms.graph

import java.util.Scanner

import GraphThay._

import scala.collection.mutable.ListBuffer

object GraphThay {
  type Vertex=Int
  type Graph=Map[Vertex,Stream[Vertex]]
}

/**
  * Using stream is slower than list => still need to figure out why
  */
object RoadAndLibrary {

  def main(args: Array[String]): Unit = {

      println(calculateCost(3, 3, 2, 1,
          List((1,2), (3,1), (2,3)).toStream
        ))

    val stream = (1 to 10).toStream
    println(stream)
    println(stream.groupBy(t => t))
//        println(calculateCost(6, 6, 2, 5,
//          List((1,3), (3,4), (2,4), (1,2), (2,3), (5,6))
//        ))

//    val sc = new Scanner(System.in)
//    val q = sc.nextInt
//    var a0 = 0
//    while (a0 < q) {
//      val n = sc.nextInt
//      val m = sc.nextInt
//      val x = sc.nextLong
//      val y = sc.nextLong
//      var a1 = 0
//      var roads = new ListBuffer[(Int, Int)]
//      while (a1 < m) {
//        val city_1 = sc.nextInt
//        val city_2 = sc.nextInt
//        roads += Tuple2(city_1, city_2)
//        a1 += 1
//      }
//      println(calculateCost(n,m, x, y, roads.toList))
//      a0 += 1
//    }
  }

  /**
    * If the cost of a road is >= the cost of a library, just build a library at each node.
    *  Otherwise, get the number of nodes (x) in each connected component.
    *  Put 1 library in each component, and the total per component cost is (x-1) (a road to connect to each node in the component) * cost of a road + cost of one library.
    */
  def calculateCost(
                     numberOfCities: Int,
                     totalRoads: Int,
                     costOfLibrary: Long,
                     costOfRoad: Long,
                     roads:Stream[(Int, Int)]) = costOfRoad >= costOfLibrary match {
    case true => numberOfCities * costOfLibrary
    case false => {

      val initialRoadStream:Stream[(Int, Stream[Int])] = (0 until numberOfCities).toStream.map(x => (x, Stream.empty))
      val inputRoadStream:Stream[(Int, Stream[Int])] = (roads.map(_.swap) ++ roads)
        .groupBy(p => p._1).toStream
        .map(p => (p._1 - 1, p._2.map(p => p._2 - 1)))

      val graph = initialRoadStream.toMap ++ inputRoadStream.toMap

      //graph.foreach(println)
      val connectedComponents = new ConnectedComponents(graph).getConnectedComponents
      connectedComponents
        .map(p => costOfLibrary + (p._2.size - 1)*costOfRoad)
        .reduce(_ + _)
      //0
    }
  }
}

class ConnectedComponents(graph: Graph) {

  private val marked = Array.fill(graph.size)(false)
  private var count = 0

  private var connectedComponents:Map[Vertex,Set[Vertex]] = Map()

  for(vertex <- 0 until graph.size) {
    if(!marked(vertex)) {
      connectedComponents = connectedComponents + (vertex -> dfs(graph, vertex))
      count += 1
    }
  }

  def dfs(graph: Graph, vertex: Vertex): Set[Vertex] = {
    var components = Set(vertex)

    def dfs0(graph: Graph, vertex: Vertex): Set[Vertex] = {
      marked(vertex) = true
      for (w:Vertex <- graph(vertex).toSet) {
        if (!marked(w)) {
          components  = components + w
          dfs0(graph, w)
        }
      }

      components
    }

    dfs0(graph, vertex)
  }

  def getConnectedComponents = {
    //println("......")
    //println(connectedComponents.mkString(","))
    connectedComponents
  }
  def getCount() = count
}

