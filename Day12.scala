import InputReader._
import scala.collection.mutable

object Day12 extends App {
  type Vertex = String

  val START_VERTEX = "start"
  val END_VERTEX = "end"

  val ADJACENCY_LIST: Map[Vertex, List[Vertex]] = {
    var neighborMap = Map.empty[Vertex, List[Vertex]]
    inputEdges.foreach { edge =>
      val Array(srcVertex, destVertex) = edge.split("-")
      val srcNeighbors = neighborMap.getOrElse(srcVertex, List.empty[Vertex])
      val destNeighbors = neighborMap.getOrElse(destVertex, List.empty[Vertex])

      neighborMap += (srcVertex -> srcNeighbors.appended(destVertex))
      neighborMap += (destVertex -> destNeighbors.appended(srcVertex))
    }
    neighborMap
  }

  def isSmallVertex(vertex: Vertex): Boolean = vertex.forall(_.isLower)

  def isStartOrEndVertex(vertex: Vertex): Boolean =
    (vertex == START_VERTEX) || (vertex == END_VERTEX)

  def solutionToFirstHalf: Long = {
    def searchPaths(
        vertex: Vertex,
        seenSmallVertices: Set[Vertex],
        path: String
    ): Long = {
      if (vertex == END_VERTEX) {
        println(path)
        return 1
      }

      val neighbors = ADJACENCY_LIST(vertex)

      neighbors.foldLeft(0L) { case (acc, neighbor) =>
        isSmallVertex(neighbor) match {
          case true =>
            if (seenSmallVertices.contains(neighbor)) acc
            else
              acc + searchPaths(
                neighbor,
                seenSmallVertices + neighbor,
                path + "->" + neighbor
              )
          case false =>
            acc + searchPaths(
              neighbor,
              seenSmallVertices,
              path + "->" + neighbor
            )
        }
      }
    }

    searchPaths(START_VERTEX, Set(START_VERTEX), "start")
  }

  def solutionToSecondHalf: Long = {
    def searchPaths(
        vertex: Vertex,
        seenSmallVertices: Set[Vertex],
        smallCaveSeenTwice: Boolean,
        path: String
    ): Long = {
      if (vertex == END_VERTEX) {
        println(path)
        return 1
      }

      val neighbors = ADJACENCY_LIST(vertex)

      neighbors.foldLeft(0L) { case (acc, neighbor) =>
        isSmallVertex(neighbor) match {
          case true =>
            seenSmallVertices.contains(neighbor) match {
              case true =>
                if (smallCaveSeenTwice || isStartOrEndVertex(neighbor)) acc
                else
                  acc + searchPaths(
                    neighbor,
                    seenSmallVertices,
                    true,
                    path + "->" + neighbor
                  )
              case false =>
                acc + searchPaths(
                  neighbor,
                  seenSmallVertices + neighbor,
                  smallCaveSeenTwice,
                  path + "->" + neighbor
                )
            }
          case false =>
            acc + searchPaths(
              neighbor,
              seenSmallVertices,
              smallCaveSeenTwice,
              path + "->" + neighbor
            )
        }
      }
    }

    searchPaths(START_VERTEX, Set(START_VERTEX), false, "start")
  }

  lazy val inputEdges: List[String] =
    readAllLines("day-12-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)

  println(solutionToFirstHalf)
  println(solutionToSecondHalf)
}
