import InputReader._

import scala.collection.mutable

object Day23A extends App {
  case class Position(row: Int, col: Int) {
    def depth: Int =
      row match {
        case 1 => 0 // hallway
        case 2 => 1 // pos1 in Room
        case 3 => 2 // pos2 in Room
      }
  }
  object Position {
    def distance(pos1: Position, pos2: Position): Long = {
      math.abs(pos1.row - pos2.row) + math.abs(pos1.col - pos2.col)
    }

    def isPathClear(
        openSpaces: Set[OpenSpace],
        pos1: Position,
        pos2: Position
    ): Boolean = {
      val (rlowerPos, rhigherPos) =
        if (pos1.row >= pos2.row) (pos2, pos1) else (pos1, pos2)
      val (clowerPos, chigherPos) =
        if (pos1.col >= pos2.col) (pos2, pos1) else (pos1, pos2)

      val verticalPositions = {
        for {
          row <- rlowerPos.row to rhigherPos.row
          nPos = Position(row, rhigherPos.col)
          if nPos != pos1
        } yield nPos
      }

      val horizontalPositions = {
        for {
          col <- clowerPos.col to chigherPos.col
          nPos = Position(rlowerPos.row, col)
          if nPos != pos1
        } yield nPos
      }

      verticalPositions.forall(pos => openSpaces.contains(OpenSpace(pos))) &&
      horizontalPositions.forall(pos => openSpaces.contains(OpenSpace(pos)))
    }
  }
  case class Wall(pos: Position)
  case class Room(pos1: Position, pos2: Position)
  object Room {
    val ROOM_A: Room = Room(Position(2, 3), Position(3, 3))
    val ROOM_B: Room = Room(Position(2, 5), Position(3, 5))
    val ROOM_C: Room = Room(Position(2, 7), Position(3, 7))
    val ROOM_D: Room = Room(Position(2, 9), Position(3, 9))
  }

  sealed trait Category
  case object Amber extends Category {
    override def toString: String = "A"
  }
  case object Bronze extends Category {
    override def toString: String = "B"
  }
  case object Copper extends Category {
    override def toString: String = "C"
  }
  case object Desert extends Category {
    override def toString: String = "D"
  }

  case class Pod(cat: Category, energy: Int, pos: Position, destRoom: Room) {
    def isHome: Boolean =
      (destRoom.pos1 == pos || destRoom.pos2 == pos)

    def depth: Int =
      pos.depth

  }

  object Pod {
    def apply(char: Char, pos: Position): Pod =
      char match {
        case 'A' => Pod(Amber, 1, pos, Room.ROOM_A)
        case 'B' => Pod(Bronze, 10, pos, Room.ROOM_B)
        case 'C' => Pod(Copper, 100, pos, Room.ROOM_C)
        case 'D' => Pod(Desert, 1000, pos, Room.ROOM_D)
      }
  }

  case class OpenSpace(pos: Position)

  case class State(
      walls: Set[Wall],
      pods: Set[Pod],
      openSpaces: Set[OpenSpace]
  ) {
    def display: Unit =
      for (i <- 0 to 4) {
        for (j <- 0 to 12) {
          val pos = Position(i, j)
          pos match {
            case p if walls.contains(Wall(p))           => print("#")
            case p if openSpaces.contains(OpenSpace(p)) => print(".")
            case _ =>
              val pod = pods.find(_.pos == pos).get
              print(pod.cat)
          }
        }
        print("\n")
      }

    // return potential next states along with the energy required to get there
    def nextStates: Map[State, Long] = {
      var nextStatesToEnergy = mutable.Map.empty[State, Long]

      def addNewState(pod: Pod, destination: Position): Unit = {
        val source = pod.pos
        val energySpent = Position.distance(source, destination) * pod.energy
        val newPods = pods - pod + pod.copy(pos = destination)
        val newOpenSpaces =
          openSpaces - OpenSpace(destination) + OpenSpace(source)
        val newState = State(walls, newPods, newOpenSpaces)
        nextStatesToEnergy(newState) = energySpent
      }

      for (currPod <- pods) {
        val destRoom = currPod.destRoom

        // if pod is in the hallway
        if (currPod.depth == 0) {
          // if destRooms first position is empty & second position is either empty or occupied by another pod of same cat, we can move in
          val firsPosIsEmpty: Boolean = {
            openSpaces.contains(OpenSpace(destRoom.pos1))
          }

          val secondPosIsEmpty: Boolean = {
            openSpaces.contains(OpenSpace(destRoom.pos2))
          }

          val secondPosIsOccupiedByPodOfSameCat: Boolean = {
            pods.exists(pod =>
              pod.cat == currPod.cat && pod.pos == destRoom.pos2
            )
          }

          if (
            firsPosIsEmpty && (secondPosIsEmpty || secondPosIsOccupiedByPodOfSameCat)
          ) {
            // if path to first pos of dest room is clear, we can make a move
            // println(currPod)
            if (Position.isPathClear(openSpaces, currPod.pos, destRoom.pos1))
              addNewState(currPod, destRoom.pos1)
          }
        } else if (currPod.depth == 1) {
          // it makes sense to go down if there is no other pod there and this is the home room
          if (
            currPod.isHome && Position.isPathClear(
              openSpaces,
              currPod.pos,
              destRoom.pos2
            )
          )
            addNewState(currPod, destRoom.pos2)

          // going to the hallway
          // open spaces in front of other rooms is not an option
          val eligibleHallwayOpenSpaces =
            openSpaces
              .filter(space => space.pos.depth == 0)
              .filterNot(space => Set(3, 5, 7, 9) contains space.pos.col)

          for (hallwaySpace <- eligibleHallwayOpenSpaces) {
            if (Position.isPathClear(openSpaces, currPod.pos, hallwaySpace.pos))
              addNewState(currPod, hallwaySpace.pos)
          }
        } else {
          // if the pod is home in the second position, don't move it
          if (!currPod.isHome) {
            // going to the hallway
            // open spaces in front of other rooms is not an option
            val eligibleHallwayOpenSpaces =
              openSpaces
                .filter(space => space.pos.depth == 0)
                .filterNot(space => Set(3, 5, 7, 9) contains space.pos.col)

            for (hallwaySpace <- eligibleHallwayOpenSpaces) {
              if (
                Position.isPathClear(openSpaces, currPod.pos, hallwaySpace.pos)
              )
                addNewState(currPod, hallwaySpace.pos)
            }
          }
        }
      }

      nextStatesToEnergy.toMap
    }

    def isTargetState: Boolean =
      pods.forall(_.isHome)
  }

  def solution(inputState: State): Long = {
    // Dijkstra's shortest path
    var seenStates: Set[State] = Set.empty[State]

    case class Record(energy: Long, state: State)

    val pq = mutable.PriorityQueue.empty[Record](
      Ordering.fromLessThan(_.energy > _.energy)
    )

    pq.enqueue(Record(0, inputState))

    var targetFound = false
    var targetRecord: Record = null

    while (!pq.isEmpty && !targetFound) {
      val Record(currEnergy, currState) = {
        while (seenStates.contains(pq.head.state))
          pq.dequeue()

        pq.dequeue()
      }

      seenStates += currState

      if (currState.isTargetState) {
        targetFound = true
        targetRecord = Record(currEnergy, currState)
      } else {
        // sort the next states by energy
        for ((nextState, energy) <- currState.nextStates.toList.sortBy(_._2)) {
          if (!seenStates.contains(nextState)) {
            val totalEnergy = currEnergy + energy
            pq.enqueue(Record(totalEnergy, nextState))
          }
        }
      }
    }

    targetRecord.state.display
    targetRecord.energy
  }

  val inputState: State = {
    val input: List[List[Char]] =
      readAllLines("day-23A-input.txt")
        .map(_.toList)

    var walls = Set.empty[Wall]
    var pods = Set.empty[Pod]
    var openSpaces = Set.empty[OpenSpace]

    for {
      (chars, rowId) <- input.zipWithIndex
      (char, colId) <- chars.zipWithIndex
    } {
      char match {
        case c if (c == '#' || c == ' ') =>
          walls += Wall(Position(rowId, colId))
        case '.' => openSpaces += OpenSpace(Position(rowId, colId))
        case c   => pods += Pod(c, Position(rowId, colId))
      }
    }

    State(walls, pods, openSpaces)
  }

  inputState.display

  println(solution(inputState))
}
