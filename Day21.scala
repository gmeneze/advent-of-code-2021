import InputReader._

object Day21 extends App {
  case class Player(pos: Int, score: Int) {
    def won: Boolean = score >= 21
    def getNext(diceTotal: Int): Player = {
      val newPos = {
        val sum = (pos + diceTotal)
        if (sum % 10 == 0) 10
        else sum % 10
      }
      Player(newPos, score + newPos)
    }
  }

  case class DetDice(value: Int, rollCount: Int) {
    def getNext: DetDice = {
      if (value == 100) DetDice(1, rollCount + 1)
      else DetDice(value + 1, rollCount + 1)
    }
  }

  case object DiracDice {
    // we only care about sum
    def getAllThreeRollCombinations: List[Int] = {
      val allCombs = for {
        i <- 1 to 3
        j <- 1 to 3
        k <- 1 to 3
      } yield {
        i + j + k
      }

      allCombs.toList
    }
  }

  def solutionToFirstHalf(players: List[Player]): Long = {
    case class RollDiceThriceResult(dice: DetDice, sum: Int)
    def rollDiceThrice(dice: DetDice): RollDiceThriceResult = {
      var currDice = dice
      var sum = 0

      (1 to 3).foreach { _ =>
        currDice = currDice.getNext
        sum += currDice.value
      }

      RollDiceThriceResult(currDice, sum)
    }

    var winnerFound = false
    var currDice = DetDice(0, 0)
    var currPlayers = players

    while (!winnerFound) {
      currPlayers = currPlayers.map { case oldPlayer =>
        winnerFound match {
          case true => oldPlayer
          case false =>
            val rollResult = rollDiceThrice(currDice)
            currDice = rollResult.dice
            oldPlayer.getNext(rollResult.sum)

            val newPlayer = oldPlayer.getNext(rollResult.sum)
            if (newPlayer.won) winnerFound = true
            newPlayer
        }
      }
    }

    val losingPlayer: Player = currPlayers.minBy(_.score)

    losingPlayer.score.toLong * currDice.rollCount
  }

  def solutionToSecondHalf(player1: Player, player2: Player): Long = {
    case class CacheKey(player1: Player, player2: Player)
    case class CacheValue(player1Wins: Long, player2Wins: Long)

    var cache: Map[CacheKey, CacheValue] = Map.empty[CacheKey, CacheValue]

    def loop(player1: Player, player2: Player): CacheValue = {
      if (player1.won) return CacheValue(1, 0)
      if (player2.won) return CacheValue(0, 1)

      val cacheKey: CacheKey = CacheKey(player1, player2)
      if (cache.contains(cacheKey)) return cache(cacheKey)

      val allPossibleCacheValues: IndexedSeq[CacheValue] =
        for {
          i <- 1 to 3
          j <- 1 to 3
          k <- 1 to 3
        } yield {
          val sum = i + j + k
          val newPlayer1 = player1.getNext(sum)
          val CacheValue(p2Wins, p1Wins) = loop(player2, newPlayer1)
          CacheValue(p1Wins, p2Wins)
        }

      val cacheValue: CacheValue =
        allPossibleCacheValues.reduce {
          case (
                CacheValue(p1WinsLeft, p2WinsLeft),
                CacheValue(p1WinsRight, p2WinsRight)
              ) =>
            CacheValue(p1WinsLeft + p1WinsRight, p2WinsLeft + p2WinsRight)
        }

      cache += (cacheKey -> cacheValue)
      cacheValue
    }

    val CacheValue(player1Wins, player2Wins) = loop(player1, player2)
    if (player1Wins > player2Wins) player1Wins
    else player2Wins
  }

  val players = List(Player(5, 0), Player(6, 0))
  println(solutionToFirstHalf(players))
  println(solutionToSecondHalf(Player(5, 0), Player(6, 0)))
}
