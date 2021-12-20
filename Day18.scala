import InputReader._

import scala.annotation.tailrec
import scala.collection.mutable

object Day18 extends App {
  sealed trait Tree(val id: Int)
  case class Leaf(value: Int, override val id: Int) extends Tree(id) {
    override def toString: String = s"$value"
    override def equals(that: Any): Boolean =
      that match {
        case that: Leaf =>
          that.id == this.id
        case _ => false
      }
  }
  case class Branch(left: Tree, right: Tree, override val id: Int)
      extends Tree(id) {
    override def toString: String = s"[$left,$right]"
    override def equals(that: Any): Boolean =
      that match {
        case that: Branch =>
          that.id == this.id
        case _ => false
      }
  }

  var MAX_BRANCH_ID = 0
  def getBranchId: Int = {
    MAX_BRANCH_ID += 1
    MAX_BRANCH_ID
  }

  object Tree {
    def updateTree(tree: Tree, oldValue: Tree, newValue: Tree): Tree = {
      if (tree == oldValue) newValue
      else {
        tree match {
          case l: Leaf => l
          case b: Branch =>
            val leftSubTree = updateTree(b.left, oldValue, newValue)
            val rightSubTree = updateTree(b.right, oldValue, newValue)
            val id =
              if (leftSubTree == b.left && rightSubTree == b.right) b.id
              else getBranchId

            Branch(leftSubTree, rightSubTree, id)
        }
      }
    }

    def fromString(str: String): Tree = {
      var chars: List[Char] = str.toList.filterNot(_ == ']')

      // [[6[15,3[2,0
      def loop: Tree = {
        val currChar = chars.head

        currChar match {
          case '[' =>
            chars = chars.tail
            Branch(loop, loop, getBranchId)

          case c =>
            val charsUntilCommaOrClosingBrace =
              chars.takeWhile(c => !(Set('[', ',') contains c))
            chars = chars.dropWhile(c => !(Set('[', ',') contains c))
            if (chars.headOption == Some(',')) chars = chars.tail
            Leaf(charsUntilCommaOrClosingBrace.mkString.toInt, getBranchId)
        }
      }

      loop
    }

    def split(tree: Tree): Tree = {
      def findLeafToSplit(tree: Tree): Option[Leaf] = {
        tree match {
          case l: Leaf if l.value >= 10 => Some(l)
          case l: Leaf                  => None
          case b: Branch =>
            val leftResult = findLeafToSplit(b.left)
            if (leftResult == None) findLeafToSplit(b.right)
            else leftResult
        }
      }

      val treeUpdatedO = findLeafToSplit(tree).map {
        case leaf @ Leaf(value, id) =>
          val left = value / 2
          val right = value - left
          val newBranch = Branch(
            Leaf(left, getBranchId),
            Leaf(right, getBranchId),
            getBranchId
          )
          updateTree(tree, leaf, newBranch)
      }

      treeUpdatedO.getOrElse(tree)
    }

    def explode(tree: Tree): Tree = {
      case class Result(branch: Branch, parents: List[Branch])
      def findBranchToExplode(
          tree: Tree,
          parents: List[Branch]
      ): Option[Result] = {
        tree match {
          case l: Leaf => None
          case branch: Branch =>
            if (parents.size == 4) Some(Result(branch, parents))
            else {
              findBranchToExplode(
                branch.left,
                parents.prepended(branch)
              ) match {
                case Some(result) => Some(result)
                case None =>
                  findBranchToExplode(branch.right, parents.prepended(branch))
              }
            }
        }
      }

      @tailrec
      def getRightmostLeaf(tree: Tree): Leaf = {
        tree match {
          case l: Leaf   => l
          case b: Branch => getRightmostLeaf(b.right)
        }
      }

      @tailrec
      def getLeftmostLeaf(tree: Tree): Leaf = {
        tree match {
          case l: Leaf   => l
          case b: Branch => getLeftmostLeaf(b.left)
        }
      }

      val branchToExplodeO: Option[Result] =
        findBranchToExplode(tree, List.empty[Branch])

      val treeUpdatedO: Option[Tree] =
        branchToExplodeO.map { case Result(branch, parents) =>
          val leftLeafO: Option[Leaf] = {
            var currBranch: Branch = branch
            var currParents: List[Branch] = parents
            while (
              currParents.headOption.map(_.left.id) == Some(currBranch.id)
            ) {
              currBranch = currParents.head
              currParents = currParents.tail
            }

            currParents.headOption.map { parent =>
              getRightmostLeaf(parent.left)
            }
          }

          val rightLeafO: Option[Leaf] = {
            var currBranch: Branch = branch
            var currParents: List[Branch] = parents
            while (
              currParents.headOption.map(_.right.id) == Some(currBranch.id)
            ) {
              currBranch = currParents.head
              currParents = currParents.tail
            }

            currParents.headOption.map { parent =>
              getLeftmostLeaf(parent.right)
            }
          }

          def updateLeftLeaf(tree: Tree): Tree = {
            leftLeafO
              .map { leaf =>
                val Branch(Leaf(leftVal, _), _, _) = branch
                val newLeaf = Leaf(leaf.value + leftVal, getBranchId)
                updateTree(tree, leaf, newLeaf)
              }
              .getOrElse(tree)
          }

          def updateRightLeaf(tree: Tree): Tree = {
            rightLeafO
              .map { leaf =>
                val Branch(_, Leaf(rightVal, _), _) = branch
                val newLeaf = Leaf(leaf.value + rightVal, getBranchId)
                updateTree(tree, leaf, newLeaf)
              }
              .getOrElse(tree)
          }

          def updateBranch(tree: Tree): Tree = {
            val newLeaf = Leaf(0, getBranchId)
            updateTree(tree, branch, newLeaf)
          }

          val allOps: Tree => Tree =
            updateBranch _ andThen updateLeftLeaf andThen updateRightLeaf

          allOps(tree)
        }

      treeUpdatedO.getOrElse(tree)
    }

    def getMagnitude(tree: Tree): Long = {
      tree match {
        case Leaf(value, _)    => value
        case Branch(t1, t2, _) => 3L * getMagnitude(t1) + 2L * getMagnitude(t2)
      }
    }

    def add(t1: Tree, t2: Tree): Tree =
      Branch(t1, t2, getBranchId)
  }

  def reduceTree(tree: Tree): Tree = {
    val explodedTree = Tree.explode(tree)
    def didTreeChange(before: Tree, after: Tree): Boolean = before != after
    didTreeChange(tree, explodedTree) match {
      case true =>
        reduceTree(explodedTree)
      case false =>
        val splitTree = Tree.split(tree)
        didTreeChange(tree, splitTree) match {
          case true  => reduceTree(splitTree)
          case false => tree
        }
    }
  }

  def solutionToFirstHalf(trees: List[Tree]): Long = {
    val resultTree: Tree =
      trees.reduce { case (t1, t2) =>
        val tree = Tree.add(t1, t2)
        reduceTree(tree)
      }

    Tree.getMagnitude(resultTree)
  }

  def solutionToSecondHalf(trees: List[Tree]): Long = {
    val magnitudes: List[Long] =
      for {
        t1 <- trees
        t2 <- trees
        if t1 != t2
      } yield {
        val newTree = Tree.add(t1, t2)
        val ops: Tree => Long = reduceTree _ andThen Tree.getMagnitude _
        ops(newTree)
      }

    magnitudes.max
  }

  val inputTrees: List[Tree] =
    readAllLines("day-18-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(Tree.fromString(_))

  println(solutionToFirstHalf(inputTrees))
  println(solutionToSecondHalf(inputTrees))
}
