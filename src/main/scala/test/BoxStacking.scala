package test

import java.util

import scala.collection.mutable.ListBuffer

object BoxStacking {

  def ableStack(box1: Box, box2: Box) = {
    box1.width > box2.width && box1.height > box2.height && box1.depth > box2.depth
  }

  def main(args: Array[String]): Unit = {
    val boxes = List(
      Box(10,10,10),
      Box(9,9, 9),
      Box(4,4,4),
      Box(10,2,5),
      Box(2,4,1000),
      Box(5,5,5),
      Box(3,3,3),
      Box(8,8,8)
    )

    stack(boxes)
  }

  def stack(boxes: List[Box]) = {
    var maxBox: Box = getMaxBox(boxes)
    var reduceBoxes: List[Box] = boxes.filter(_ != maxBox)
    val stack = new util.Stack[Box]

    while(!reduceBoxes.isEmpty) {
      stack.push(maxBox)
      maxBox = getMaxBox(reduceBoxes)
      reduceBoxes = reduceBoxes.filter(_ != maxBox)
    }

    while(!stack.isEmpty) {
      println(stack.pop())
    }
  }

  def getMaxBox(boxes: List[Box]): Box = {
    boxes.sortWith(ableStack(_, _))(0)
  }
}

case class Box(width: Int, height: Int, depth: Int)

