package test

import java.util

object BoxStacking {

  def ableStack(box1: Box, box2: Box) = {
    box1.width > box2.width && box1.height > box2.height && box1.depth > box2.depth
  }

  def main(args: Array[String]): Unit = {
    val boxes = List(
      Box(4,4,4),
      Box(10,10,10),
      Box(9,9, 9),
      Box(10,2,5),
      Box(2,4,1000),
      Box(5,5,5),
      Box(3,3,3),
      Box(2,2,3),
      Box(8,8,8),
      Box(8,8,9),
      Box(3,5,1004),
      Box(2,4,1003)
    )

    val stackBoxes = new util.Stack[Box]
    stack(boxes, stackBoxes)
    while(!stackBoxes.isEmpty) println(stackBoxes.pop())
  }

  def stack(boxes: List[Box], stackBoxes: util.Stack[Box]): Unit = {
    println(boxes.mkString(","))
    if(boxes.size > 0) {
      var maxBox = boxes(0)
      var findMax = false
      for(i <- 1 until boxes.length) {
        if(ableStack(boxes(i), maxBox)) {
          maxBox = boxes(i)
          findMax = true
        }
        else if(ableStack(maxBox, boxes(i))) {
          findMax = true
        }
      }

      if((stackBoxes.size() == 0 && findMax) || (stackBoxes.size() > 0 && ableStack(stackBoxes.peek(), maxBox))) {
        stackBoxes.push(maxBox)
      }

      stack(boxes.filter(_ != maxBox), stackBoxes)
    }
  }
}

case class Box(width: Int, height: Int, depth: Int)

