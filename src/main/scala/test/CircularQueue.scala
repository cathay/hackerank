package test

class CircularQueue(val elements: Array[Int],
                    val maxElement:Int) {

  if(elements.length > maxElement) throw new IllegalArgumentException("input elements size is larger than max size")

  def enqueue(element: Int) = elements.length == maxElement match {
    case true => throw new IllegalStateException(s"The queue has limit size of $maxElement")
    case false => CircularQueue(elements :+ element, maxElement)
  }

  def dequeue(): (Int, CircularQueue) = elements.length match {
    case 0 => throw new IllegalStateException("The queue is empty")
    case _ => (elements(0), CircularQueue(elements.drop(1), maxElement))
  }
}

object CircularQueue {

  def apply(elements: Array[Int],
            maxElement:Int) = new CircularQueue(elements, maxElement)

  def initialize(size: Int) = size <=0  match {
    case false => CircularQueue (Array(), size)
    case true => throw new IllegalArgumentException("The size of queue must be larger than 0")
  }
}
