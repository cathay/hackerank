package test

import org.scalatest.FunSuite

class CircularQueueTest extends FunSuite {

  test("initialize queue") {
    val queue = CircularQueue.initialize(1)

    assert(queue.maxElement == 1)
    assert(queue.elements.length == 0)
  }

  test("initialize queue with negative size") {
    assertThrows[IllegalArgumentException](CircularQueue.initialize(-1))
  }

  test("initialize queue with 0 size") {
    assertThrows[IllegalArgumentException](CircularQueue.initialize(0))
  }

  test("enqueue") {
    val queue = CircularQueue.initialize(1).enqueue(3)

    assert(queue.maxElement == 1)
    assert(queue.elements sameElements  Array(3))
  }

  test("enqueue with max size") {
    val queue = CircularQueue.initialize(1).enqueue(3)
    assertThrows[IllegalStateException](queue.enqueue(4))
  }

  test("dequeue") {
    val queue = new CircularQueue(Array(7,11,18), 4)
    val dequeue = queue.dequeue()

    assert(dequeue._1 == 7)
    assert(dequeue._2.maxElement == 4)
    assert(dequeue._2.elements sameElements Array(11,18))
  }

  test("dequeue an empty queue") {
    assertThrows[IllegalStateException](CircularQueue.initialize(1).dequeue())
  }
}
