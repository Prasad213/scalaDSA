package in.olc.DSA.Sorting

import scala.annotation.tailrec

object selectionSort {
  def main(args: Array[String]): Unit = {
    val arr = Array[Int](1, 10, 30, 60, 2, 9, 1)
    val result = sorted(arr)
    arr.foreach(println)
  }
  def sorted(arr: Array[Int]): Array[Int] = {
    for (i <- arr.indices) {
      var min = i
      for (j <- (i + 1) until arr.length) {
        if (arr(min) > arr(j)) {
          min = j
        }
      }
      val temp = arr(i)
      arr.update(i, arr(min))
      arr.update(min, temp)
    }
    arr
  }
}
