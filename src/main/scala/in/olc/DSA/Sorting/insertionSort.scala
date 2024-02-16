package in.olc.DSA.Sorting

import scala.annotation.tailrec

object insertionSort {
  def main(args: Array[String]): Unit = {
    val arr=Array[Int](1,10,30,60,2,9,1)
    val result=sorted(arr)
    result.foreach(println)
  }
  //create part unsorted and sorted arr
  //then take each element insert in correct position in sorted arr
  @tailrec
  def sorted(unsortedArr: Array[Int], sortedArr:Array[Int]=Array[Int]()):Array[Int]={
    if(unsortedArr.isEmpty){
      sortedArr
    }else{
      val element=unsortedArr.head
      val newSortedArr=insertElement(element, sortedArr)
      sorted(unsortedArr.tail,newSortedArr)
    }
  }

  def insertElement(element: Int, sortedArr: Array[Int]): Array[Int] = {
    var i = 0
    if(sortedArr.isEmpty){
      sortedArr :+ element
    }else{
      while ((i <= sortedArr.length - 1) && (sortedArr(i) < element)) {
        i += 1
      }
      val parted = sortedArr.splitAt(i)
      parted(0) :+ element :++ parted(1)
    }
  }
}
