package in.olc.DSA.Searching

object BinarySearch {
  def main(args: Array[String]): Unit = {
    val arr = Array(2,4,5,7,8,9,10,23,24)
    val element=9
    val result=search(arr,element)
    println(result)
  }
  //search algorithm work if array sorted in Ascending order
  def search(arr: Array[Int], element: Int,bool:Boolean=false):Boolean={
    val mid=arr.length/2
    if(bool || arr.isEmpty){
      return bool
    }else{
      if(arr(mid)==element){
        search(arr,element,true)
      }else if(element > arr(mid)){
        search(arr.slice(mid+1,arr.length),element, bool)
      }else{
        search(arr.slice(0,mid),element, bool)
      }
    }
  }

}
