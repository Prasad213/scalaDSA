package in.olc.DSA.Searching

object linearSearch {
  def main(args: Array[String]): Unit = {
    val arr=Array(2,1,4,6,8,7,0,2)
    val element= 10
    val index=search(arr,element)
    println(index)
  }
  private def search(arr:Array[Int],element:Int):Int={
    var index:Int= 0
    while((index < arr.length) && (arr(index)!=element)){
      index+=1
    }
    if(index == arr.length){
      -1
    }else{
      index
    }
  }


}
