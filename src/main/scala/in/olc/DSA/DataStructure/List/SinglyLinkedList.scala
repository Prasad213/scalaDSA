package in.olc.DSA.DataStructure.List

object SinglyLinkedList {
  def main(args: Array[String]): Unit = {
    val linkedList=LinkedList[Int]()
    linkedList.append(10)
    linkedList.append(20)
    linkedList.append(30)
    linkedList.append(40)
    println(linkedList.search(2).value)
    linkedList.deleteElement(2)
    linkedList.prepend(50)
    println(linkedList.search(2).value)
    println(linkedList.length())
    println(linkedList.detectLoop())
    linkedList.insertAtIndex(2,60)
  }
  case class Node[T](value: T, var next: Option[Node[T]] = None) //next value is var its needs to change in each Node
  class LinkedList[T]{
    private var head: Option[Node[T]] = None
    private var lastVisited:Option[Node[T]]=None
    def append(elem:T):Unit={
      val newNode=Node(elem)
      if(head.isEmpty){
        head=Some(newNode)
        lastVisited=Some(newNode)
      }else{
        lastVisited.get.next=Some(newNode)
        lastVisited=Some(newNode)
      }
    }
    def prepend(elem:T):Unit={
      val newNode=Node(elem)
      if (head.isEmpty) {
        head = Some(newNode)
        lastVisited = Some(newNode)
      } else{
        newNode.next=head
        head=Some(newNode)
      }
    }
    def search(index:Int,node:Option[Node[T]]=head):Node[T]={
      if(index<0){
        throw new IndexOutOfBoundsException("Index must be greater than Zero")
      }else if(index == 0){
        node.get
      }else{
        search(index - 1,node.get.next)
      }
    }
    def insertAtIndex(index:Int,elem:T):Unit={
      val newNode=Node(elem)
      //if(index<0) throw new IndexOutOfBoundsException("Index must be greater than Zero")
      if(head.isEmpty || index == 0){
        prepend(elem)
      }else{
        val currentNodeAtIndex=search(index)
        val newNode=Node(elem,currentNodeAtIndex.next)
        currentNodeAtIndex.next=Some(newNode)
      }
    }
    def length():Int={
      var currentNode=head
      var count=0
      while(currentNode.get.next.isDefined){
        currentNode=currentNode.get.next
        count+=1
      }
      count
    }

    def middleElement(): T = {
      /*
      val len = this.length()
      val mid=(len/2) + 1
      search(mid)
      */
      //Double Pointer Technique to optimize solution
      var singleStep = head //init sigle Step marker
      var doubleStep = head //init double step marker
      while (singleStep.get.next.isDefined && doubleStep.get.next.get.next.isDefined) {
        singleStep = singleStep.get.next
        doubleStep = doubleStep.get.next.get.next
      }
      doubleStep.get.value

    }
    def deleteElement(index:Int):Unit={
      var currentElement=head
      var counter=0
      while(currentElement.get.next.isDefined && (counter!=index-1)){
        currentElement=currentElement.get.next
        counter+=1
      }
      currentElement.get.next=currentElement.get.next.get.next
      if(currentElement.get.next.isEmpty){
        lastVisited=currentElement
      }
    }
    def detectLoop():Boolean={
      val currentNode = head
      val objectMap:List[Node[Int]]=List()
      while(currentNode.get.next.isDefined && objectMap.contains(currentNode.get)){
        objectMap :+ currentNode.get
      }
      if(objectMap.contains(currentNode.get)){
        true
      }else{
        false
      }
    }

//    def reverse():Unit={
//      ???
//    }
  }
}
