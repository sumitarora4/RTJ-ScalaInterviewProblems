package com.rtj.lists

import scala.::
import scala.annotation.tailrec

object RecursionProblems extends App{

  val list = List("A","B","C","D")
  val anotherList = List("x","y","z")


 def length[T](list: List[T]): Int = {

   @tailrec
    def lengthTailRec(remainingList: List[T], accum: Int): Int = {
      if (remainingList.isEmpty) accum
      else lengthTailRec(remainingList.tail, accum + 1)
    }

    lengthTailRec(list, 0)
  }


 def reverseAList[T](list: List[T]): List[T] = {

   @tailrec
   def reverseTailRec(remainingList: List[T], result: List[T] ): List[T] = {
     if(remainingList.isEmpty) result
     else reverseTailRec(remainingList.tail, remainingList.head :: result)
   }

   reverseTailRec(list, Nil)
  }


  def appendAnotherList(anotherList: List[String]): List[String] = {

    def appendTailRec(anotherList:List[String], result: List[String]): List[String] = {

      if(anotherList.isEmpty) result
      else appendTailRec(anotherList.tail, anotherList.head :: result)
    }

    appendTailRec(anotherList, list.reverse).reverse
  }


  def removeAt(index: Int): List[String] = {

    def removeTailRec(remainingList: List[String], result: List[String]): List[String] = {

      if(remainingList.isEmpty) result
      else {
        println(remainingList.take(index) +"::::" + remainingList.drop(index + 1))

        // this will remove every value which are coming on index 2 location recursively
//        removeTailRec(remainingList.tail, remainingList.take(index) ++ remainingList.drop(index + 1))

//      this will remove only one value which are on index 2 location so it is not a recursive iteration
        remainingList.take(index) ++ remainingList.drop(index + 1)
      }
    }

    removeTailRec(list, Nil)
  }



  println(length(list))
  println(reverseAList(list))
  println(appendAnotherList(anotherList))
  println(removeAt(2))


}
