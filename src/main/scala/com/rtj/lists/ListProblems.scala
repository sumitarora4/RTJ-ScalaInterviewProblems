package com.rtj.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] { //covariant type

  /**
  * Standard Functions
   * */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
//  override def prepend[S >: Nothing](element: S): RList[S] = new Cons(element, this)
  // prepend method also denotes as ::
  def ::[S >: T](element: S): RList[S] = new ::(element, this)

  /**
   * Easy Problems
  * */

  //get an element of a given index
  def apply(index: Int): T

  // size of the list
  def length: Int

  // reverse the List
  def reverse: RList[T]

  // concatenate another list to this one
  def ++[S >:T](RList: RList[S]): RList[S]

  // remove an element at the index position and return new list
  def removeAt(index: Int): RList[T]

}

case object RNil extends RList[Nothing]{
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  // no use here as perpend method is already implemented in abstract class so comment from object
//  override def prepend[S >: Nothing](element: S): RList[S] = new Cons(element, this)

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil

  // append another list
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element at given index
  override def removeAt(index: Int): RList[Nothing] = throw new NoSuchElementException

}

// Cons Case Class which is also known as ::
case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T]{
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String):String ={
      if (remaining.isEmpty) result
      else if(remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail,s"$result${remaining.head}, ")
    }
    "["+ toStringTailRec(this, "") + "]"
  }


  // no use here as perpend method is already implemented in abstract class so comment it  from class
//  override def prepend[S >: T](element: S): RList[S] = new Cons(element, this)

  override def apply(index: Int): T = {

//     complexity of this apply method algo
//    O(min(N, index))
    @tailrec
    def applyTailRec( remaining: RList[T], currentIndex: Int): T = {
      if(index == currentIndex) remaining.head
      else applyTailRec(remaining.tail, currentIndex +1 )
    }

    // now check for if index is < 0
    if(index < 0) throw new NoSuchElementException
    else applyTailRec(this, 0)
  }


  override def length: Int = {

    /*
    * [1,2,3,4,5].length = lengthTailRec([1,2,3,4,5],0)
    * = lengthTailRec([2,3,4,5],1)
    * = lengthTailRec([3,4,5],2)
    * = lengthTailRec([4,5],3)
    * = lengthTailRec([5],5)
    * = lengthTailRec([],5)
    * = 5
    *
    * Complexity = O(N)
    * */
    @tailrec
    def lengthTailRec(remaining: RList[T], accumulator: Int) : Int = {
      if (remaining.isEmpty) accumulator
      else lengthTailRec(remaining.tail, accumulator + 1)
    }
    lengthTailRec(this, 0)
  }

  override def reverse: RList[T] = {

    /*
   * [1,2,3,4,5].reverse = reverseTailRec([1,2,3,4,5],RNil)
   * = reverseTailRec([2,3,4,5],[1])
   * = reverseTailRec([3,4,5],[2,1])
   * = reverseTailRec([4,5],[3,2,1])
   * = reverseTailRec([5],[4,3,2,1])
   * = reverseTailRec([],[5,4,3,2,1])
   * = [5,4,3,2,1]
   *
   * Complexity = O(N)
   * */
    @tailrec
    def reverseTailRec(remaining: RList[T], result: RList[T]): RList[T] = {

      if (remaining.isEmpty) result
      else reverseTailRec(remaining.tail, remaining.head :: result)
    }

    reverseTailRec(this, RNil)

  }


  // append another list
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {

    /*
    * Complexity: O(M) + O(M + N) = O(M + N)
    * length of this list = N
    * length of anotherList = M
    * */
    def concatTailRec(anotherList: RList[S], result: RList[S]): RList[S] = {
      if (anotherList.isEmpty) result
      else concatTailRec(anotherList.tail, anotherList.head :: result)
    }
    concatTailRec(anotherList, this.reverse).reverse
  }

  //remove an element at a given index

  override def removeAt(index: Int): RList[T] = {

//    def removeAtTailRec(remainingList: RList[T]): RList[T] = {
//
//      if(remainingList.isEmpty) remainingList
//      else (remainingList.toString.take(index) ++ remainingList.toString.drop(index +1))
//
//
//    }
//    removeAtTailRec(this)
    ???
  }

}

object ListProblems extends App{

//  val aList = ::(1, ::(2, ::(3, RNil)))
  val aList = 1 :: 2 :: 3 :: RNil // equivalent to RNil.::(3).::(2).::(1)
  println(aList)

  val aLargeList = 4 :: 5 :: RNil
  // here apply means get value
  println(aList.apply(2))
//  println(aList.apply(-1))
//  println(aList.apply(30))

//  Test Length of List
  println("length:"+aList.length)

//  Test Reverse
  println("reverse:" + aList.reverse)

//  Test Concatenate
  println(aList ++ aLargeList)

  // Test Remove
  println("removed List= " + aList.removeAt(1))
}
