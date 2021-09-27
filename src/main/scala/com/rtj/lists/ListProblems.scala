package com.rtj.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] { //covariant type
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

//  override def prepend[S >: Nothing](element: S): RList[S] = new Cons(element, this)
  def ::[S >: T](element: S): RList[S] = new ::(element, this)

  def apply(index: Int): T

}

case object RNil extends RList[Nothing]{
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  // no use here as perpend method is already implemented in abstract class so comment from object
//  override def prepend[S >: Nothing](element: S): RList[S] = new Cons(element, this)

  override def apply(index: Int): Nothing = throw new NoSuchElementException

}


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


  // no use here as perpend method is already implemented in abstract class so comment from class
//  override def prepend[S >: T](element: S): RList[S] = new Cons(element, this)

  override def apply(index: Int): T = {

    @tailrec
    def applyTailRec( remaining: RList[T], currentIndex: Int): T = {
      if(index == currentIndex) remaining.head
      else applyTailRec(remaining.tail, currentIndex +1 )
    }

    // now check for if index is < 0
    if(index < 0) throw new NoSuchElementException
    else applyTailRec(this, 0)
  }

}

object ListProblems extends App{

//  val aList = ::(1, ::(2, ::(3, RNil)))
  val aList = 1 :: 2 :: 3 :: 4:: RNil // equivalent to RNil.::(3).::(2).::(1)
  println(aList)

  println(aList.apply(2))
  println(aList.apply(-1))
//  println(aList.apply(30))

  
}
