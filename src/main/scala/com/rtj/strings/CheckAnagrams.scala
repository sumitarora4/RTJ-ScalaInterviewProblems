package com.rtj.strings

import scala.annotation.tailrec

object CheckAnagrams {

  def main(args: Array[String]): Unit = {

    def checkAnagrams(str1: String, str2: String): Boolean = {

// 1) Naive Solution
      if(str1.length != str2.length) false
      else{
        str1.sorted == str2.sorted
      }

//2) using previous CountCharacters.scala file function (tailRecursion)
      def countTheCharacters(str: String) = {
        @tailrec
        def countTheCharactersTailRec(remaining: String, acc: Map[Char, Int]):Map[Char, Int] ={
          if (remaining.isEmpty) acc
          else if(acc.contains(remaining.head)) {
            val currentOccurance = acc(remaining.head)
            countTheCharactersTailRec(remaining.tail, acc + (remaining.head -> (currentOccurance +1)))
          }
          else
            countTheCharactersTailRec(remaining.tail, acc + (remaining.head -> 1))
        }

        countTheCharactersTailRec(str, Map())
      }

      if(str1.length != str2.length) false
      else{
        countTheCharacters(str1) == countTheCharacters(str2)
      }

    }

    println(checkAnagrams("sumit", "timus"))
    println(checkAnagrams("sumit", "arora"))



  }
}
