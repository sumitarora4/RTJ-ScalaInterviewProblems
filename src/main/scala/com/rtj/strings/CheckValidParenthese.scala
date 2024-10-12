package com.rtj.strings

object CheckValidParenthese extends App{

def hadValidParens(str: String): Boolean = {

  def validParensTailRec(remaining: String, openParens: Int): Boolean = {

    if (remaining.isEmpty) openParens == 0
    else if(openParens ==0 && remaining.head == ')') false
    else if(remaining.head=='(') validParensTailRec(remaining.tail, openParens + 1)
    else validParensTailRec(remaining.tail, openParens -1 )

    /*remaining.length match{
      case 0 => openParens == 0
      case _ => (openParens == 0 && remaining.head == ')') false
      case (remaining.head=='(') => validParensTailRec(remaining.tail, openParens + 1)
      case _ =>  validParensTailRec(remaining.tail, openParens -1 )

    }*/
  }

  validParensTailRec(str, 0)
}


  def testValidParens() = {

    println(hadValidParens("()"))
    println(hadValidParens("(()"))
    println(hadValidParens("())"))
    println(hadValidParens("(())"))

  }

  testValidParens()

}
