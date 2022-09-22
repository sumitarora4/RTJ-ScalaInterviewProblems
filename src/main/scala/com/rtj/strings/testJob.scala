package com.rtj.strings

import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

object testJob {
  def startTask(number: Int): Future[Unit] = Future {
    println(s"Starting task#$number")
    Thread.sleep(2000) // wait 2secs
    println(s"Finished task#$number")
  }

  def main(args: Array[String]): Unit = {
    println("Starting Main")
    //val tasks = Future.traverse(1 to 20)(startTask)
    println("Continuing Main")
    // waits for all tasks to complete before exiting
    //Await.result(tasks, Duration.Inf)
  }

  // ...
}