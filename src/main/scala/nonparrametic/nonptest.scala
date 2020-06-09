package nonparrametic

import scala.io.{BufferedSource, Source}

/**
  * Created by WZZC on 2018/9/14
  **/
object nonptest {
  def main(args: Array[String]): Unit = {

    /////////////////////CoxStuart/////////////////////////////

    val source: BufferedSource = Source.fromFile(args(0))
    val data = source.getLines().map(_.toDouble)
    val coxstuer1 = nonparrametic CoxStuart (data.toSeq)
    println("p-value = " + coxstuer1)

    /////////////////////Runstest/////////////////////////////
    val run = Array(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
    val value = nonparrametic runsTest (run.toSeq)
    println(value)
    println("================")
    val wil = Array(4D, 7, 9, 21, 4, 6, 8, 11, 5, 8)
    val y = nonparrameticutils sign (wil)
    println(nonparrametic runsTest (y))


  }
}
