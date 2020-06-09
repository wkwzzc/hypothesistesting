package nonparrametic

//import breeze.linalg.{DenseVector, rank}


import breeze.stats.distributions.{Binomial, Gaussian}

import scala.collection.mutable.ListBuffer

/**
  * Created by WZZC on 2018/7/25
  *
  **/
object nonparrameticutils {


  /**
    * 中位数算法
    *
    * @param serise
    * @return
    */
  def mid(serise: Seq[Double]): Double = {
    val sorted = serise.sorted
    val len = sorted.length
    if (len % 2 != 0) {
      sorted(len / 2 + 1)
    } else {
      (sorted(len / 2) + sorted(len / 2 - 1)) / 2
    }
  }


  /**
    * 平均值
    *
    * @param serise
    * @return
    */
  def mean(serise: Seq[Double]) = {
    require(serise.length > 0)
    serise.sum / serise.size
  }

  /**
    * 求秩
    *
    * @param serise
    * @return
    */
  def rank(serise: Seq[Double]) = {
    val lr = new ListBuffer[(Double, Int)]()
    val sorted = serise.sorted
    for (i <- 0 until sorted.size) {
      lr.append((sorted(i), i + 1))
    }
    lr.groupBy(_._1)
      .mapValues(tp => {
        val rmean = mean(tp.map(_._2.toDouble))
        val rcount = tp.length
        (rmean, rcount)
      })
  }


  /**
    * 符号计算
    *
    * @param series
    * @return
    */
  def sign(series: Seq[Double]) = {
    val median = nonparrameticutils.mid(series)
    series.map { d =>
      if (d - median > 0) 1 else 0
    }
  }

  /**
    * 计算二项分布的分布函数
    * * @param binomial
    *
    * @param n
    * @return
    */
  def pbinom(binomial: Binomial, n: Int) = {
    var p = 0.0
    if (n >= binomial.n) {
      p = 1
    } else {
      for (i <- 0 to n) {
        p += binomial.probabilityOf(i)
      }
    }
    p
  }

  /**
    * 计算正态分布的分布函数值
    *
    * @param norm
    * @param statistic
    * @return
    */
  def pnorm(norm: Gaussian, statistic: Double) = {
    norm.probability(Double.NegativeInfinity, statistic)
  }


}
