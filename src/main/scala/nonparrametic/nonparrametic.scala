package nonparrametic

import breeze.stats.distributions._

import math._
import scala.collection.mutable.ListBuffer


/**
  * Created by WZZC on 2018/7/24
  *
  **/
object nonparrametic {


  /**
    * CoxSturt趋势检验
    *
    * @param timeSeries
    * @param AlternativeHypothesis
    * @return
    */
  def CoxStuart(timeSeries: Seq[Double],
                AlternativeHypothesis: String = hypothesis.GROWTHREND) = {

    val length = timeSeries.length

    val ts = if (length % 2 != 0) {
      timeSeries.drop(length / 2 + 1)
    } else timeSeries

    val pre = ts.slice(0, ts.length / 2)
    val pro = ts.slice(ts.length / 2, ts.length)
    val sign = new ListBuffer[Double]()
    for (i <- 0 until length / 2) {
      sign.append(pre(i) - pro(i))
    }
    val spositive = sign.count(_ > 0)
    val snagtive = sign.count(_ < 0)

    def min(n: Int, g: Int) = {
      if (n > g) g else n
    }

    val binomial = Binomial(length / 2, 0.5)

    val p = AlternativeHypothesis.toUpperCase match {
      case "REDUCETREND" => nonparrameticutils pbinom(binomial, snagtive)
      case "NOTREND" => nonparrameticutils.pbinom(binomial, min(snagtive, spositive)) * 2
      case _ => nonparrameticutils pbinom(binomial, spositive)
    }
    coxstuart(ts.length / 2, spositive, snagtive, p)
  }

  /**
    *
    * @param serise
    * @param alternative
    * @return
    */
  def runsTest(serise: Seq[Int], alternative: String = hypothesis.twosided): (Double, Double) = {

    if (serise.distinct.length > 2) {
      println("the serise need 0-1 destribution")
      (Double.NegativeInfinity, Double.NegativeInfinity)
    } else {
      // 计算序列长度
      val slen = serise.length
      // 计算游程数
      var runsTimes = 1
      for (i <- 0 until slen - 1) {
        if (serise(i) != serise(i + 1)) runsTimes += 1 else runsTimes
      }

      val m = serise.filter(_ == 1).size
      val n = serise.filter(_ == 0).size
      val E = 1 + 2 * n * m / (n + m)
      val s2 = (2 * n * m * (2 * n * m - n - m)) / (math.pow(n + m, 2) * (n + m - 1))

      // 构建检验统计量（大样本条件下的服从正态分布）
      val statistic = (runsTimes - E) / math.sqrt(s2)

      val norm = new Gaussian(0, 1)


      // 求不同假设条件下的p值
      val pvalue: Double = if (alternative == "positive.correlated") {
        // "Runs Test - Positive Correlated"
        nonparrameticutils pnorm(norm, statistic)

      } else if (alternative == "negative.correlated") {
        //  "Runs Test - Negative Correlated"
        1 - nonparrameticutils.pnorm(norm, statistic)

      } else {
        //  "Runs Test - Two sided"
        2 * min(nonparrameticutils pnorm(norm, statistic), 1 - nonparrameticutils.pnorm(norm, statistic))

      }
      // 返回检验统计量和p值
      (statistic, pvalue)
    }

  }


  /**
    * Wilcox
    *
    * @param serise
    * @return
    */
  def Wilcoxon(serise: Seq[Double]) = {
    // 求中位数
    val m0 = nonparrameticutils.mid(serise)
    // 计算 xi -m0 ； |xi-m0|
    val absdiff = serise.map(xi => {
      (xi, m0, math.abs(xi - m0))
    })
    // 求秩 返回（M0-中位数,Xi,Rank-秩,number-个数）
    val drank = nonparrameticutils
      .rank(absdiff.map(_._3).filter(_ > 0))
      .map(r => {
        (m0, r._1, r._2._1, r._2._2)
      })

    val wpostive = drank.filter(r => r._2 > r._1)
    val wnagtive = drank.filter(r => r._2 < r._1)

    drank
  }


}


