package nonparrametic



/**
  * Created by WZZC on 2018/8/11
  **/

case class coxstuart(pairing: Int, spositive: Int, snagtive: Int, pvalue: Double) {

  require(pvalue >= 0)
  require(pvalue <= 1)

  def apply(pairing: Int, spositive: Int, snagtive: Int, pvalue: Double): coxstuart = new coxstuart(pairing, spositive, snagtive, pvalue)

}


