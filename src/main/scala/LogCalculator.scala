class LogCalculator(name:String) extends Calculator2(name){
  def log(num: Double, base: Double) = math.log(num) / math.log(base)
}
