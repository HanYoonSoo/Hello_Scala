object ArrayTest2 {
  def main(args: Array[String]) : Unit = {
    val array = Array("hello", "world")

    // +: Array 앞에, :+ Array 뒤에 추가됨
    val conn = "front" +: array :+ "back"
    println(conn.length)

    // for문 사용법은 다음 실습에 더 자세히 하도록 하겠습니다.
    for (i <- 0 to conn.length - 1) {
      println(conn(i))
    }
  }
}
