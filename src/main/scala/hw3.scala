import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.convert.ImplicitConversions.`collection asJava`

object hw3 {

  // 입력 n은 0보다 큰 정수라고 가정합니다.
  // p1
  def sum(n: Int): Int = {
    // 자기호출 함수를 사용하여 계산합니다.
    if(n == 0)
      return 0

    n + sum(n-1)
  }

  // 입력 n은 0보다 큰 정수라고 가정합니다.
  // p2
  def fac(n: Int): Int = {
    // n이 1이면 1로 출력합니다
    // 그렇지 않으면 재귀함수를 사용하여 계산합니다.
    if(n == 1)
      return 1
    else{
      n * fac(n-1)
    }
  }

  // 입력 n은 0보다 큰 정수라고 가정합니다.
  // p3
  def fib(n: Int): Int = n match {
    // n 이 1 또는 2면 1을 출력합니다.
    // n이 3 이상인 경우에는
    case 1 | 2 => 1
    case _ => fib(n-1) + fib(n-2)
  }

  // 입력 m과 n은 0 이상이고 m+n은 0보다 크다고 가정합니다.
  // p4
  def gcd(n: Int, m: Int): Int = {
    // 작은 값과 큰 값을 먼저 출력합니다.
    // 유클리드 호제법을 사용하여 자기호출 함수로 계산하세요

    if(m < n)
      println("gcd " + m + " " + n)

    if(m == 0) {
      n
    } else {
      gcd(m, n % m)
    }
  }

  // 입력 list는 List(3, 4, 14, 7, 2)와 같은 형태의 정수 리스트라고 가정합니다.
  // p5
  def max(list: List[Int]): Int = {
    // 정수 리스트에서 가장 큰 값을 반환하는 자기호출 함수를 작성하세요.
    // 빈 리스트를 입력받으면 0을 반환하세요.
    // 그렇지 않다면..

    if(list.isEmpty){
      0
    }
    else{
      val head = list.head
      val tail = list.drop(1)
      val maxValue = max(tail)

      if(head < maxValue)
        maxValue
      else
        head
    }
  }

  sealed trait Tree[Int]
  case class Leaf[Int](elem: Int) extends Tree[Int]
  case class Node[Int](elem: Int, left: Tree[Int], right: Tree[Int]) extends Tree[Int]

  // p6
  def sum_tree(t: Tree[Int]): Int = {
    //재귀함수를 이용하여 트리의 합을 구하세요.
    if(t.isInstanceOf[Leaf[Int]]){
      var leaf = t.asInstanceOf[Leaf[Int]]
      leaf.elem
    }
    else{
      var node = t.asInstanceOf[Node[Int]]
      node.elem + sum_tree(node.left) + sum_tree(node.right)
    }
  }

  // p7
  def depth(t: Tree[Int]): Int = {
    //재귀함수를 이용하여 트리의 깊이를 구하세요.
    if(t.isInstanceOf[Leaf[Int]])
      0
    else{
      val node = t.asInstanceOf[Node[Int]]
      1 + List(depth(node.left), depth(node.right)).max
    }
  }

  // p8
  def bin_search(t: Tree[Int], x: Int): Boolean = {
    // 재귀함수를 이용하여 이진 검색을 수행합니다.

    // 트리 순회를 하는 경우
//    if(t.isInstanceOf[Leaf[Int]]){
//      val leaf = t.asInstanceOf[Leaf[Int]]
//      if(leaf.elem == x)
//        true
//      else
//        false
//    }
//    else{
//      val node = t.asInstanceOf[Node[Int]]
//      if(node.elem == x)
//        true
//      else
//        bin_search(node.left, x) || bin_search(node.right, x)
//    }

    // 이진 검색의 경우(하지만, 주어진 트리가 이진 탐색 트리가 아니라 의미가 없음)
    if(t.isInstanceOf[Leaf[Int]]){
      val leaf = t.asInstanceOf[Leaf[Int]]
      if(leaf.elem == x){
        true
      }
      else{
        false
      }
    }
    else{
      val node = t.asInstanceOf[Node[Int]]
      if(node.elem == x){
        true
      }
      else if(node.elem < x){
        bin_search(node.right, x)
      }
      else{
        bin_search(node.left, x)
      }
    }

  }

  sealed trait Exp[Int]
  case class INT[Int](value: Int) extends Exp[Int]
  case class ADD[Int](left: Exp[Int], right: Exp[Int]) extends Exp[Int]
  case class SUB[Int](left: Exp[Int], right: Exp[Int]) extends Exp[Int]
  case class MUL[Int](left: Exp[Int], right: Exp[Int]) extends Exp[Int]
  case class DIV[Int](left: Exp[Int], right: Exp[Int]) extends Exp[Int]
  case class MOD[Int](left: Exp[Int], right: Exp[Int]) extends Exp[Int]

  // p9
  def interp(exp: Exp[Int]): Int = {
    // 재귀함수를 사용하여 함수를 작성하세요.
    if(exp.isInstanceOf[INT[Int]]){
      val intExp = exp.asInstanceOf[INT[Int]]
      intExp.value
    }
    else if(exp.isInstanceOf[ADD[Int]]){
      val addExp = exp.asInstanceOf[ADD[Int]]
      interp(addExp.left) + interp(addExp.right)
    }
    else if (exp.isInstanceOf[SUB[Int]]) {
      val subExp = exp.asInstanceOf[SUB[Int]]
      interp(subExp.left) - interp(subExp.right)
    }
    else if (exp.isInstanceOf[MUL[Int]]) {
      val mulExp = exp.asInstanceOf[MUL[Int]]
      interp(mulExp.left) * interp(mulExp.right)
    }
    else if (exp.isInstanceOf[DIV[Int]]) {
      val divExp = exp.asInstanceOf[DIV[Int]]
      interp(divExp.left) / interp(divExp.right)
    }
    else if (exp.isInstanceOf[MOD[Int]]) {
      val modExp = exp.asInstanceOf[MOD[Int]]
      interp(modExp.left) % interp(modExp.right)
    }
    else{
      println("계산할 수 없습니다.")
      -1
    }
  }

  sealed trait Bool
  case object True extends Bool
  case object False extends Bool
  case class Neg(fma: Bool) extends Bool
  case class Or(left: Bool, right: Bool) extends Bool
  case class And(left: Bool, right: Bool) extends Bool
  case class Imply(left: Bool, right: Bool) extends Bool

  // p10
  def formula(fma: Bool): Boolean = fma match {
    case True => true
    case False => false
    case Neg(fma) => !formula(fma)
    case Or(left, right) => formula(left) || formula(right)
    case And(left, right) => formula(left) && formula(right)
    case Imply(left, right) => !formula(left) || formula(right)
  }
  def main(args: Array[String]):Unit = {
    println("** p1 **")
    println(sum(100))
    println()

    println("** p2 **")
    println(fac(9))
    println()

    println("** p3 **")
    println(fib(10))
    println()

    println("** p4 **")
    println(gcd(15, 20))
    println()

    var x = List(3, 4, 14, 7, 2)
    println("** p5 **")
    println(max(x))
    println()

    var tree = Node(7, Node(3, Leaf(1), Leaf(2)), Leaf(4))
    println("***p6***")
    println("Sum of tree nodes: " + sum_tree(tree))
    println()

    println("***p7***")
    println("depth of tree nodes: " + depth(tree))
    println()

    println("***p8***")
    println("bin_search of tree nodes: " + bin_search(tree, 3))
    println()

    val exp = ADD(SUB(INT(100), INT(10)), MUL(INT(2), INT(8)))
    println("***p9***")
    println("Interpretation of the expression is : " + interp(exp))
    println()

    val fma = Imply(And(True, Or(True, False)), False)
    println("***p10***")
    println(formula(fma))
  }
}
