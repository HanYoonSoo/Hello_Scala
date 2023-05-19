class Calculator2(name: String) {
  var color : String = if(name == "Choi"){
    "blue"
  }
  else if(name == "Kim"){
    "green"
  }
  else{
    "white"
  }
  def add(n1 : Int, n2 : Int) : Int = n1 + n2
}
