object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //Concatenar 2 elementos al final de una lista
  List(1,2,3):::4::Nil:::5::Nil                   //> res0: List[Int] = List(1, 2, 3, 4, 5)
}