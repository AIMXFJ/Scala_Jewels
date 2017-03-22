object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //Concatenar 2 elementos al final de una lista
  val list = List(1,2,3):::4::Nil:::7::Nil        //> list  : List[Int] = List(1, 2, 3, 4, 7)
  
  list.view.zipWithIndex foreach {case (value,index) => println(value,index)}
                                                  //> (1,0)
                                                  //| (2,1)
                                                  //| (3,2)
                                                  //| (4,3)
                                                  //| (7,4)
  if(list.isEmpty)
  	print("empty")
  	
  list.tail.tail                                  //> res0: List[Int] = List(3, 4, 7)
  
  def longitudLista(lista:List[Int]):Int = {
	  if(!lista.isEmpty)
	    longitudLista(lista.tail)+1
	  else
	    0
	}                                         //> longitudLista: (lista: List[Int])Int
	
	longitudLista(list)                       //> res1: Int = 5
	
	List.range(0, 5).foreach(i => println(i)) //> 0
                                                  //| 1
                                                  //| 2
                                                  //| 3
                                                  //| 4
}