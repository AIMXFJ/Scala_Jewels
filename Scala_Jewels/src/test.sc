object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //Concatenar 2 elementos al final de una lista
  val list = List(1,2,3):::4::Nil:::5::Nil        //> list  : List[Int] = List(1, 2, 3, 4, 5)
  
  list.view.zipWithIndex foreach {case (value,index) => println(value,index)}
                                                  //> (1,0)
                                                  //| (2,1)
                                                  //| (3,2)
                                                  //| (4,3)
                                                  //| (5,4)
  if(list.isEmpty)
  	print("empty")
  	
  list.tail.tail                                  //> res0: List[Int] = List(3, 4, 5)
  
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
	
	if(true)
		list ::: 16 :: Nil ::: 55 :: Nil  //> res2: Any = List(1, 2, 3, 4, 5, 16, 55)
		
	val list1 = List(1,2)                     //> list1  : List[Int] = List(1, 2)
	val list_copy = list                      //> list_copy  : List[Int] = List(1, 2, 3, 4, 5)
	list_copy ::: 56 :: Nil                   //> res3: List[Int] = List(1, 2, 3, 4, 5, 56)
	list ::: -56 :: Nil                       //> res4: List[Int] = List(1, 2, 3, 4, 5, -56)
	
	list1 ::: Nil ::: Nil ::: 1 :: Nil ::: 2 :: Nil
                                                  //> res5: List[Int] = List(1, 2, 1, 2)
}