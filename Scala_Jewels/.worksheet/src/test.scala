object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(57); 
  println("Welcome to the Scala worksheet");$skip(95); 
  
  //Concatenar 2 elementos al final de una lista
  val list = List(1,2,3):::4::Nil:::5::Nil;System.out.println("""list  : List[Int] = """ + $show(list ));$skip(81); 
  
  list.view.zipWithIndex foreach {case (value,index) => println(value,index)};$skip(37); 
  if(list.isEmpty)
  	print("empty");$skip(21); val res$0 = 
  	
  list.tail.tail;System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(121); 
  
  def longitudLista(lista:List[Int]):Int = {
	  if(!lista.isEmpty)
	    longitudLista(lista.tail)+1
	  else
	    0
	};System.out.println("""longitudLista: (lista: List[Int])Int""");$skip(23); val res$1 = 
	
	longitudLista(list);System.out.println("""res1: Int = """ + $show(res$1));$skip(45); 
	
	List.range(0, 5).foreach(i => println(i));$skip(47); val res$2 = 
	
	if(true)
		list ::: 16 :: Nil ::: 55 :: Nil;System.out.println("""res2: Any = """ + $show(res$2));$skip(26); 
		
	val list1 = List(1,2);System.out.println("""list1  : List[Int] = """ + $show(list1 ));$skip(22); 
	val list_copy = list;System.out.println("""list_copy  : List[Int] = """ + $show(list_copy ));$skip(25); val res$3 = 
	list_copy ::: 56 :: Nil;System.out.println("""res3: List[Int] = """ + $show(res$3));$skip(21); val res$4 = 
	list ::: -56 :: Nil;System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(51); val res$5 = 
	
	list1 ::: Nil ::: Nil ::: 1 :: Nil ::: 2 :: Nil;System.out.println("""res5: List[Int] = """ + $show(res$5))}
}
