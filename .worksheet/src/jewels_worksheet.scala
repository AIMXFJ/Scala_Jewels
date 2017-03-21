object jewels_worksheet extends App {
  import util.Random;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(201); 
  
  //Imprime cualquier lista o variable valida para for each
  def printList(args: TraversableOnce[_]): Unit = {
  	args.foreach(println)
	};System.out.println("""printList: (args: TraversableOnce[_])Unit""");$skip(273); 
  
  //Función para introducir un numero en una posicion determinada
	def introducirElemento(color:Int, pos:Int, tablero:List[Int]):List[Int] =
		if(tablero.isEmpty) Nil else if (pos==0) color::tablero.tail
		else tablero.head::introducirElemento(color,pos-1,tablero.tail);System.out.println("""introducirElemento: (color: Int, pos: Int, tablero: List[Int])List[Int]""");$skip(156); 
  
  def printTablero(tablero:List[Int], pos:Int, anchura:Int, altura:Int):Unit =
  	if(pos%anchura==0 && pos!=0) println("")
  	else println(tablero(pos));System.out.println("""printTablero: (tablero: List[Int], pos: Int, anchura: Int, altura: Int)Unit""");$skip(253); 
  
  //Genera un numero aleatorio en funcion de la dificultad
  def numRandom(dificultad:Int): Int =
  	if(dificultad == 1)(1+Random.nextInt(3))
  	else if(dificultad == 2)(1+Random.nextInt(5))
  	else if(dificultad == 3)(1+Random.nextInt(7))
  	else 0;System.out.println("""numRandom: (dificultad: Int)Int""");$skip(277); 
  
  //Inicializa el tablero de "times" tamaño a un numero aleatorio en función de la dificultad
  def initTablero (times:Int, dificultad:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initTablero (times-1,dificultad):::numRandom(dificultad)::Nil
 		}
	};System.out.println("""initTablero: (times: Int, dificultad: Int)List[Int]""");$skip(366); 
	
	//Numero de jewels a la izquierda de la posicion (x,y)
	def cuantasJewelsIzquierda(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x-1+(y)*anchura >= 0)
			if(tablero(x-1+(y)*anchura)==valor)
				cuantasJewelsIzquierda(tablero, anchura, altura, x-1, y, valor, contador+1)
			else
				contador
		else
			contador
	};System.out.println("""cuantasJewelsIzquierda: (tablero: List[Int], anchura: Int, altura: Int, x: Int, y: Int, valor: Int, contador: Int)Int""");$skip(372); 
	
	//Numero de jewels a la derecha de la posicion (x,y)
	def cuantasJewelsDerecha(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+1+(y)*anchura < anchura*altura)
			if(tablero(x+1+(y)*anchura)==valor)
				cuantasJewelsDerecha(tablero, anchura, altura, x+1, y, valor, contador+1)
			else
				contador
		else
			contador
	};System.out.println("""cuantasJewelsDerecha: (tablero: List[Int], anchura: Int, altura: Int, x: Int, y: Int, valor: Int, contador: Int)Int""");$skip(354); 
	
	//Numero de jewels por debajo de la posicion (x,y)
	def cuantasJewelsAbajo(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+(y-1)*anchura >= 0)
			if(tablero(x+(y-1)*anchura)==valor)
				cuantasJewelsAbajo(tablero, anchura, altura, x, y-1, valor, contador+1)
			else
				contador
		else
			contador
	};System.out.println("""cuantasJewelsAbajo: (tablero: List[Int], anchura: Int, altura: Int, x: Int, y: Int, valor: Int, contador: Int)Int""");$skip(368); 
	
	//Numero de jewels por encima de la posicion (x,y)
	def cuantasJewelsArriba(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+(y+1)*anchura < anchura*altura)
			if(tablero(x+(y+1)*anchura)==valor)
				cuantasJewelsArriba(tablero, anchura, altura, x, y+1, valor, contador+1)
			else
				contador
		else
			contador
	};System.out.println("""cuantasJewelsArriba: (tablero: List[Int], anchura: Int, altura: Int, x: Int, y: Int, valor: Int, contador: Int)Int""");$skip(456); 
	
	//Devuelve las jewels a eliminar en Vertical como List
	def llenarJewelsEliminarVertical(tablero:List[Int], anchura:Int, x:Int, y:Int, contArriba:Int, contAbajo:Int):List[Int] =
		if(contArriba>0) llenarJewelsEliminarVertical(tablero,anchura,x,y+1,contArriba-1,contAbajo):::tablero(x + (y+1) * anchura)::Nil
		else if(contAbajo>0) llenarJewelsEliminarVertical(tablero,anchura,x,y-1,contArriba,contAbajo-1):::tablero(x + (y-1) * anchura)::Nil
		else Nil;System.out.println("""llenarJewelsEliminarVertical: (tablero: List[Int], anchura: Int, x: Int, y: Int, contArriba: Int, contAbajo: Int)List[Int]""");$skip(488); 
	
	//Devuelve las jewels a eliminar en Horizontal como List
	def llenarJewelsEliminarHorizontal(tablero:List[Int], anchura:Int, x:Int, y:Int, contDerecha:Int, contIzquierda:Int):List[Int] =
		if(contDerecha>0) llenarJewelsEliminarHorizontal(tablero,anchura,x+1,y,contDerecha-1,contIzquierda):::tablero(x + 1 + (y) * anchura)::Nil
		else if(contIzquierda>0) llenarJewelsEliminarHorizontal(tablero,anchura,x-1,y,contDerecha,contIzquierda-1):::tablero(x - 1 + (y) * anchura)::Nil
		else Nil;System.out.println("""llenarJewelsEliminarHorizontal: (tablero: List[Int], anchura: Int, x: Int, y: Int, contDerecha: Int, contIzquierda: Int)List[Int]""");$skip(913); 
	
	//Analiza si hay jewels para eliminar y devuelve cuales en caso de que se pueda eliminar
	def analizarPosicion(tablero:List[Int],anchura:Int,altura:Int,x:Int,y:Int):List[Int] = {
		if(cuantasJewelsDerecha(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) + cuantasJewelsIzquierda(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) >= 3){
			llenarJewelsEliminarHorizontal(tablero,anchura,x,y,cuantasJewelsDerecha(tablero,anchura,altura,x,y,tablero(x+y*anchura),0),cuantasJewelsIzquierda(tablero,anchura,altura,x,y,tablero(x+y*anchura),0))
		}else if(cuantasJewelsArriba(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) + cuantasJewelsAbajo(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) >= 3){
			llenarJewelsEliminarVertical(tablero,anchura,x,y,cuantasJewelsArriba(tablero,anchura,altura,x,y,tablero(x+y*anchura),0),cuantasJewelsAbajo(tablero,anchura,altura,x,y,tablero(x+y*anchura),0))
		}else Nil
	};System.out.println("""analizarPosicion: (tablero: List[Int], anchura: Int, altura: Int, x: Int, y: Int)List[Int]""");$skip(197); 
	
	//TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
	def eliminarJewels(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, jewelsParaEliminar:List[Int]):List[Int] = tablero;System.out.println("""eliminarJewels: (tablero: List[Int], anchura: Int, altura: Int, dificultad: Int, jewelsParaEliminar: List[Int])List[Int]""");$skip(509); 
	
	//Analiza si la jewel que se ha movido a la posicion de destino permite eliminar jewels, cuantas y cuales y llama a eliminarlas
	def analisisManual(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int, valor:Int):List[Int] = {
		val jewelsParaEliminar = analizarPosicion(tablero, anchura, altura, x, y)
		if(jewelsParaEliminar == Nil) {/*return tablero*/ jewelsParaEliminar}
		else {
			//eliminarJewels(tablero,anchura,altura,dificultad,jewelsParaEliminar)
			jewelsParaEliminar
		}
	};System.out.println("""analisisManual: (tablero: List[Int], dificultad: Int, anchura: Int, altura: Int, x: Int, y: Int, valor: Int)List[Int]""");$skip(256); 
	
	//Intercambia 2 elementos en el tablero, para ello se necesita el primer elemento y ambas posiciones
	def swap(tablero:List[Int], pos1:Int, pos2:Int, elem1:Int):List[Int] =
		introducirElemento(elem1,pos2,introducirElemento(tablero(pos2),pos1,tablero));System.out.println("""swap: (tablero: List[Int], pos1: Int, pos2: Int, elem1: Int)List[Int]""");$skip(1236); 
	
	//Funcion que se ocupa de intercambiar con la jewel indicada y realizar llamar al analisis manual
	def intercambiarJewels(tablero:List[Int], pos1_x:Int, pos1_y:Int, direccion:Int, anchura:Int, altura:Int, seleccion:Int):List[Int] = {
		direccion match {
		//Arriba
			case 1 => { swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y+1)*anchura, tablero(pos1_x + pos1_y*anchura))
									analisisManual(tablero, dificultad,anchura, altura, pos1_x, pos1_y+1, tablero(pos1_x + pos1_y*anchura)) }
		//Abajo
			case 2 => { swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y-1)*anchura, tablero(pos1_x + pos1_y*anchura))
									analisisManual(tablero, dificultad,anchura, altura, pos1_x, pos1_y-1, tablero(pos1_x + pos1_y*anchura)) }
		//Izquierda
			case 3 => { swap(tablero, pos1_x + pos1_y*anchura, pos1_x-1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura))
								analisisManual(tablero, dificultad,anchura, altura, pos1_x+1, pos1_y, tablero(pos1_x + pos1_y*anchura)) }
		//Derecha
			case 4=> { swap(tablero, pos1_x + pos1_y*anchura, pos1_x+1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura))
								analisisManual(tablero, dificultad,anchura, altura, pos1_x+1, pos1_y, tablero(pos1_x + pos1_y*anchura)) }
		}
	};System.out.println("""intercambiarJewels: (tablero: List[Int], pos1_x: Int, pos1_y: Int, direccion: Int, anchura: Int, altura: Int, seleccion: Int)List[Int]""");$skip(36); 
	
	println("Anchura del tablero: ");$skip(38); 
	val anchura=scala.io.StdIn.readInt();System.out.println("""anchura  : Int = """ + $show(anchura ));$skip(35); 
	
	println("Altura del tablero: ");$skip(37); 
	val altura=scala.io.StdIn.readInt();System.out.println("""altura  : Int = """ + $show(altura ));$skip(39); 
  
	println("Dificultad (1, 2 o 3): ");$skip(41); 
	val dificultad=scala.io.StdIn.readInt();System.out.println("""dificultad  : Int = """ + $show(dificultad ));$skip(84); 
	
	//Se inicializa el tablero
	val tablero = initTablero(anchura*altura,dificultad);System.out.println("""tablero  : List[Int] = """ + $show(tablero ));$skip(91); 
	
	printList(analisisManual(tablero,dificultad,anchura,altura,5,7,tablero(5 + 7*anchura)))}
}
