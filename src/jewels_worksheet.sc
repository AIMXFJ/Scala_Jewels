object jewels_worksheet {
  import util.Random
  
  //Función para introducir un numero en una posicion determinada
	def introducirElemento(color:Int, pos:Int, tablero:List[Int]):List[Int] =
		if(tablero.isEmpty) Nil else if (pos==0) color::tablero.tail
		else tablero.head::introducirElemento(color,pos-1,tablero.tail)
  
  def printTablero(tablero:List[Int], pos:Int, anchura:Int, altura:Int):Unit =
  	if(pos%anchura==0 && pos!=0) println("")
  	else println(tablero(pos))
  
  //Genera un numero aleatorio en funcion de la dificultad
  def numRandom(dificultad:Int): Int =
  	if(dificultad == 1)(1+Random.nextInt(3))
  	else if(dificultad == 2)(1+Random.nextInt(5))
  	else if(dificultad == 3)(1+Random.nextInt(7))
  	else 0
  
  //Inicializa el tablero de "times" tamaño a un numero aleatorio en función de la dificultad
  def initTablero (times:Int, dificultad:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initTablero (times-1,dificultad):::numRandom(dificultad)::Nil
 		}
	}
	
	//Numero de jewels a la izquierda de la posicion (x,y)
	def cuantasJewelsIzquierda(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x-1+(y)*anchura >= 0)
			if(tablero(x-1+(y)*anchura)==valor)
				cuantasJewelsIzquierda(tablero, anchura, altura, x-1, y, valor, contador+1)
			else
				contador
		else
			contador
	}
	
	//Numero de jewels a la derecha de la posicion (x,y)
	def cuantasJewelsDerecha(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+1+(y)*anchura < anchura*altura)
			if(tablero(x+1+(y)*anchura)==valor)
				cuantasJewelsDerecha(tablero, anchura, altura, x+1, y, valor, contador+1)
			else
				contador
		else
			contador
	}
	
	//Numero de jewels por debajo de la posicion (x,y)
	def cuantasJewelsAbajo(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+(y-1)*anchura >= 0)
			if(tablero(x+(y-1)*anchura)==valor)
				cuantasJewelsAbajo(tablero, anchura, altura, x, y-1, valor, contador+1)
			else
				contador
		else
			contador
	}
	
	//Numero de jewels por encima de la posicion (x,y)
	def cuantasJewelsArriba(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+(y+1)*anchura < anchura*altura)
			if(tablero(x+(y+1)*anchura)==valor)
				cuantasJewelsArriba(tablero, anchura, altura, x, y+1, valor, contador+1)
			else
				contador
		else
			contador
	}
	
	//Devuelve las jewels a eliminar en Vertical como List
	def llenarJewelsEliminarVertical(tablero:List[Int], anchura:Int, x:Int, y:Int, contArriba:Int, contAbajo:Int):List[Int] =
		if(contArriba>0) llenarJewelsEliminarVertical(tablero,anchura,x,y+1,contArriba-1,contAbajo):::tablero(x + (y+1) * anchura)::Nil
		else if(contAbajo>0) llenarJewelsEliminarVertical(tablero,anchura,x,y-1,contArriba,contAbajo-1):::tablero(x + (y-1) * anchura)::Nil
		else Nil
	
	//Devuelve las jewels a eliminar en Horizontal como List
	def llenarJewelsEliminarHorizontal(tablero:List[Int], anchura:Int, x:Int, y:Int, contDerecha:Int, contIzquierda:Int):List[Int] =
		if(contDerecha>0) llenarJewelsEliminarHorizontal(tablero,anchura,x+1,y,contDerecha-1,contIzquierda):::tablero(x + 1 + (y) * anchura)::Nil
		else if(contIzquierda>0) llenarJewelsEliminarHorizontal(tablero,anchura,x-1,y,contDerecha,contIzquierda-1):::tablero(x - 1 + (y) * anchura)::Nil
		else Nil
	
	//Analiza si hay jewels para eliminar y devuelve cuales en caso de que se pueda eliminar
	def analizarPosicion(tablero:List[Int],anchura:Int,altura:Int,x:Int,y:Int):List[Int] = {
		if(cuantasJewelsDerecha(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) + cuantasJewelsIzquierda(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) >= 3){
			llenarJewelsEliminarHorizontal(tablero,anchura,x,y,cuantasJewelsDerecha(tablero,anchura,altura,x,y,tablero(x+y*anchura),0),cuantasJewelsIzquierda(tablero,anchura,altura,x,y,tablero(x+y*anchura),0))
		}else if(cuantasJewelsArriba(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) + cuantasJewelsAbajo(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) >= 3){
			llenarJewelsEliminarVertical(tablero,anchura,x,y,cuantasJewelsArriba(tablero,anchura,altura,x,y,tablero(x+y*anchura),0),cuantasJewelsAbajo(tablero,anchura,altura,x,y,tablero(x+y*anchura),0))
		}else Nil
	}
	
	//TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
	def eliminarJewels(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, jewelsParaEliminar:List[Int]):List[Int] = tablero
	
	//Analiza si la jewel que se ha movido a la posicion de destino permite eliminar jewels, cuantas y cuales y llama a eliminarlas
	def analisisManual(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int, valor:Int):List[Int] = {
		val jewelsParaEliminar = analizarPosicion(tablero, anchura, altura, x, y)
		if(jewelsParaEliminar == Nil) {/*return tablero*/ jewelsParaEliminar}
		else {
			//eliminarJewels(tablero,anchura,altura,dificultad,jewelsParaEliminar)
			jewelsParaEliminar
		}
	}
	
	//Intercambia 2 elementos en el tablero, para ello se necesita el primer elemento y ambas posiciones
	def swap(tablero:List[Int], pos1:Int, pos2:Int, elem1:Int):List[Int] =
		introducirElemento(elem1,pos2,introducirElemento(tablero(pos2),pos1,tablero))
	
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
	}
  
	val anchura = 15
	val altura = 11
	val dificultad = 3
	
	//Se inicializa el tablero
	val tablero = initTablero(anchura*altura,dificultad)
	
	analisisManual(tablero,dificultad,anchura,altura,5,7,tablero(5 + 7*anchura))
}