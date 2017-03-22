

object jewels extends App {
  import util.Random
  
  //Imprime cualquier lista o variable valida para for each
  def printList(args:List[Int]):Unit = {
      args.foreach(println)
  }
  
  //Función para introducir un numero en una posicion determinada
	def introducirElemento(color:Int, pos:Int, tablero:List[Int]):List[Int] =
		if(tablero.isEmpty) Nil else if (pos==0) color::tablero.tail
		else tablero.head::introducirElemento(color,pos-1,tablero.tail)
		
	def printTablero(tablero:List[Int], x:Int, y:Int, anchura:Int, altura:Int):Unit = {
	  if(x >= 0 && x < anchura-1){
	    print(tablero(x+y*anchura) + " ")
	    printTablero(tablero, x+1, y, anchura, altura)
	  }else if (x==anchura-1 && y>=0) {
	    print(tablero(x+y*anchura))
	    println()
	    if(y>0)
	      printTablero(tablero, 0, y-1, anchura, altura)
	  }
	}
  
  //Genera un numero aleatorio en funcion de la dificultad
  def numRandom(dificultad:Int): Int =
  	if(dificultad == 1)(1+Random.nextInt(4))
  	else if(dificultad == 2)(1+Random.nextInt(6))
  	else if(dificultad == 3)(1+Random.nextInt(8))
  	else 0
  
  //Inicializa el tablero de "times" tamaño a un numero aleatorio en función de la dificultad
  def initTablero (times:Int, dificultad:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initTablero (times-1,dificultad):::numRandom(dificultad)::Nil
 		}
	}
  
  //Inicializa un tablero auxiliar a todo 1
  def initTableroAux (times:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initTableroAux (times-1):::1::Nil
 		}
	}
	
	//Numero de jewels a la izquierda de la posicion (x,y)
	def cuantasJewelsIzquierda(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x-1+(y)*anchura >= 0 && (x-1) >= 0 && (x-1) < anchura -1)
			if(tablero(x-1+(y)*anchura)==valor)
				cuantasJewelsIzquierda(tablero, anchura, altura, x-1, y, valor, contador+1)
			else
				contador
		else
			contador
	}
	
	//Numero de jewels a la derecha de la posicion (x,y)
	def cuantasJewelsDerecha(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+1+(y)*anchura < anchura*altura && (x+1)>0 && (x+1) < anchura)
			if(tablero(x+1+(y)*anchura)==valor)
				cuantasJewelsDerecha(tablero, anchura, altura, x+1, y, valor, contador+1)
			else
				contador
		else
			contador
	}
	
	//Numero de jewels por debajo de la posicion (x,y)
	def cuantasJewelsAbajo(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+(y-1)*anchura >= 0 && (y-1) >= 0 && (y-1) < altura -1 )
			if(tablero(x+(y-1)*anchura)==valor)
				cuantasJewelsAbajo(tablero, anchura, altura, x, y-1, valor, contador+1)
			else
				contador
		else
			contador
	}
	
	//Numero de jewels por encima de la posicion (x,y)
	def cuantasJewelsArriba(tablero:List[Int], anchura:Int, altura:Int, x:Int, y:Int, valor:Int, contador:Int):Int = {
		if(x+(y+1)*anchura < anchura*altura && (y+1) > 0 && (y+1) < altura)
			if(tablero(x+(y+1)*anchura)==valor)
				cuantasJewelsArriba(tablero, anchura, altura, x, y+1, valor, contador+1)
			else
				contador
		else
			contador
	}
	
	//Devuelve las jewels a eliminar en Vertical como List
	def llenarJewelsEliminarVertical(tablero:List[Int], anchura:Int, x:Int, y:Int, contArriba:Int, contAbajo:Int, añadidoInicial:Boolean):List[Int] = {
	  println("Llenar Vertical")
	  if(contAbajo>0) llenarJewelsEliminarVertical(tablero,anchura,x,y,contArriba,contAbajo-1,añadidoInicial):::x::Nil:::y-contAbajo::Nil
		else if(!añadidoInicial) llenarJewelsEliminarVertical(tablero,anchura,x,y,contArriba,contAbajo,true):::x::Nil:::y::Nil
	  else if(contArriba>0) llenarJewelsEliminarVertical(tablero,anchura,x,y,contArriba-1,contAbajo,añadidoInicial):::x::Nil:::(y+contArriba)::Nil
		else Nil
  }
	
	//Devuelve las jewels a eliminar en Horizontal como List
	def llenarJewelsEliminarHorizontal(tablero:List[Int], anchura:Int, x:Int, y:Int, contDerecha:Int, contIzquierda:Int, añadidoInicial:Boolean):List[Int] = {
		println("Llenar Horiontal")
		if(contIzquierda>0) llenarJewelsEliminarHorizontal(tablero,anchura,x,y,contDerecha,contIzquierda-1,añadidoInicial):::(x - contIzquierda)::Nil:::y::Nil
		else if(!añadidoInicial) llenarJewelsEliminarHorizontal(tablero,anchura,x,y,contDerecha,contIzquierda,true):::x::Nil:::y::Nil
		else if(contDerecha>0) llenarJewelsEliminarHorizontal(tablero,anchura,x,y,contDerecha-1,contIzquierda,añadidoInicial):::(x + contDerecha)::Nil:::y::Nil
		else Nil
	}
	
	//Analiza si hay jewels para eliminar y devuelve cuales en caso de que se pueda eliminar
	def analizarPosicion(tablero:List[Int],anchura:Int,altura:Int,x:Int,y:Int):List[Int] = {
		if(cuantasJewelsDerecha(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) + 1 + cuantasJewelsIzquierda(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) >= 3){
			llenarJewelsEliminarHorizontal(tablero,anchura,x,y,cuantasJewelsDerecha(tablero,anchura,altura,x,y,tablero(x+y*anchura),0),cuantasJewelsIzquierda(tablero,anchura,altura,x,y,tablero(x+y*anchura),0),false)
		}else if(cuantasJewelsArriba(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) + 1 + cuantasJewelsAbajo(tablero,anchura,altura,x,y,tablero(x+y*anchura),0) >= 3){
			println("VERTICAL")
		  llenarJewelsEliminarVertical(tablero,anchura,x,y,cuantasJewelsArriba(tablero,anchura,altura,x,y,tablero(x+y*anchura),0),cuantasJewelsAbajo(tablero,anchura,altura,x,y,tablero(x+y*anchura),0),false)
		}else Nil
	}
	
	//Intercambia 2 elementos en el tablero, para ello se necesita el primer elemento y ambas posiciones
	def swap(tablero:List[Int], pos1:Int, pos2:Int, elem1:Int):List[Int] =
		introducirElemento(elem1,pos2,introducirElemento(tablero(pos2),pos1,tablero))
	
	//Baja la columna eliminando de forma vertical
	def moverColumnaVertical(tablero:List[Int], anchura:Int, altura:Int, init_y:Int, x:Int, y:Int, dificultad:Int, jewelsParaEliminar:List[Int]):List[Int] = {
	  println("x: " + x + " y: " + y)
	  println("jewels0: " + jewelsParaEliminar(0))
	  if(y+1<altura){
	    println("SWAP")
	    moverColumnaVertical(swap(tablero, x+y*anchura,x+(y+1)*anchura,tablero(x+y*anchura)),anchura,altura,init_y,x,y+1,dificultad,jewelsParaEliminar)
	  }else if(!jewelsParaEliminar.tail.tail.isEmpty){
	    println("Fin Iteracion, continua")
	    moverColumnaVertical(introducirElemento(numRandom(dificultad), x+y*anchura, tablero),anchura,altura,init_y,jewelsParaEliminar.tail.tail(1),init_y,dificultad,jewelsParaEliminar.tail.tail)
	  }else{
	    println("FIN")
	    introducirElemento(numRandom(dificultad), x+y*anchura, tablero)
	  }
	}
	
	//Baja las columnas al eliminar horizontalmente
	def moverColumnas(tablero:List[Int], anchura:Int, altura:Int, init_y:Int, x:Int, y:Int, dificultad:Int, jewelsParaEliminar:List[Int]):List[Int] = {
	  if(y+1<altura){
	    moverColumnas(swap(tablero, x+y*anchura,x+(y+1)*anchura,tablero(x+y*anchura)),anchura,altura,init_y,x,y+1,dificultad,jewelsParaEliminar)
	  }else if(!jewelsParaEliminar.tail.tail.isEmpty){
	    moverColumnas(introducirElemento(numRandom(dificultad), x+y*anchura, tablero),anchura,altura,init_y,jewelsParaEliminar.tail.tail(0),init_y,dificultad,jewelsParaEliminar.tail.tail)
	  }else{
	    introducirElemento(numRandom(dificultad), x+y*anchura, tablero)
	  }
	}
	
	//TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
	def eliminarJewels(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, jewelsParaEliminar:List[Int]):List[Int] = {
	  printList(jewelsParaEliminar)
	  //Horizontal
	  if(jewelsParaEliminar(1)==jewelsParaEliminar(3)){
	    println("HORIZONTAL")
  	  return moverColumnas(tablero, anchura, altura, jewelsParaEliminar(1), jewelsParaEliminar(0), jewelsParaEliminar(1),dificultad,jewelsParaEliminar)
  	//Vertical
	  }else{
	    println("VERTICAL")
	    return moverColumnaVertical(tablero, anchura, altura, jewelsParaEliminar.reverse(0), jewelsParaEliminar.reverse(1), jewelsParaEliminar.reverse(0),dificultad,jewelsParaEliminar.reverse)
	  }
	}
	
	//Analiza si la jewel que se ha movido a la posicion de destino permite eliminar jewels, cuantas y cuales y llama a eliminarlas
	def analisisManual(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int, valor:Int):List[Int] = {
		val jewelsParaEliminar = analizarPosicion(tablero, anchura, altura, x, y)
		if(jewelsParaEliminar == Nil) {tablero}
		else {
		  println("ELIMINAR")
			return eliminarJewels(tablero,anchura,altura,dificultad,jewelsParaEliminar)
		}
	}
	
	//Funcion que se ocupa de intercambiar con la jewel indicada y realizar llamar al analisis manual
	def intercambiarJewels(tablero:List[Int], pos1_x:Int, pos1_y:Int, direccion:Int, anchura:Int, altura:Int, seleccion:Int):List[Int] = {
		direccion match {
		//analisisManual recibe como tablero el resultante del swap
		//Arriba, si no se sale del tablero lo hace
			case 1 => { if(pos1_x + (pos1_y+1)*anchura < anchura*altura) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y+1)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x, pos1_y+1, tablero(pos1_x + pos1_y*anchura))
			}
			else tablero
			}
		//Abajo
			case 2 => { if(pos1_x + (pos1_y-1)*anchura >= 0) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y-1)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x, pos1_y-1, tablero(pos1_x + pos1_y*anchura))
			}
			else tablero
			}
		//Izquierda
			case 3 => { if(pos1_x-1 + (pos1_y)*anchura < anchura*altura) { 
			  analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x-1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x+1, pos1_y, tablero(pos1_x + pos1_y*anchura))
			}
			else tablero
			}
		//Derecha
			case 4=> { if(pos1_x+1 + (pos1_y)*anchura >= 0) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x+1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x+1, pos1_y, tablero(pos1_x + pos1_y*anchura))
			}
			else tablero
			}
		}
	}
	
	//Solo mira directamente a la derecha -> Solucion: Que mueva con minimo 2 en vez de 3
	def analisisAutomaticoRec(tablero:List[Int], aux:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int):List[Int] = {
	  if(x+y*anchura < altura*anchura) {
  	  if(x < anchura-1)
  	    analisisAutomaticoRec(tablero,introducirElemento(cuantasJewelsDerecha(tablero, anchura, altura, x+1, y, tablero(x+y*anchura), 0)+1,x+1+y*anchura,aux),dificultad,anchura,altura,x+1,y)
  	  else
  	    analisisAutomaticoRec(tablero,introducirElemento(cuantasJewelsDerecha(tablero, anchura, altura, x+1, y, tablero(x+y*anchura), 0)+1,x+1+y*anchura,aux),dificultad,anchura,altura,0,y+1)
	  } else
	      aux
	}
	
	//Busca el mejor movimiento hacia la derecha empleando una estructura auxiliar para guardar cuantas jewels se eliminarian
	def analisisAutomatico(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int):List[Int] = {
	  val aux = initTableroAux(anchura*altura)
	  printTablero(analisisAutomaticoRec(tablero, aux, dificultad, anchura, altura, 0, 0),0,altura-1,anchura,altura)
	  aux
	}
	
	def bucleJuego(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, seleccion:Int, jugando:Boolean):Unit = {
	  if(!jugando) return
	  
	  println("Nuevo turno")
	  printTablero(tablero,0,altura-1,anchura,altura)
	  println("________________________________")
	  
	  seleccion match {
	    case 1 => {
	      //bucleJuego(analisisAutomatico(tablero, dificultad, anchura, altura),anchura,altura,dificultad,seleccion,jugando)
	      analisisAutomatico(tablero, dificultad, anchura, altura)
	      //bucleJuego(tablero,anchura,altura,dificultad,seleccion,jugando)
	    }
	    case 2 => {
	      println("Seleccione la jewel a intercambiar")
	      
	      println("X->")
	      val x=scala.io.StdIn.readInt()
	      
	      println("Y->")
	      val y=scala.io.StdIn.readInt()
	      
	      println("Direccion del intercambio:\n   1.-Arriba\n   2.-Abajo\n   3.-Izquierda\n   4.-Derecha")
	      val direccion=scala.io.StdIn.readInt()
	      
	      if(direccion < 1 && direccion > 4){
	        println("¡DIRECCION ERRONEA!")
	        bucleJuego(tablero,anchura,altura,dificultad,seleccion,jugando)
	      }
	      
	      bucleJuego(intercambiarJewels(tablero, x, y, direccion, anchura, altura, seleccion),anchura,altura,dificultad,seleccion,jugando)
	    }
	  }
	  
	  //bucleJuego(tablero,anchura,altura,dificultad,seleccion,jugando)
	}
	
	println("Anchura del tablero: ")
	val anchura=scala.io.StdIn.readInt()
	
	println("Altura del tablero: ")
	val altura=scala.io.StdIn.readInt()
  
	println("Dificultad    1.-Facil    2.-Medio    3.-Dificil ")
	val dificultad=scala.io.StdIn.readInt()
	
	println("¿Automatico?    1.-Si    2.-No ")
	val seleccion=scala.io.StdIn.readInt()
	
	//Se inicializa el tablero
	val tablero = initTablero(anchura*altura,dificultad)
	
	val jugando = true
	
	bucleJuego(tablero, anchura, altura, dificultad, seleccion, jugando)
}