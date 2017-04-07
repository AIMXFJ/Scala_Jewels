

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
		
	def printTableroAux(tablero:List[Int], x:Int, y:Int, anchura:Int, altura:Int):Unit = {		
		  if(x >= 0 && x < anchura-1){
		    print(" | " + tablero(x+y*anchura))
		    printTableroAux(tablero, x+1, y, anchura, altura)
		  }else if (x==anchura-1 && y>=0) {
		    print(" | " +tablero(x+y*anchura)+" | ")
		    println()
		    if(y>0)
	        printTableroAux(tablero, 0, y-1, anchura, altura)
		  }
	}

	def printTablero(tablero:List[Int], x:Int, y:Int, anchura:Int, altura:Int):Unit = {
	  if(x >= 0 && x < anchura-1){
	    tablero(x+y*anchura) match {
	      case 1 => print("A ")
	      case 2 => print("R ")
	      case 3 => print("N ")
	      case 4 => print("V ")
	      case 5 => print("P ")
	      case 6 => print("M ")
	      case 7 => print("G ")
	      case 8 => print("B ")
	    }
	    printTablero(tablero, x+1, y, anchura, altura)
	  }else if (x==anchura-1 && y>=0) {
	    tablero(x+y*anchura) match {
	      case 1 => print("A")
	      case 2 => print("R")
	      case 3 => print("N")
	      case 4 => print("V")
	      case 5 => print("P")
	      case 6 => print("M")
	      case 7 => print("G")
	      case 8 => print("B")
	    }
	    println()
	    if(y>0)
	      printTablero(tablero, 0, y-1, anchura, altura)
	  }
	}
	
	def tamañoLista(lista:List[Int]):Int = {
	  if(lista.tail.isEmpty)
	    1
	  else
	    1 + tamañoLista(lista.tail)
	}
  
  //Genera un numero aleatorio en funcion de la dificultad
  def numRandom(dificultad:Int): Int =
  	if(dificultad == 1)(1+Random.nextInt(4))
  	else if(dificultad == 2)(1+Random.nextInt(5))
  	else if(dificultad == 3)(1+Random.nextInt(7))
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

	
	def analizarPosicion(tablero:List[Int], pisados:List[Int],anchura:Int,altura:Int,x:Int,y:Int,x_ant:Int,y_ant:Int,valor:Int):(List[Int],List[Int]) = {
	  println("Analizar pos: " + x + " " + y + " ant: " + x_ant + " " + y_ant + " valor: " + valor)
	  //println("Pisados:")
	  //printTableroAux(pisados,0,altura-1,anchura,altura)
	  if((x != x_ant || y != y_ant) && x>=0 && y>=0 && x+y*anchura < anchura * altura && x+y*anchura >= 0 && pisados(x+y*anchura) == 1 && tablero(x+y*anchura) == valor){
	    if(x_ant == -50 && y_ant == -50){ //Inicial
	      //println("Inicial, " + x + " " + y)
	      val derecha = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x+1,y,x,y,valor)
	      //printTableroAux(derecha._2,0,altura-1,anchura,altura)
  	    val izquierda = analizarPosicion(tablero,derecha._2,anchura,altura,x-1,y,x,y,valor)
  	    //printTableroAux(izquierda._2,0,altura-1,anchura,altura)
  	    val arriba = analizarPosicion(tablero,izquierda._2,anchura,altura,x,y+1,x,y,valor)
  	    //printTableroAux(arriba._2,0,altura-1,anchura,altura)
  	    val abajo = analizarPosicion(tablero,arriba._2,anchura,altura,x,y-1,x,y,valor)
  	    //printTableroAux(abajo._2,0,altura-1,anchura,altura)
  	    
  	    //println("derecha: " + derecha.size + " izq: " + izquierda.size + " arr: " + arriba.size + " debaj: " + abajo.size)
  	    
  	    if(derecha._1.size + izquierda._1.size >= 2){
  	      val list = derecha._1 ::: izquierda._1 ::: arriba._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil
  	      return (list, abajo._2)
  	    }
  	    if(arriba._1.size + abajo._1.size >= 2){
  	      val list = derecha._1 ::: izquierda._1 ::: arriba._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil
  	      return (list,abajo._2)
  	    }
  	    return (Nil,abajo._2)
	    }
	    if(x - x_ant > 0){ //Poda Izquierda
	      val derecha = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x+1,y,x,y,valor)
  	    val izquierda = Nil
  	    val arriba = analizarPosicion(tablero,derecha._2,anchura,altura,x,y+1,x,y,valor)
  	    val abajo = analizarPosicion(tablero,arriba._2,anchura,altura,x,y-1,x,y,valor)
  	    //printList(derecha ::: izquierda ::: arriba ::: abajo ::: x :: Nil ::: y :: Nil)
  	    return (derecha._1 ::: izquierda ::: arriba._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil,abajo._2)
	    }
	    if(x - x_ant < 0){ //Poda Derecha
	      val derecha = Nil
  	    val izquierda = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x-1,y,x,y,valor)
  	    val arriba = analizarPosicion(tablero,izquierda._2,anchura,altura,x,y+1,x,y,valor)
  	    val abajo = analizarPosicion(tablero,arriba._2,anchura,altura,x,y-1,x,y,valor)
  	    return (derecha ::: izquierda._1 ::: arriba._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil,abajo._2)
	    }
	    if(y - y_ant > 0){ //Poda Abajo
	      val derecha = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x+1,y,x,y,valor)
  	    val izquierda = analizarPosicion(tablero,derecha._2,anchura,altura,x-1,y,x,y,valor)
  	    val arriba = analizarPosicion(tablero,izquierda._2,anchura,altura,x,y+1,x,y,valor)
  	    val abajo = Nil
  	    return (derecha._1 ::: izquierda._1 ::: arriba._1 ::: abajo ::: x :: Nil ::: y :: Nil,arriba._2)
	    }
	    if(y - y_ant < 0){ //Poda Arriba
	      val derecha = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x+1,y,x,y,valor)
  	    val izquierda = analizarPosicion(tablero,derecha._2,anchura,altura,x-1,y,x,y,valor)
  	    val arriba = Nil
  	    val abajo = analizarPosicion(tablero,izquierda._2,anchura,altura,x,y-1,x,y,valor)
  	    return (derecha._1 ::: izquierda._1 ::: arriba ::: abajo._1 ::: x :: Nil ::: y :: Nil,abajo._2)
	    }
	    return (Nil,pisados)
	  }else{
	    return (Nil,pisados)
	  }
	}
	
	//Intercambia 2 elementos en el tablero, para ello se necesita el primer elemento y ambas posiciones
	def swap(tablero:List[Int], pos1:Int, pos2:Int, elem1:Int):List[Int] = {
	  try{
		  introducirElemento(elem1,pos2,introducirElemento(tablero(pos2),pos1,tablero))
		} catch {
         case ex: IndexOutOfBoundsException =>{
           tablero
         }
      }
	}
	
	//Baja la columna eliminando de forma vertical
	def moverColumnaVertical(tablero:List[Int], anchura:Int, altura:Int, init_y:Int, x:Int, y:Int, dificultad:Int, jewelsParaEliminar:List[Int]):List[Int] = {
	  //println("x: " + x + " y: " + y)
	  //println("jewels0: " + jewelsParaEliminar(0))
	  if(y+1<altura){
	    //println("SWAP")
	    moverColumnaVertical(swap(tablero, x+y*anchura,x+(y+1)*anchura,tablero(x+y*anchura)),anchura,altura,init_y,x,y+1,dificultad,jewelsParaEliminar)
	  }else if(!jewelsParaEliminar.tail.tail.isEmpty){
	    //println("Fin Iteracion, continua")
	    moverColumnaVertical(introducirElemento(numRandom(dificultad), x+y*anchura, tablero),anchura,altura,init_y,jewelsParaEliminar.tail.tail(1),init_y,dificultad,jewelsParaEliminar.tail.tail)
	  }else{
	    //println("FIN")
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
	
	//Eliminar jewels y calcular puntuacion 
	def eliminarJewels(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, jewelsParaEliminar:List[Int], puntos:Int, conjuntos:Int):Unit = {
	  printList(jewelsParaEliminar)
	  //Horizontal
	  if(jewelsParaEliminar(1)==jewelsParaEliminar(3)){
	    //println("HORIZONTAL")
	    bucleJuego(moverColumnas(tablero, anchura, altura, jewelsParaEliminar(1), jewelsParaEliminar(0), jewelsParaEliminar(1),dificultad,jewelsParaEliminar),anchura,altura,dificultad,seleccion,(tamañoLista(jewelsParaEliminar)/2)*25+puntos,conjuntos+1)
  	//Vertical
	  }else{
	    //println("VERTICAL")
	    bucleJuego(moverColumnaVertical(tablero, anchura, altura, jewelsParaEliminar.reverse(0), jewelsParaEliminar.reverse(1), jewelsParaEliminar.reverse(0),dificultad,jewelsParaEliminar.reverse),anchura,altura,dificultad,seleccion,(tamañoLista(jewelsParaEliminar)/2)*25+puntos,conjuntos+1)
	  }
	}
	
	//Analiza si la jewel que se ha movido a la posicion de destino permite eliminar jewels, cuantas y cuales y llama a eliminarlas
	def analisisManual(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int, valor:Int, puntos:Int, conjuntos:Int):Unit = {
		val pisados = initTableroAux(anchura*altura)
	  val jewelsParaEliminar = analizarPosicion(tablero, pisados, anchura, altura, x, y,-50,-50, valor)
		
		/*if(jewelsParaEliminar!=Nil)
		  printList(jewelsParaEliminar)
		else
		  println("Resultado no cumple")*/
		  
		if(jewelsParaEliminar._1 == Nil || tamañoLista(jewelsParaEliminar._1) < 3) {bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos)}
		else {
		  //println("ELIMINAR")
			eliminarJewels(tablero,anchura,altura,dificultad,jewelsParaEliminar._1,puntos,conjuntos)
		}
	}
	
	//Funcion que se ocupa de intercambiar con la jewel indicada y realizar llamar al analisis manual
	def intercambiarJewels(tablero:List[Int], pos1_x:Int, pos1_y:Int, direccion:Int, anchura:Int, altura:Int, puntos:Int, conjuntos:Int):Unit = {
		direccion match {
		//analisisManual recibe como tablero el resultante del swap
		//Arriba, si no se sale del tablero lo hace
			case 1 => { if(pos1_x + (pos1_y+1)*anchura < anchura*altura) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y+1)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x, pos1_y+1, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos)
			}
			}
		//Abajo
			case 2 => { if(pos1_x + (pos1_y-1)*anchura >= 0) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y-1)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x, pos1_y-1, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos)
			}
			}
		//Izquierda
			case 3 => { if(pos1_x-1 + (pos1_y)*anchura < anchura*altura) { 
			  analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x-1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x-1, pos1_y, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos)
			}
			}
		//Derecha
			case 4=> { if(pos1_x+1 + (pos1_y)*anchura >= 0) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x+1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x+1, pos1_y, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos)
			}
			}
		}
	}
	
	//Solo mira directamente a la derecha -> Solucion: Que mueva con minimo 2 en vez de 3
	def analisisAutomaticoRec(tablero:List[Int], aux:List[Int],aux_dir:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int):(List[Int],List[Int]) = {
	  //analizarPosicion(tablero,anchura,altura,x,y,-50,-50,valor)
	  /*if(x+y*anchura < altura*anchura) {
  	  if(x < anchura-1)
  	    analisisAutomaticoRec(tablero,introducirElemento(cuantasJewelsDerecha(tablero, anchura, altura, x+1, y, tablero(x+y*anchura), 0)+1,x+1+y*anchura,aux),dificultad,anchura,altura,x+1,y)
  	  else
  	    analisisAutomaticoRec(tablero,introducirElemento(cuantasJewelsDerecha(tablero, anchura, altura, x+1, y, tablero(x+y*anchura), 0)+1,x+1+y*anchura,aux),dificultad,anchura,altura,0,y+1)
	  } else
	      aux*/
	  if(x+y*anchura < altura*anchura) {
	    //println("AutomaticoRec x: " + x + " y: " + y)
  	  val copia_tablero = tablero
  	  val pisados = initTableroAux(anchura*altura)
  	  //printTableroAux(copia_tablero,0,altura-1,anchura,altura)
  	  val eliminados_arriba = analizarPosicion(swap(copia_tablero, x + y*anchura, x + (y+1)*anchura, copia_tablero(x + y*anchura)),introducirElemento(0,x+y*anchura,pisados),anchura,altura,x,y,-50,-50,copia_tablero(x+y*anchura))
  	  val eliminados_abajo = analizarPosicion(swap(copia_tablero, x + y*anchura, x + (y-1)*anchura, copia_tablero(x + y*anchura)),eliminados_arriba._2,anchura,altura,x,y,-50,-50,copia_tablero(x+y*anchura))
  	  val eliminados_derecha = analizarPosicion(swap(copia_tablero, x + y*anchura, x+1 + (y)*anchura, copia_tablero(x + y*anchura)),eliminados_abajo._2,anchura,altura,x,y,-50,-50,copia_tablero(x+y*anchura))
  	  val eliminados_izquierda = analizarPosicion(swap(copia_tablero, x + y*anchura, x-1 + (y)*anchura, copia_tablero(x + y*anchura)),eliminados_derecha._2,anchura,altura,x,y,-50,-50,copia_tablero(x+y*anchura))
  	  val lista = List(eliminados_arriba._1.size,eliminados_abajo._1.size,eliminados_izquierda._1.size,eliminados_derecha._1.size)
  	  
  	  if(x < anchura-1)
  	    analisisAutomaticoRec(tablero,introducirElemento(lista.zipWithIndex.maxBy(_._1)._1, x+y*anchura, aux),introducirElemento(lista.zipWithIndex.maxBy(_._1)._2+1, x+y*anchura, aux_dir),dificultad,anchura,altura,x+1,y)
  	  else
  	    analisisAutomaticoRec(tablero,introducirElemento(lista.zipWithIndex.maxBy(_._1)._1, x+y*anchura, aux),introducirElemento(lista.zipWithIndex.maxBy(_._1)._2+1, x+y*anchura, aux_dir),dificultad,anchura,altura,0,y+1)
	  } else {
	    return (aux,aux_dir)
	  }
	}
	
	//Optiene el mejor movimiento y lo ejecuta
	def analizarMejorOpcion(aux_tuple:(List[Int],List[Int]), x:Int, y:Int, x_mejor:Int, y_mejor:Int, valor_mejor:Int, dir_mejor:Int, tablero:List[Int], anchura:Int, altura:Int, puntos:Int, conjuntos:Int):Unit = {
	  //Se busca la mejor opcion
	  //println("______Aux valores______")
	  //printTableroAux(aux_tuple._1,0,altura-1,anchura,altura)
	  //println("______Aux direcciones______")
	  //printTableroAux(aux_tuple._2,0,altura-1,anchura,altura)
	  
	  if(x+y*anchura < altura*anchura) {
  	  if(x < anchura-1){
    	  if(aux_tuple._1(x+y*anchura)>=2 && aux_tuple._1(x+y*anchura) > valor_mejor){
    	    analizarMejorOpcion(aux_tuple, x+1, y, x, y, aux_tuple._1(x+y*anchura),aux_tuple._2(x+y*anchura),tablero,anchura,altura,puntos,conjuntos)
	      }else{
	        analizarMejorOpcion(aux_tuple, x+1, y, x_mejor, y_mejor, valor_mejor,dir_mejor,tablero,anchura,altura,puntos,conjuntos)
	      }
  	  }else{
  	    if(aux_tuple._1(x+y*anchura)>=2 && aux_tuple._1(x+y*anchura) > valor_mejor){
    	    analizarMejorOpcion(aux_tuple, 0, y+1, x, y, aux_tuple._1(x+y*anchura),aux_tuple._2(x+y*anchura),tablero,anchura,altura,puntos,conjuntos)
	      }else{
	        analizarMejorOpcion(aux_tuple, 0, y+1, x_mejor, y_mejor, valor_mejor,dir_mejor,tablero,anchura,altura,puntos,conjuntos)
	      }
  	  }
	  }else{
	    //Ya se ha obtenido la mejor opcion
	     intercambiarJewels(tablero, x_mejor, y_mejor, dir_mejor, anchura, altura,puntos,conjuntos)
	  }
	}
	
	//Busca el mejor movimiento hacia la derecha empleando una estructura auxiliar para guardar cuantas jewels se eliminarian
	def analisisAutomatico(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int, puntos:Int, conjuntos:Int):Unit = {
	  val aux = initTableroAux(anchura*altura)
	  val aux_dir = initTableroAux(anchura*altura)
	  //printTablero(analisisAutomaticoRec(tablero, aux, dificultad, anchura, altura, 0, 0),0,altura-1,anchura,altura)
	  analizarMejorOpcion(analisisAutomaticoRec(tablero, aux, aux_dir, dificultad, anchura, altura, 0, 0), 0, 0, 0, 0, 0, 4, tablero, anchura, altura, puntos, conjuntos)
	}
	
	def bucleJuego(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, seleccion:Int, puntos:Int, conjuntos:Int):Unit = {
	  println("Puntos: " + puntos)
	  println("Numero de conjuntos eliminados: " + conjuntos)
	  printTablero(tablero,0,altura-1,anchura,altura)
	  println("________________________________")
	  
	  seleccion match {
	    case 1 => {
	      println("Elige una acción:\n   1.-Mejor Jugada\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir")
    	  val opcionElegida = scala.io.StdIn.readInt()
    	      
    	  //Opciones 2, 3 no hacen nada de momento, solo continuan el bucle del juego, 0 sale del programa
    	  opcionElegida match {
    	    case 0 => System.exit(1)
    	     case 1 => analisisAutomatico(tablero, dificultad, anchura, altura,puntos,conjuntos)
    	     case 2 => {bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos)}
    	     case 3 => {bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos)}
    	     case _ => bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos)
    	  }
	    }
	    case 2 => {
	      println("Elige una acción:\n   1.-Intercambiar Jewel\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir")
	      val opcionElegida = scala.io.StdIn.readInt()
	      
	      //Opciones 2, 3 no hacen nada de momento, solo continuan el bucle del juego, 0 sale del programa
	      opcionElegida match {
	        case 0 => System.exit(1)
	        case 1 => {  //Intercambio de la jewel a elegir
	          println("Seleccione la jewel a intercambiar")
	      
    	      println("X->")
    	      val x=scala.io.StdIn.readInt()
    	      
    	      println("Y->")
    	      val y=scala.io.StdIn.readInt()
    	      
    	      println("Direccion del intercambio:\n   1.-Arriba\n   2.-Abajo\n   3.-Izquierda\n   4.-Derecha")
    	      val direccion=scala.io.StdIn.readInt()
    	      
    	      if(direccion < 1 && direccion > 4){
    	        println("¡DIRECCION ERRONEA!")
    	        bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos)
    	      }
    	      
    	      intercambiarJewels(tablero, x, y, direccion, anchura, altura, puntos, conjuntos)
	        }
	        case 2 => {
	          bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos)
	        }
	        case 3 => {
	          bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos)
	        }
	      }
	    }
	    case _ => bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos)
	  }
	  
	}
  
	println("Dificultad:\n    1.-Facil\n    2.-Medio\n    3.-Dificil ")
	val dificultad=scala.io.StdIn.readInt()
	
	println("Modo:\n    1.-Automatico\n    2.-Manual ")
	val seleccion=scala.io.StdIn.readInt()
	
	dificultad match {
	  case 1 => {
	    val tablero = initTablero(7*9,dificultad)
	    bucleJuego(tablero, 7, 9, dificultad,seleccion,0,0)
	  }
	  case 2 =>{
	    val tablero = initTablero(11*17,dificultad)
	    bucleJuego(tablero, 11, 17, dificultad,seleccion,0,0)
	  }
	  case 3 =>{
	    val tablero = initTablero(15*27,dificultad)
	    bucleJuego(tablero, 15, 27, dificultad,seleccion,0,0)
	  }
	  case _ => System.exit(-2)
	}
}