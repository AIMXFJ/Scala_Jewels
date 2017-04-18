import javax.swing.ImageIcon
import scala.swing._
import scala.swing.ScrollPane
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import javax.swing.JOptionPane

object TableroGrafico extends SimpleSwingApplication {
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
	
	def analizarPosicion(tablero:List[Int], pisados:List[Int],anchura:Int,altura:Int,x:Int,y:Int,x_ant:Int,y_ant:Int,valor:Int):(List[Int],List[Int]) = {
	  //println("Analizar pos: " + x + " " + y + " ant: " + x_ant + " " + y_ant + " valor: " + valor)
	  //if(x>=0 && y >=0 && y<altura && x<anchura)
	  //println("Valor de la posicion: " + tablero(x+y*anchura))
	  //printTablero(tablero,0,altura-1,anchura,altura)
	  //println("Pisados:")
	  //printTableroAux(pisados,0,altura-1,anchura,altura)
	  if((x != x_ant || y != y_ant) && x>=0 && y>=0 && x<anchura && y < altura && x+y*anchura < anchura * altura && x+y*anchura >= 0 && pisados(x+y*anchura) == 1 && tablero(x+y*anchura) == valor){
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
  	    
  	    //println("!!!!!!derecha: " + derecha._1.size + " izq: " + izquierda._1.size + " arr: " + arriba._1.size + " debaj: " + abajo._1.size)
  	    
  	    if(derecha._1.size + izquierda._1.size >= 2){
  	      val list = derecha._1 ::: izquierda._1 ::: arriba._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil
  	      //println("Inicio (" + x +","+y+")" + " Resultado: " + list)
  	      return (list, abajo._2)
  	    }
  	    if(arriba._1.size + abajo._1.size >= 2){
  	      val list = derecha._1 ::: izquierda._1 ::: arriba._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil
  	      //println("Inicio (" + x +","+y+")" + " Resultado: " + list)
  	      return (list,abajo._2)
  	    }
  	    //println("Sin resultado")
  	    return (Nil,abajo._2)
	    }
	    if(x - x_ant > 0){ //Poda Izquierda
	      val derecha = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x+1,y,x,y,valor)
  	    val izquierda = Nil
  	    val arriba = analizarPosicion(tablero,derecha._2,anchura,altura,x,y+1,x,y,valor)
  	    val abajo = analizarPosicion(tablero,arriba._2,anchura,altura,x,y-1,x,y,valor)
  	    //printList(derecha ::: izquierda ::: arriba ::: abajo ::: x :: Nil ::: y :: Nil)
  	    return (derecha._1 ::: arriba._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil,abajo._2)
	    }
	    if(x - x_ant < 0){ //Poda Derecha
	      val derecha = Nil
  	    val izquierda = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x-1,y,x,y,valor)
  	    val arriba = analizarPosicion(tablero,izquierda._2,anchura,altura,x,y+1,x,y,valor)
  	    val abajo = analizarPosicion(tablero,arriba._2,anchura,altura,x,y-1,x,y,valor)
  	    return (izquierda._1 ::: arriba._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil,abajo._2)
	    }
	    if(y - y_ant > 0){ //Poda Abajo
	      val derecha = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x+1,y,x,y,valor)
  	    val izquierda = analizarPosicion(tablero,derecha._2,anchura,altura,x-1,y,x,y,valor)
  	    val arriba = analizarPosicion(tablero,izquierda._2,anchura,altura,x,y+1,x,y,valor)
  	    val abajo = Nil
  	    return (derecha._1 ::: izquierda._1 ::: arriba._1 ::: x :: Nil ::: y :: Nil,arriba._2)
	    }
	    if(y - y_ant < 0){ //Poda Arriba
	      val derecha = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x+1,y,x,y,valor)
  	    val izquierda = analizarPosicion(tablero,derecha._2,anchura,altura,x-1,y,x,y,valor)
  	    val arriba = Nil
  	    val abajo = analizarPosicion(tablero,izquierda._2,anchura,altura,x,y-1,x,y,valor)
  	    return (derecha._1 ::: izquierda._1 ::: abajo._1 ::: x :: Nil ::: y :: Nil,abajo._2)
	    }
	    //println("No poda")
	    return (Nil,pisados)
	  }else{
	    //println("Fuera de posicion")
	    return (Nil,pisados)
	  }
	}
	
	//Intercambia 2 elementos en el tablero, para ello se necesita el primer elemento y ambas posiciones
	def swap(tablero:List[Int], pos1:Int, pos2:Int, elem1:Int):List[Int] = {
	  try{
		  introducirElemento(elem1,pos2,introducirElemento(tablero(pos2),pos1,tablero))
		} catch {
         case ex: IndexOutOfBoundsException =>{
           //println("Out of bounds!")
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
	
	def getGrupoJewels(jewelsParaEliminar:List[Int], x_ant:Int, y_ant:Int):List[Int] = {
	  if(!jewelsParaEliminar.isEmpty){
	    if(x_ant == jewelsParaEliminar(0)){
	      return jewelsParaEliminar(0) :: Nil ::: jewelsParaEliminar(1) :: Nil ::: getGrupoJewels(jewelsParaEliminar.tail.tail, jewelsParaEliminar(0),jewelsParaEliminar(1))
	    }else{
	      return Nil
	    }
	  }else{
	    return Nil
	  }
	}
	
	//Eliminar jewels y calcular puntuacion 
	def eliminarJewels(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, jewelsParaEliminar:List[Int], puntos:Int, conjuntos:Int, canvas:Canvas,labelPuntos:Label,labelConjuntos:Label):Unit = {
	  if(!jewelsParaEliminar.isEmpty){
  	  //printList(jewelsParaEliminar)
	    //printTableroAux(jewelsParaEliminar,0,0,jewelsParaEliminar.size,1)
  	  val jewelsAEliminar = jewelsParaEliminar(0) :: Nil ::: jewelsParaEliminar(1) :: Nil ::: getGrupoJewels(jewelsParaEliminar.tail.tail,jewelsParaEliminar(0),jewelsParaEliminar(1))
  	  val newTablero = moverColumnaVertical(tablero, anchura, altura, jewelsAEliminar.reverse(0), jewelsAEliminar.reverse(1), jewelsAEliminar.reverse(0),dificultad,jewelsAEliminar.reverse)
  	  eliminarJewels(newTablero,anchura,altura,dificultad,jewelsParaEliminar.drop(jewelsAEliminar.size),puntos+(jewelsAEliminar.size/2)*25,conjuntos,canvas,labelPuntos,labelConjuntos)
	  }else{
	    bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos+1,canvas,labelPuntos,labelConjuntos)
	  }
	}
	
	//Analiza si la jewel que se ha movido a la posicion de destino permite eliminar jewels, cuantas y cuales y llama a eliminarlas
	def analisisManual(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int, valor:Int, puntos:Int, conjuntos:Int, canvas:Canvas,labelPuntos:Label,labelConjuntos:Label):Unit = {
		val pisados = initTableroAux(anchura*altura)
	  val jewelsParaEliminar = analizarPosicion(tablero, pisados, anchura, altura, x, y,-50,-50, valor)
		
		/*if(jewelsParaEliminar!=Nil)
		  printList(jewelsParaEliminar)
		else
		  println("Resultado no cumple")*/
		  
		if(jewelsParaEliminar._1 == Nil || tamañoLista(jewelsParaEliminar._1) < 3) {bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)}
		else {
		  //println("ELIMINAR")
		  //println("Antes de eliminar____________")
		  //printTablero(tablero,0,altura-1,anchura,altura)
			eliminarJewels(tablero,anchura,altura,dificultad,jewelsParaEliminar._1,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
		}
	}
	
	//Funcion que se ocupa de intercambiar con la jewel indicada y realizar llamar al analisis manual
	def intercambiarJewels(tablero:List[Int], pos1_x:Int, pos1_y:Int, direccion:Int, anchura:Int, altura:Int, puntos:Int, conjuntos:Int, canvas:Canvas,labelPuntos:Label,labelConjuntos:Label):Unit = {
		direccion match {
		//analisisManual recibe como tablero el resultante del swap
		//Arriba, si no se sale del tablero lo hace
			case 1 => { if(pos1_x + (pos1_y+1)*anchura < anchura*altura) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y+1)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x, pos1_y+1, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
			}
			}
		//Abajo
			case 2 => { if(pos1_x + (pos1_y-1)*anchura >= 0) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y-1)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x, pos1_y-1, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
			}
			}
		//Izquierda
			case 3 => { if(pos1_x-1 + (pos1_y)*anchura < anchura*altura) { 
			  analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x-1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x+1, pos1_y, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
			}
			}
		//Derecha
			case 4=> { if(pos1_x+1 + (pos1_y)*anchura >= 0) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x+1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x+1, pos1_y, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
			}
			}
		}
	}
	
	def analisisAutomaticoRec(tablero:List[Int], aux:List[Int],aux_dir:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int):(List[Int],List[Int]) = {
	  if(x+y*anchura < altura*anchura) {
	    //println("AutomaticoRec x: " + x + " y: " + y)
  	  val pisados = initTableroAux(anchura*altura)
  	  
  	  val eliminados_arriba = analizarPosicion(swap(tablero, x + y*anchura, x + (y+1)*anchura, tablero(x + y*anchura)),pisados,anchura,altura,x,y+1,-50,-50,tablero(x+y*anchura))
  	  val eliminados_abajo = analizarPosicion(swap(tablero, x + y*anchura, x + (y-1)*anchura, tablero(x + y*anchura)),pisados,anchura,altura,x,y-1,-50,-50,tablero(x+y*anchura))
  	  val eliminados_derecha = analizarPosicion(swap(tablero, x + y*anchura, x+1 + (y)*anchura, tablero(x + y*anchura)),pisados,anchura,altura,x+1,y,-50,-50,tablero(x+y*anchura))
  	  val eliminados_izquierda = analizarPosicion(swap(tablero, x + y*anchura, x-1 + (y)*anchura, tablero(x + y*anchura)),pisados,anchura,altura,x-1,y,-50,-50,tablero(x+y*anchura))
  	  val lista = List(eliminados_arriba._1.size,eliminados_abajo._1.size,eliminados_izquierda._1.size,eliminados_derecha._1.size)
  	  
  	  //println("Lista entera:")
  	  //printList(lista)
  	  //println("Tablero auxiliar rec")
  	  //printTableroAux(aux,0,altura-1,anchura,altura)
  	  //println("Tablero auxiliar direccion")
  	  //printTableroAux(aux_dir,0,altura-1,anchura,altura)
  	  //println("PosicionAnalisisRec: (" + x + "," + y + ") arriba: " + tamañoLista(eliminados_arriba._1) + " abajo: " + tamañoLista(eliminados_abajo._1) + " izquierda: " + tamañoLista(eliminados_izquierda._1) + " derecha: " + tamañoLista(eliminados_derecha._1))
  	  
  	  
  	  if(x < anchura-1){
  	    analisisAutomaticoRec(tablero,introducirElemento(lista.zipWithIndex.maxBy(_._1)._1, x+y*anchura, aux),introducirElemento(lista.zipWithIndex.maxBy(_._1)._2+1, x+y*anchura, aux_dir),dificultad,anchura,altura,x+1,y)
  	  }else{
  	    analisisAutomaticoRec(tablero,introducirElemento(lista.zipWithIndex.maxBy(_._1)._1, x+y*anchura, aux),introducirElemento(lista.zipWithIndex.maxBy(_._1)._2+1, x+y*anchura, aux_dir),dificultad,anchura,altura,0,y+1)
  	  }
    } else {
	    //println("Tablero auxiliar FINAL VALORES")
  	  //printTableroAux(aux,0,altura-1,anchura,altura)
  	  //println("Tablero auxiliar FINAL DIR")
  	  //printTableroAux(aux_dir,0,altura-1,anchura,altura)
	    return (aux,aux_dir)
	  }
	}
	
	//Optiene el mejor movimiento y lo ejecuta
	def analizarMejorOpcion(aux_tuple:(List[Int],List[Int]), x:Int, y:Int, x_mejor:Int, y_mejor:Int, valor_mejor:Int, dir_mejor:Int, tablero:List[Int], anchura:Int, altura:Int, puntos:Int, conjuntos:Int,canvas:Canvas, labelPuntos:Label, labelConjuntos:Label):Unit = {
	  //Se busca la mejor opcion
	  //println("______Aux valores______")
	  //printTableroAux(aux_tuple._1,0,altura-1,anchura,altura)
	  //printList(aux_tuple._1)
	  //println("______Aux direcciones______")
	  //printList(aux_tuple._1)
	  //printTableroAux(aux_tuple._2,0,altura-1,anchura,altura)
	  //println("X: " + x + " Y: " + y + " X mejor: " + x_mejor + " Y mejor: " + y_mejor + " Dir mejor: " + dir_mejor + " Valor mejor: " + valor_mejor)
	  if(x+y*anchura < altura*anchura) {
  	  if(x < anchura-1){
    	  if(aux_tuple._1(x+y*anchura)>=2 && aux_tuple._1(x+y*anchura) > valor_mejor){
    	    analizarMejorOpcion(aux_tuple, x+1, y, x, y, aux_tuple._1(x+y*anchura),aux_tuple._2(x+y*anchura),tablero,anchura,altura,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	      }else{
	        analizarMejorOpcion(aux_tuple, x+1, y, x_mejor, y_mejor, valor_mejor,dir_mejor,tablero,anchura,altura,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	      }
  	  }else{
  	    if(aux_tuple._1(x+y*anchura)>=2 && aux_tuple._1(x+y*anchura) > valor_mejor){
    	    analizarMejorOpcion(aux_tuple, 0, y+1, x, y, aux_tuple._1(x+y*anchura),aux_tuple._2(x+y*anchura),tablero,anchura,altura,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	      }else{
	        analizarMejorOpcion(aux_tuple, 0, y+1, x_mejor, y_mejor, valor_mejor,dir_mejor,tablero,anchura,altura,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	      }
  	  }
	  }else{
	    //Ya se ha obtenido la mejor opcion
	    //println("Mejor -> x: " + x_mejor + " y: " + y_mejor + " direccion: " + dir_mejor)
	     intercambiarJewels(tablero, x_mejor, y_mejor, dir_mejor, anchura, altura,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	  }
	}
	
	def analisisAutomatico(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int, puntos:Int, conjuntos:Int,canvas:Canvas, labelPuntos:Label, labelConjuntos:Label):Unit = {
	  val aux = initTableroAux(anchura*altura)
	  val aux_dir = initTableroAux(anchura*altura)
	  //printTablero(analisisAutomaticoRec(tablero, aux, dificultad, anchura, altura, 0, 0),0,altura-1,anchura,altura)
	  analizarMejorOpcion(analisisAutomaticoRec(tablero, aux, aux_dir, dificultad, anchura, altura, 0, 0), 0, 0, 0, 0, 0, 4, tablero, anchura, altura, puntos, conjuntos,canvas,labelPuntos,labelConjuntos)
	}
	
	def bucleJuego(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, seleccion:Int, puntos:Int, conjuntos:Int, canvas:Canvas, labelPuntos:Label, labelConjuntos:Label):Unit = {
	  //println("Puntos: " + puntos)
	  //println("Numero de conjuntos eliminados: " + conjuntos)
	  //printTablero(tablero,0,altura-1,anchura,altura)
	  //println("________________________________")
	  canvas.jewels = tablero
	  canvas.repaint()
	  labelPuntos.text = puntos.toString()
	  labelConjuntos.text = conjuntos.toString()
	  
	  seleccion match {
	    case 1 => {
	      println("Elige una acción:\n   1.-Mejor Jugada\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir")
    	  val opcionElegida = scala.io.StdIn.readInt()
	      //val opcionElegida =  JOptionPane.showInputDialog("Elige una acción:\n   1.-Mejor Jugada\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir").toInt
    	      
    	  //Opciones 2, 3 no hacen nada de momento, solo continuan el bucle del juego, 0 sale del programa
    	  opcionElegida match {
    	    case 0 => System.exit(1)
    	     case 1 => analisisAutomatico(tablero, dificultad, anchura, altura,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
    	     case 2 => {bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)}
    	     case 3 => {bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)}
    	     case _ => bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
    	  }
	    }
	    case 2 => {
	      println("Elige una acción:\n   1.-Intercambiar Jewel\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir")
	      val opcionElegida = scala.io.StdIn.readInt()
	      //val opcionElegida =  JOptionPane.showInputDialog("Elige una acción:\n   1.-Intercambiar Jewel\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir").toInt
	      
	      //Opciones 2, 3 no hacen nada de momento, solo continuan el bucle del juego, 0 sale del programa
	      opcionElegida match {
	        case 0 => System.exit(1)
	        case 1 => {  //Intercambio de la jewel a elegir
	          println("Seleccione la jewel a intercambiar")
	          //JOptionPane.showMessageDialog(new java.awt.Label("Elige una acción:\n   1.-Mejor Jugada\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir"),null)
	      
    	      println("X->")
    	      val x=scala.io.StdIn.readInt()
    	      
    	      println("Y->")
    	      val y=scala.io.StdIn.readInt()
    	      
    	      println("Direccion del intercambio:\n   1.-Arriba\n   2.-Abajo\n   3.-Izquierda\n   4.-Derecha")
    	      val direccion=scala.io.StdIn.readInt()
	          //val x =  JOptionPane.showInputDialog("X:").toInt
	          //val y =  JOptionPane.showInputDialog("Y:").toInt
	          //val direccion =  JOptionPane.showInputDialog("Direccion:").toInt
    	      
    	      if(direccion < 1 && direccion > 4){
    	        println("¡DIRECCION ERRONEA!")
    	        bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
    	      }
    	      
    	      intercambiarJewels(tablero, x, y, direccion, anchura, altura, puntos, conjuntos,canvas,labelPuntos,labelConjuntos)
	        }
	        case 2 => {
	          bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	        }
	        case 3 => {
	          bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	        }
	      }
	    }
	    case _ => bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	  }
	  
	}
  
	//val dificultad =  JOptionPane.showInputDialog("Dificultad (1,2 o 3):").toInt
  //val seleccion =  JOptionPane.showInputDialog("Automatico (1) o manual (2)").toInt
	
	println("Dificultad:\n    1.-Facil\n    2.-Medio\n    3.-Dificil ")
	val dificultad=scala.io.StdIn.readInt()
	
	println("Modo:\n    1.-Automatico\n    2.-Manual ")
	val seleccion=scala.io.StdIn.readInt()
	
  val anchura = dificultad match {
	  case 1 => 7
	  case 2 => 11
	  case 3 => 15
	}
	
  val altura = dificultad match {
	  case 1 => 9
	  case 2 => 17
	  case 3 => 27
	}
  
  val tablero = initTablero(anchura*altura,dificultad)
  
  val canvas = new Canvas(anchura,altura) {
    preferredSize = new Dimension(1200, 1200)
  }
  
  val labelPuntos = new Label {
    text = "0"
    font = new Font("Ariel", java.awt.Font.ITALIC, 24)
  }
  
  val labelConjuntos = new Label {
    text = "0"
    font = new Font("Ariel", java.awt.Font.ITALIC, 24)
  }
	
	def top = new MainFrame { // top is a required method
    title = "Jewels Interface"

    val labelPuntosTxt = new Label {
      text = "Puntos: "
      font = new Font("Ariel", java.awt.Font.ITALIC, 24)
    }
    
    val labelConjuntosTxt = new Label {
      text = "Conjuntos: "
      font = new Font("Ariel", java.awt.Font.ITALIC, 24)
    }
    
    canvas.jewels = tablero
    
    val gridPanel = new GridPanel(1, 4) {
      contents += labelConjuntosTxt
      contents += labelConjuntos
      contents += labelPuntosTxt
      contents += labelPuntos
    }
    
    val scrollPane = new ScrollPane {
      contents = canvas
    }

    // choose a top-level Panel and put components in it
    // Components may include other Panels
    contents = new BorderPanel {
      layout(gridPanel) = North
      layout(scrollPane) = Center
    }
    size = new Dimension(1000, 500)
    menuBar = new MenuBar {
      contents += new Menu("Opciones") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }

    // specify which Components produce events of interest
    listenTo(canvas.mouse.clicks)
    
    visible = true

    // react to events
    reactions += {
      case MouseClicked(_, point, _, _, _) =>
        repaint()
    }
  }
	this.top
  bucleJuego(tablero, anchura, altura, dificultad,seleccion,0,0,canvas,labelPuntos,labelConjuntos)
}