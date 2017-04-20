import javax.swing.ImageIcon
import scala.swing._
import scala.swing.ScrollPane
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import javax.swing.JOptionPane
import java.io._
import scala.io.Source

object TableroGrafico extends SimpleSwingApplication {
  //Imprime cualquier lista o variable valida para for each
  def printList(args:List[Int]):Unit = {
      args.foreach(println)
  }
  
  //Función para introducir un numero en una posicion determinada
	def introducirElemento(color:Int, pos:Int, tablero:List[Int]):List[Int] =
		if(tablero.isEmpty) Nil else if (pos==0) color::tablero.tail
		else tablero.head::introducirElemento(color,pos-1,tablero.tail)
		
	//Imprime el tablero
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
	
	//Devuelve el tamaño de una lista
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
  
  //Inicializa un tablero auxiliar a todo 1s
  def initTableroAux (times:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initTableroAux (times-1):::1::Nil
 		}
	}
	
  /** Se encarga de revisar si en torno a (x,y) se producen conjuntos de jewels iguales, es decir,
	 * mira si hay jewels formando grupos.
	 * @param x_ant posición X del paso anterior, para evitar ciclos infinitos podando la rama ya vista del grafo
	 * @param y_ant posición Y del paso anterior, se emplea para podar la rama ya vista del grafo.
	 * @return Devuelve una tupla formada por la lista de jewels que forman grupo y por el auxiliar de hijos (casillas o jewels) ya visitados
	 * */
	def analizarPosicion(tablero:List[Int], pisados:List[Int],anchura:Int,altura:Int,x:Int,y:Int,x_ant:Int,y_ant:Int,valor:Int):(List[Int],List[Int]) = {
	  //println("Analizar pos: " + x + " " + y + " ant: " + x_ant + " " + y_ant + " valor: " + valor)
	  //if(x>=0 && y >=0 && y<altura && x<anchura)
	  //println("Valor de la posicion: " + tablero(x+y*anchura))
	  //printTablero(tablero,0,altura-1,anchura,altura)
	  //println("Pisados:")
	  //printTableroAux(pisados,0,altura-1,anchura,altura)
	  if((x != x_ant || y != y_ant) && x>=0 && y>=0 && x<anchura && y < altura && x+y*anchura < anchura * altura && x+y*anchura >= 0 && pisados(x+y*anchura) == 1 && tablero(x+y*anchura) == valor){
	    if(x_ant == -50 && y_ant == -50){ //Inicial
	      val derecha = analizarPosicion(tablero,introducirElemento(0,x+y*anchura,pisados),anchura,altura,x+1,y,x,y,valor)
  	    val izquierda = analizarPosicion(tablero,derecha._2,anchura,altura,x-1,y,x,y,valor)
  	    val arriba = analizarPosicion(tablero,izquierda._2,anchura,altura,x,y+1,x,y,valor)
  	    val abajo = analizarPosicion(tablero,arriba._2,anchura,altura,x,y-1,x,y,valor)

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
	    return (Nil,pisados)
	  }else{
	    return (Nil,pisados)
	  }
	}
	
	/*Intercambia 2 elementos en el tablero, para ello se necesita el primer elemento y ambas posiciones
	 * En caso de salirse del tablero (por culpa de la funcion que revisa el mejor movimiento) devuelve el
	 * tablero sin modificar */
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
	  if(y+1<altura){
	    moverColumnaVertical(swap(tablero, x+y*anchura,x+(y+1)*anchura,tablero(x+y*anchura)),anchura,altura,init_y,x,y+1,dificultad,jewelsParaEliminar)
	  }else if(!jewelsParaEliminar.tail.tail.isEmpty){
	    moverColumnaVertical(introducirElemento(numRandom(dificultad), x+y*anchura, tablero),anchura,altura,init_y,jewelsParaEliminar.tail.tail(1),init_y,dificultad,jewelsParaEliminar.tail.tail)
	  }else{
	    introducirElemento(numRandom(dificultad), x+y*anchura, tablero)
	  }
	}
	
	/**Se encarga de dividir las jewels a eliminar en grupos eliminables verticalmente, en caso de que sea un grupo horizontal
	 * 		se generarán tantos grupos como jewels lo formen, siendo cada grupo formado por una jewel eliminada como si fuera un grupo
	 * 		vertical, permitiendo eliminar cualquier tipo de grupo de forma generalizada
	 * 
	 * @param jewelsParaEliminar jewes que hay que eliminar
	 * @param x_ant valor X de la jewel anterior, para ver si forman grupo vertical o no
	 * @param y_ant valor Y de la jewel anterior, mismo uso que x_ant
	 * @return Grupo de jewels horizontal/vertical listo para ser eliminado verticalmente*/
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
	
	/*Eliminar jewels y calcular puntuacion. Elimina grupos de jewels hasta que no hay más en la lista. */
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
		  
		if(jewelsParaEliminar._1 == Nil || tamañoLista(jewelsParaEliminar._1) < 3) {bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)}
		else {
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
			  analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x-1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x-1, pos1_y, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
			}
			}
		//Derecha
			case 4=> { if(pos1_x+1 + (pos1_y)*anchura >= 0) {
				analisisManual(swap(tablero, pos1_x + pos1_y*anchura, pos1_x+1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura)), dificultad,anchura, altura, pos1_x+1, pos1_y, tablero(pos1_x + pos1_y*anchura),puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
			}
			}
		}
	}
	
	/**Revisa por cada jewel todos los movimientos posibles, escribiendo en un auxiliar el mejor de los movimientos (cantidad que elimina)
	 * 		y en otro auxiliar el movimiento que debe hacer con la jewel analizada.
	 * 
	 * @param aux tablero auxiliar que guarda cuantas jewels se eliminan con su mejor movimiento
	 * @param aux_dir tablero auxiliar que guarda la dirección hacia la que intercambiar para obtener el valor guardado en "aux" en la misma posición
	 * @return tupla formada por ambos tableros auxiliares
	 * 
	 */
	def analisisAutomaticoRec(tablero:List[Int], aux:List[Int],aux_dir:List[Int], dificultad:Int,anchura:Int, altura:Int, x:Int, y:Int):(List[Int],List[Int]) = {
	  if(x+y*anchura < altura*anchura) {
	    //println("AutomaticoRec x: " + x + " y: " + y)
  	  val pisados = initTableroAux(anchura*altura)
  	  
  	  val eliminados_arriba = analizarPosicion(swap(tablero, x + y*anchura, x + (y+1)*anchura, tablero(x + y*anchura)),pisados,anchura,altura,x,y+1,-50,-50,tablero(x+y*anchura))
  	  val eliminados_abajo = analizarPosicion(swap(tablero, x + y*anchura, x + (y-1)*anchura, tablero(x + y*anchura)),pisados,anchura,altura,x,y-1,-50,-50,tablero(x+y*anchura))
  	  val eliminados_derecha = analizarPosicion(swap(tablero, x + y*anchura, x+1 + (y)*anchura, tablero(x + y*anchura)),pisados,anchura,altura,x+1,y,-50,-50,tablero(x+y*anchura))
  	  val eliminados_izquierda = analizarPosicion(swap(tablero, x + y*anchura, x-1 + (y)*anchura, tablero(x + y*anchura)),pisados,anchura,altura,x-1,y,-50,-50,tablero(x+y*anchura))
  	  val lista = List(eliminados_arriba._1.size,eliminados_abajo._1.size,eliminados_izquierda._1.size,eliminados_derecha._1.size)
  	  
  	  if(x < anchura-1){
  	    analisisAutomaticoRec(tablero,introducirElemento(lista.zipWithIndex.maxBy(_._1)._1, x+y*anchura, aux),introducirElemento(lista.zipWithIndex.maxBy(_._1)._2+1, x+y*anchura, aux_dir),dificultad,anchura,altura,x+1,y)
  	  }else{
  	    analisisAutomaticoRec(tablero,introducirElemento(lista.zipWithIndex.maxBy(_._1)._1, x+y*anchura, aux),introducirElemento(lista.zipWithIndex.maxBy(_._1)._2+1, x+y*anchura, aux_dir),dificultad,anchura,altura,0,y+1)
  	  }
    } else {  //Cuando acaba de analizar
	    return (aux,aux_dir)
	  }
	}
	
	//Optiene el mejor movimiento de la tupla de auxiliares y lo ejecuta
	def analizarMejorOpcion(aux_tuple:(List[Int],List[Int]), x:Int, y:Int, x_mejor:Int, y_mejor:Int, valor_mejor:Int, dir_mejor:Int, tablero:List[Int], anchura:Int, altura:Int, puntos:Int, conjuntos:Int,canvas:Canvas, labelPuntos:Label, labelConjuntos:Label):Unit = {
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
	  }else{  //Cuando acaba de revisar
	    //Ya se ha obtenido la mejor opcion
	    intercambiarJewels(tablero, x_mejor, y_mejor, dir_mejor, anchura, altura,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	  }
	}
	
	//Prepara y ejecuta el analisis recursivo automático
	def analisisAutomatico(tablero:List[Int], dificultad:Int,anchura:Int, altura:Int, puntos:Int, conjuntos:Int,canvas:Canvas, labelPuntos:Label, labelConjuntos:Label):Unit = {
	  val aux = initTableroAux(anchura*altura)
	  val aux_dir = initTableroAux(anchura*altura)
	  analizarMejorOpcion(analisisAutomaticoRec(tablero, aux, aux_dir, dificultad, anchura, altura, 0, 0), 0, 0, 0, 0, 0, 4, tablero, anchura, altura, puntos, conjuntos,canvas,labelPuntos,labelConjuntos)
	}
	
	/*Guarda los datos de la partida actual en archivos separados*/
	def guardar(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, seleccion:Int, puntos:Int, conjuntos:Int, canvas:Canvas, labelPuntos:Label, labelConjuntos:Label):Unit = {
	  val ficheroTablero = new PrintWriter(new File("tablero.txt"))
	  val ficheroAnchura = new PrintWriter(new File("anchura.txt"))
	  val ficheroAltura = new PrintWriter(new File("altura.txt"))
	  val ficheroDificultad = new PrintWriter(new File("dificultad.txt"))
	  val ficheroSeleccion = new PrintWriter(new File("seleccion.txt"))
	  val ficheroPuntos = new PrintWriter(new File("puntos.txt"))
	  val ficheroConjuntos = new PrintWriter(new File("conjuntos.txt"))
	  ficheroTablero.write(tablero.mkString(","))
	  ficheroAnchura.write(anchura.toString())
	  ficheroAltura.write(altura.toString())
	  ficheroDificultad.write(dificultad.toString())
	  ficheroSeleccion.write(seleccion.toString())
	  ficheroPuntos.write(puntos.toString())
	  ficheroConjuntos.write(conjuntos.toString())
	  ficheroTablero.close
	  ficheroAnchura.close
	  ficheroAltura.close
	  ficheroDificultad.close
	  ficheroSeleccion.close
	  ficheroPuntos.close
	  ficheroConjuntos.close
	  println("Datos guardados.")
	  bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos, canvas,labelPuntos,labelConjuntos)
	}
	
	/*Carga los datos y actualiza el estado actual del juego con los mismos.*/
	def cargar(canvas:Canvas, labelPuntos:Label, labelConjuntos:Label):Unit = {
	  val tablero = Source.fromFile("tablero.txt").getLines().toList.head.split(",").map(_.trim).toList.map(_.toInt)
	  val anchura = Source.fromFile("anchura.txt").getLines().toList.head.toInt
	  val altura = Source.fromFile("altura.txt").getLines().toList.head.toInt
	  val dificultad = Source.fromFile("dificultad.txt").getLines().toList.head.toInt
	  val seleccion = Source.fromFile("seleccion.txt").getLines().toList.head.toInt
	  val puntos = Source.fromFile("puntos.txt").getLines().toList.head.toInt
	  val conjuntos = Source.fromFile("conjuntos.txt").getLines().toList.head.toInt
	  println("Partida Cargada.")
	  bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos, canvas,labelPuntos,labelConjuntos)
	}
	
	/*Bucle principal del juego, se actualiza la interfaz al principio de cada iteración*/
	def bucleJuego(tablero:List[Int], anchura:Int, altura:Int, dificultad:Int, seleccion:Int, puntos:Int, conjuntos:Int, canvas:Canvas, labelPuntos:Label, labelConjuntos:Label):Unit = {
	  canvas.jewels = tablero
	  canvas.repaint()
	  labelPuntos.text = puntos.toString()
	  labelConjuntos.text = conjuntos.toString()
	  
	  seleccion match {
	    case 1 => {
	      println("Elige una acción:\n   1.-Mejor Jugada\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir")
    	  val opcionElegida = scala.io.StdIn.readInt()
    	  opcionElegida match {
    	    case 0 => System.exit(1)
    	     case 1 => analisisAutomatico(tablero, dificultad, anchura, altura,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
    	     case 2 => {
    	       println("Guardar Partida Actual")
    	       guardar(tablero, anchura, altura, dificultad, seleccion, puntos, conjuntos,canvas,labelPuntos,labelConjuntos)}
    	     case 3 => {
    	       println("Cargar Partida Guardada")
    	       cargar(canvas,labelPuntos,labelConjuntos)}
    	     case _ => bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
    	  }
	    }
	    case 2 => {
	      println("Elige una acción:\n   1.-Intercambiar Jewel\n   2.-Guardar Partida\n   3.-Cargar Partida\n   0.-Salir")
	      val opcionElegida = scala.io.StdIn.readInt()
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
    	        bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
    	      }
    	      
    	      intercambiarJewels(tablero, x, y, direccion, anchura, altura, puntos, conjuntos,canvas,labelPuntos,labelConjuntos)
	        }
	        case 2 => {
    	       println("Guardar Partida Actual")
    	       guardar(tablero, anchura, altura, dificultad, seleccion, puntos, conjuntos,canvas,labelPuntos,labelConjuntos)}
    	     case 3 => {
    	       println("Cargar Partida Guardada")
    	       cargar(canvas,labelPuntos,labelConjuntos)
    	       }
	      }
	    }
	    case _ => bucleJuego(tablero,anchura,altura,dificultad,seleccion,puntos,conjuntos,canvas,labelPuntos,labelConjuntos)
	  }
	  
	}
	
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
    
    //Se usa un scroll pane por si la pantalla es pequeña, asi puede navegarse cualquier tabla
    val scrollPane = new ScrollPane {
      contents = canvas
    }

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

    listenTo(canvas.mouse.clicks)
    
    visible = true

    //Eventos
    reactions += {
      case MouseClicked(_, point, _, _, _) =>
        repaint()
    }
  }
	this.top
  bucleJuego(tablero, anchura, altura, dificultad,seleccion,0,0,canvas,labelPuntos,labelConjuntos)
}