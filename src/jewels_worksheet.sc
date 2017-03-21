object jewels_worksheet {
  import util.Random
  
  //Función para introducir un numero en una posicion determinada
	def introducirElemento(color:Int, pos:Int, tablero:List[Int]):List[Int] =
		if(tablero.isEmpty) Nil else if (pos==0) color::tablero.tail
		else tablero.head::introducirElemento(color,pos-1,tablero.tail)
                                                  //> introducirElemento: (color: Int, pos: Int, tablero: List[Int])List[Int]
  
  def printTablero(tablero:List[Int], pos:Int, anchura:Int, altura:Int):Unit =
  	if(pos%anchura==0 && pos!=0) println("")
  	else println(tablero(pos))                //> printTablero: (tablero: List[Int], pos: Int, anchura: Int, altura: Int)Unit
                                                  //| 
  
  //Genera un numero aleatorio en funcion de la dificultad
  def numRandom(dificultad:Int): Int =
  	if(dificultad == 1)(1+Random.nextInt(3))
  	else if(dificultad == 2)(1+Random.nextInt(5))
  	else if(dificultad == 3)(1+Random.nextInt(7))
  	else 0                                    //> numRandom: (dificultad: Int)Int
  
  
  //Inicializa el tablero de "times" tamaño a un numero aleatorio en función de la dificultad
  def initTablero (times:Int, dificultad:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initTablero (times-1,dificultad):::numRandom(dificultad)::Nil
 		}
	}                                         //> initTablero: (times: Int, dificultad: Int)List[Int]
	
	
	//Analiza si la jewel que se ha movido a la posicion de destino permite eliminar jewels, cuantas y cuales
	def analisisManual(tablero:List[Int]):Unit = 0
                                                  //> analisisManual: (tablero: List[Int])Unit
	
	
	//Intercambia 2 elementos en el tablero, para ello se necesita el primer elemento y ambas posiciones
	def swap(tablero:List[Int], pos1:Int, pos2:Int, elem1:Int):List[Int] =
		introducirElemento(elem1,pos2,introducirElemento(tablero(pos2),pos1,tablero))
                                                  //> swap: (tablero: List[Int], pos1: Int, pos2: Int, elem1: Int)List[Int]
	
	//Funcion que se ocupa de intercambiar con la jewel indicada y realizar llamar al analisis manual
	def intercambiarJewels(tablero:List[Int], pos1_x:Int, pos1_y:Int, direccion:Int, anchura:Int, altura:Int, seleccion:Int):List[Int] = {
		direccion match {
		//Arriba
			case 1 => swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y+1)*anchura, tablero(pos1_x + pos1_y*anchura))
		//Abajo
			case 2 => swap(tablero, pos1_x + pos1_y*anchura, pos1_x + (pos1_y-1)*anchura, tablero(pos1_x + pos1_y*anchura))
		//Izquierda
			case 3 => swap(tablero, pos1_x + pos1_y*anchura, pos1_x-1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura))
		//Derecha
			case 4=> swap(tablero, pos1_x + pos1_y*anchura, pos1_x+1 + (pos1_y)*anchura, tablero(pos1_x + pos1_y*anchura))
		}
		//analisisManual()
	}                                         //> intercambiarJewels: (tablero: List[Int], pos1_x: Int, pos1_y: Int, direccio
                                                  //| n: Int, anchura: Int, altura: Int, seleccion: Int)List[Int]
  
	val anchura = 15                          //> anchura  : Int = 15
	val altura = 10                           //> altura  : Int = 10
	val dificultad = 3                        //> dificultad  : Int = 3
	
	//Se inicializa el tablero
	val tablero = initTablero(anchura*altura,dificultad)
                                                  //> tablero  : List[Int] = List(7, 5, 5, 2, 6, 3, 2, 6, 4, 7, 3, 7, 5, 5, 5, 6,
                                                  //|  1, 6, 6, 4, 5, 7, 5, 7, 2, 7, 3, 4, 5, 4, 1, 2, 5, 5, 2, 7, 7, 3, 7, 3, 1,
                                                  //|  2, 5, 7, 5, 5, 3, 4, 1, 3, 1, 1, 4, 3, 7, 6, 4, 2, 4, 5, 1, 3, 4, 7, 3, 5,
                                                  //|  6, 7, 6, 2, 7, 7, 2, 6, 2, 5, 4, 6, 2, 1, 7, 7, 5, 1, 5, 7, 5, 2, 2, 3, 5,
                                                  //|  3, 3, 1, 3, 4, 1, 3, 7, 2, 3, 4, 5, 7, 5, 4, 3, 3, 6, 4, 5, 5, 2, 5, 2, 7,
                                                  //|  2, 3, 1, 1, 1, 7, 5, 5, 7, 6, 4, 6, 6, 6, 4, 7, 6, 6, 3, 5, 2, 3, 6, 5, 3,
                                                  //|  5, 6, 5, 7, 3, 3, 5, 7, 2)
}