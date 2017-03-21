object jewels_worksheet {
  import util.Random;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(319); 
  
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
  	else 0;System.out.println("""numRandom: (dificultad: Int)Int""");$skip(280); 
  
  
  //Inicializa el tablero de "times" tamaño a un numero aleatorio en función de la dificultad
  def initTablero (times:Int, dificultad:Int) : List[Int] = {
 		times match { case 0 => Nil
 		case times => initTablero (times-1,dificultad):::numRandom(dificultad)::Nil
 		}
	};System.out.println("""initTablero: (times: Int, dificultad: Int)List[Int]""");$skip(159); 
	
	
	//Analiza si la jewel que se ha movido a la posicion de destino permite eliminar jewels, cuantas y cuales
	def analisisManual(tablero:List[Int]):Unit = 0;System.out.println("""analisisManual: (tablero: List[Int])Unit""");$skip(258); 
	
	
	//Intercambia 2 elementos en el tablero, para ello se necesita el primer elemento y ambas posiciones
	def swap(tablero:List[Int], pos1:Int, pos2:Int, elem1:Int):List[Int] =
		introducirElemento(elem1,pos2,introducirElemento(tablero(pos2),pos1,tablero));System.out.println("""swap: (tablero: List[Int], pos1: Int, pos2: Int, elem1: Int)List[Int]""");$skip(791); 
	
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
	};System.out.println("""intercambiarJewels: (tablero: List[Int], pos1_x: Int, pos1_y: Int, direccion: Int, anchura: Int, altura: Int, seleccion: Int)List[Int]""");$skip(21); 
  
	val anchura = 15;System.out.println("""anchura  : Int = """ + $show(anchura ));$skip(17); 
	val altura = 10;System.out.println("""altura  : Int = """ + $show(altura ));$skip(20); 
	val dificultad = 3;System.out.println("""dificultad  : Int = """ + $show(dificultad ));$skip(84); 
	
	//Se inicializa el tablero
	val tablero = initTablero(anchura*altura,dificultad);System.out.println("""tablero  : List[Int] = """ + $show(tablero ))}
}
