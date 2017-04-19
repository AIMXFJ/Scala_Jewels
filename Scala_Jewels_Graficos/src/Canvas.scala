import scala.swing.Panel
import java.awt.{ Graphics2D, Color }
import java.awt.Graphics
import java.awt.Image
import javax.swing.ImageIcon

case class Canvas(anch:Int, alt:Int) extends Panel {  //Clase que contiene el tablero
  var jewels:List[Int] = List()
  val anchura = anch
  val altura = alt

  override def paintComponent(g: Graphics2D) {  //Método de la interfaz para dibujar las jewels.
    
    // Se limpia el Canvas
    g.clearRect(0, 0, size.width, size.height)
    g.setColor(Color.white)
    g.fillRect(0, 0, 1200, 1200)
    updateJewels(g,this.jewels,anchura,altura,0,altura-1)
    
  }

  //Calcula donde deben dibujarse las jewels y las dibuja hasta que este todo el tablero dibujado.
  def updateJewels(g:Graphics2D, jewels:List[Int], anchura:Int, altura:Int, x:Int, y:Int) {
    if(x+y*anchura < anchura*altura && x+y*anchura >= 0){
    jewels(x+y*anchura) match {
      case 1 => {
        g.drawImage(new ImageIcon(getClass().getResource("img/Jewel1.png")).getImage(),x*44,(altura-1-y)*44,null);
        if((x+1)%anchura==0)
          updateJewels(g,jewels,anchura,altura,0,y-1)
        else
          updateJewels(g,jewels,anchura,altura,x+1,y)
      }
      case 2 => {
          g.drawImage(new ImageIcon(getClass().getResource("img/Jewel2.png")).getImage(),x*44,(altura-1-y)*44,null);
          if((x+1)%anchura==0)
            updateJewels(g,jewels,anchura,altura,0,y-1)
          else
            updateJewels(g,jewels,anchura,altura,x+1,y)
        }
      case 3 => {
          g.drawImage(new ImageIcon(getClass().getResource("img/Jewel3.png")).getImage(),x*44,(altura-1-y)*44,null);
          if((x+1)%anchura==0)
            updateJewels(g,jewels,anchura,altura,0,y-1)
          else
            updateJewels(g,jewels,anchura,altura,x+1,y)
        }
      case 4 => {
          g.drawImage(new ImageIcon(getClass().getResource("img/Jewel4.png")).getImage(),x*44,(altura-1-y)*44,null);
          if((x+1)%anchura==0)
            updateJewels(g,jewels,anchura,altura,0,y-1)
          else
            updateJewels(g,jewels,anchura,altura,x+1,y)
        }
      case 5 => {
          g.drawImage(new ImageIcon(getClass().getResource("img/Jewel5.png")).getImage(),x*44,(altura-1-y)*44,null);
          if((x+1)%anchura==0)
            updateJewels(g,jewels,anchura,altura,0,y-1)
          else
            updateJewels(g,jewels,anchura,altura,x+1,y)
        }
      case 6 => {
          g.drawImage(new ImageIcon(getClass().getResource("img/Jewel6.png")).getImage(),x*44,(altura-1-y)*44,null);
          if((x+1)%anchura==0)
            updateJewels(g,jewels,anchura,altura,0,y-1)
          else
            updateJewels(g,jewels,anchura,altura,x+1,y)
        }
      case 7 => {
          g.drawImage(new ImageIcon(getClass().getResource("img/Jewel7.png")).getImage(),x*44,(altura-1-y)*44,null);
          if((x+1)%anchura==0)
            updateJewels(g,jewels,anchura,altura,0,y-1)
          else
            updateJewels(g,jewels,anchura,altura,x+1,y)
        }
      case 8 => {
          g.drawImage(new ImageIcon(getClass().getResource("img/Jewel8.png")).getImage(),x*44,(altura-1-y)*44,null);
          if((x+1)%anchura==0)
            updateJewels(g,jewels,anchura,altura,0,y-1)
          else
            updateJewels(g,jewels,anchura,altura,x+1,y)
        }
    }
    }
  }
}