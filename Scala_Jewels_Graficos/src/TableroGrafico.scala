import javax.swing.ImageIcon
import java.awt.Toolkit
import java.awt.Graphics2D
import java.awt.Image
import javax.imageio.ImageIO
//import javax.swing.JLabel
import swing._

object TableroGrafico extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Jewels"
    contents =new Label {
                   icon = new ImageIcon( getClass().getResource("img/Jewel1.png") );
              }
  }
}