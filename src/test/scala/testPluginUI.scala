import org.scalatest.WordSpec
import ij.IJ
import algo.SearchHairs._
import algo._
import ij.plugin.ContrastEnhancer
import ij.plugin.filter.RankFilters
import ij.gui.ImageCanvas
import java.awt.event.ActionEvent
import ij.ImagePlus
import java.awt.FlowLayout
import ij.gui.ImageWindow
import java.awt.event.ActionListener
import ij.plugin._
import ij._
import ij.gui._
import ij.process._
import java.awt._
import java.awt.image._
import java.awt.event._
/*
 * experiment with imagej plugin to add ui to manipulate hairs
 * http://rsb.info.nih.gov/ij/plugins/ , http://rsb.info.nih.gov/ij/plugins/panel-window.html
 **/
object testPluginUI {
  
  
  class CustomWindow( impe:ImagePlus,  ice:ImageCanvas) extends ImageWindow(impe, ice) with ActionListener {
    
        val (button1,button2) = addPanel()
       
    
        def addPanel()={
            val panel = new Panel();
            panel.setLayout(new GridLayout(1, 2));
            val button1 = new Button(" Invert ");
            button1.addActionListener(this);
            panel.add(button1);
            val button2 = new Button(" Flip ");
            button2.addActionListener(this);
            panel.add(button2);
            add(panel);
            pack();
            (button1, button2)
        }
      
        override def actionPerformed( e:ActionEvent) {
            val b = e.getSource();
            if (b==button1) {
                imp.getProcessor().invert();
                imp.updateAndDraw();
            } else {
                imp.getProcessor().flipVertical();
                imp.updateAndDraw();
            }
            val ic = imp.getCanvas();
            if (ic!=null)
                ic.requestFocus();
        }
        
    } // CustomWindow inner class
  
  def main(args: Array[String]): Unit = {

        val img=IJ.openImage("./images/hairPixels_001.tif") 
        val imgproc=img.getProcessor
        new CustomWindow(img, img.getCanvas);
        img.getCanvas.requestFocus();
  }
}