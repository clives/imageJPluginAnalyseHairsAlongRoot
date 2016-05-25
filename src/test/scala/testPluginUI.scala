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
import java.awt.image._
import java.awt.event._
import java.awt.TextArea
import imagej.tools._
import akka.stream.OverflowStrategy
import akka.actor.ActorRef
import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{ Flow, Sink, Source, Tcp }
import akka.util.ByteString
import scala.concurrent.duration._
import scala.util.{ Failure, Success }
import scala.concurrent.ExecutionContext.Implicits.global
import javax.swing.SwingUtilities
import hairRegrouping._
import java.awt._
import java.io.FileInputStream
import java.io.ObjectInputStream
        
/*
 * experiment with imagej plugin to add ui to manipulate hairs
 * http://rsb.info.nih.gov/ij/plugins/ , http://rsb.info.nih.gov/ij/plugins/panel-window.html
 **/
object testPluginUI {
	  implicit val system = ActorSystem("TestSystem")
	  implicit val materializer = ActorMaterializer()
		import java.awt.event.MouseEvent
		
		trait ItemListener extends java.awt.event.ItemListener{
		  def itemStateChanged(e:ItemEvent){}
	  }
		
	  trait MouseListener  extends java.awt.event.MouseListener{
  		def mouseClicked(e:MouseEvent){}
  		def mousePressed(e:MouseEvent){}
  		def mouseReleased(e:MouseEvent){}
  		def mouseEntered(e:MouseEvent){}
  		def mouseExited(e:MouseEvent){}
	  }
  
    implicit def funToRunnable(fun: () => Unit) = new Runnable() { def run() = fun() }       
          
    class ListeningMouseClicked extends MouseListener{
      
      var listHairs= scala.List.empty[PossiblePixelsHair]
      
      def appendHairInfo( info: PossiblePixelsHair )={
        listHairs :+= info
      }

      override def mouseClicked(e: MouseEvent)={
        println("click");
      }
      
    }
  
  class CustomWindow( impe:ImagePlus,  ice:ImageCanvas, listeningMouseClicked:ListeningMouseClicked) extends ImageWindow(impe, ice) with ActionListener with MouseListener with ItemListener {
    
        val (button1,button2, textarea, labelNbrHaires, filterHair) = addPanel()
        val HAIR_COLOR = java.awt.Color.RED;
        val HAIR_COLOR_FILTERED = java.awt.Color.BLUE;
        
        
        var listHairs= scala.List.empty[PossiblePixelsHair]
       
        override def itemStateChanged(e:ItemEvent)={
          if( e.getItemSelectable == filterHair){

            
             if (e.getStateChange() == ItemEvent.DESELECTED){
               println("DESELECTED");
             }else{
               println("SELECTED");
               val proc = impe.getProcessor
               proc.setColor( HAIR_COLOR_FILTERED)
               listHairs.filter { x => x.pixels.size <= 40 }.foreach { 
                 elem =>  elem.pixels.foreach{
                     pixels =>
                       proc.drawPixel(pixels.hp.x, pixels.hp.y.get)                                          
                  }
               
               }
               impe.updateAndDraw()            
             }
          }else{          
          }
        }
        
        override def mouseClicked(e: MouseEvent)={
          println("click - CustomWindow");
          
          def closeEnough(p:HairPixelDelta, x:Int, y:Int )={
            p.hp.x > x-4 && p.hp.x < x+4 && p.hp.y.get > y-4 && p.hp.y.get < y+4  
            
          }
          
          listHairs.filter{ ourhair =>  
              ourhair.pixels.exists { p => closeEnough(p,e.getX,e.getY) }
          }.headOption.map{
            p =>
              val deltaSize= p.pixels.map(_.delta).distinct.size
              val nbrPixels = p.pixels.size
              
              SwingUtilities.invokeLater( () => {textarea.setText(s"id: ${p.hairId}, deltaSize:$deltaSize , nbrPixels: $nbrPixels")})
          }.getOrElse{
            SwingUtilities.invokeLater( () => {textarea.setText(s"Not a hair")})
          }
          
        }
        
        val ourSink= Sink.foreach[PossiblePixelsHair]{
          elem => 
            listHairs :+= elem
            println("size nbr hairs:"+listHairs.size)
            val proc = impe.getProcessor
            proc.setColor( HAIR_COLOR)
            elem.pixels.map{
                 pixels =>
                   proc.drawPixel(pixels.hp.x, pixels.hp.y.get)                                          
              }
            impe.updateAndDraw()
            listeningMouseClicked.appendHairInfo(elem)
            SwingUtilities.invokeLater( () => {labelNbrHaires.setText(s"MaxNbrHair: ${elem.hairId}");pack(); } ) 
        } 
        
        def setText( content: String)={
          textarea.setText(content)
        }
        
        def addPanel()={
            val panel = new Panel();
            panel.setLayout(new GridLayout(1, 4));
            val button1 = new Button(" Invert ");
            button1.addActionListener(this);
            panel.add(button1);
            val button2 = new Button(" Flip ");
            button2.addActionListener(this);
            panel.add(button2);            
            val textarea=new TextArea()
            
            val nbrHairs = new Label(" ");
          //  nbrHairs.setMinimumSize( new Dimension(100,100))
            panel.add(nbrHairs)
            
            val filterHair =new Checkbox();
            filterHair.setState(false)
            filterHair.setEnabled(false)
            filterHair.setLabel("Filter hairs:")
            filterHair.addItemListener( this)
            panel.add(filterHair)
            
            add(panel);
            add( textarea)
            pack();
            (button1, button2, textarea, nbrHairs,filterHair)
        }
        
        setText("test")
      
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
        val img=IJ.openImage("images/wt_me10_2.tif")        
        val deltaRange=(-300 to 300 by 1)

        import hairRegrouping._
        
        //Thread.sleep(30000)
        
        cleanResultDirectory()
        implicit val searchtoutside =new fuzzySearchOutsidersPixelUsingMultiplePolyRegression(50,1)
        implicit val dd =new searchWhiteToBlackLine();
        implicit val dst=destinationFiles( "pluginExperiment/")
        
        //
        // create analyse or read file with last result
        //
        val mapDeltaHairPixels= 
          if( false){
          hairsCount(img,deltaRange)
          }else{
             val ois = new ObjectInputStream(new FileInputStream("saveMapHairs" ))
             val ourmap=ois.readObject().asInstanceOf[DeltaHairs]
             // zoneFilter( ourmap, Pixel(37,568), Pixel(100,650))
             ourmap
          }
        
         

//import system.dispatcher

        def run(actor: ActorRef) = {
          Future { Thread.sleep(300); actor ! 1 }
          Future { Thread.sleep(200); actor ! 2 }
          Future { Thread.sleep(100); actor ! 3 }
        }
        val s = Source
          .actorRef[hairRegrouping.PossiblePixelsHair](bufferSize = 0, OverflowStrategy.fail)
          .mapMaterializedValue{
            ref => 
              
              //remove pixel with p(hair) below threshold = 0.04d
              val mapfiltered=mapDeltaHairPixels.map{
                delta_listpixels => (delta_listpixels._1, delta_listpixels._2.filter(_.prHair>=0.04d))
              }             
                    
              consumeHairs(mapfiltered, deltaRange, img.copyToNewImg("workingcopy"),Some(ref), thresholdHair=0.04d)
         }
          
        val ip = img.getProcessor();
		    img.setProcessor(null, ip.convertToRGB());
		    img.setCalibration(img.getCalibration()); //update calibration
         
        val ourwindow=new CustomWindow(img, img.getCanvas, new ListeningMouseClicked);
        img.getCanvas.requestFocus();
        img.getCanvas.addMouseListener(ourwindow)
        
        
        
        val flow=s to ourwindow.ourSink
        flow.run()
        ourwindow.filterHair.setEnabled(true)

  }
}