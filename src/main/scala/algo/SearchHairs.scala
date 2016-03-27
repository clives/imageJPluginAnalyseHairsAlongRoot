package algo

import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
import ij.ImagePlus
import org.apache.commons.math3.fitting.PolynomialCurveFitter
import org.apache.commons.math3.fitting.WeightedObservedPoints

object SearchHairs {
  import imagej.tools._
  
  def hairsCount( img: ImagePlus, ourline: PolynomialFunction, delta: Int, debug: Boolean =true )={
    val x_color=searchPixelValueOnPolyLine( img , ourline, delta)
    val specialPixel=getPixelWithColorFarFromPolyLine( x_color)
   
    if( debug){
      val imgcopy=img.copyToNewImg( s"${img.getTitle}_delta_$delta" )
      val proc=imgcopy.getProcessor
      proc.setColor( java.awt.Color.WHITE)
      specialPixel.foreach{
        pixel_color => proc.drawPixel(pixel_color._1, delta + ourline.value( pixel_color._1).toInt)       
      }
      imgcopy.show()
      imgcopy.updateAndDraw()
    }
    
    regroupPixelOverLine( specialPixel)
  }
  
  
  private def regroupPixelOverLine( overlinepixel: List[(Int,Int)], count :Int=0):Int ={
      overlinepixel match{
        case x::Nil => 
          println("Last x:"+x)
          count
        case Nil =>   count
        case x::xs =>
          
          val updatecount=if( xs.head._1 - x._1 < 4 ){   
             if( count ==0 ) 1 //count change, we have to add the first one.
             else count
            }else{
              println("Raise over:"+x._1)
              count+1;
            }
          
          regroupPixelOverLine( xs, updatecount)
        
      }
    }
  
  /*
   * Search over a polynomial line the color of the pixel ( x, color)
   * Permits to know the variation over the line, large variation => could be a hair 
   * 
   * delta - permits to move the line over y (up / down)
   */
  private def searchPixelValueOnPolyLine( img: ImagePlus, ourline: PolynomialFunction, delta: Int )={
    val proc=img.getProcessor
    val pixelValueOverTheLine = for( x <- 0 until img.getWidth )yield{
      val valueatx=proc.getPixel(  x , ourline.value(x).toInt + delta  )   
      (x,valueatx)
    }
    pixelValueOverTheLine.toList
  }
  
  
  /*
   * pixelOverTheLine => (x, color) , we create a polynomial line who match those (x,color)
   * then we calculate the mean difference => we use this mean difference to detect pixel under this line with
   * larger difference than the mean.
   */
  private def getPixelWithColorFarFromPolyLine(pixelOverTheLine: List[(Int,Int)] )={
    val polyfitterOverLine= PolynomialCurveFitter.create(8);
    val obsOverLine = new WeightedObservedPoints();
    pixelOverTheLine.foreach{ p =>
      obsOverLine.add( p._1.toDouble, p._2.toDouble )
    }
    val ourfunctionOverLine = new PolynomialFunction(polyfitterOverLine.fit(obsOverLine.toList) )
    
    val meanVariation=pixelOverTheLine.map{ x=> Math.abs(ourfunctionOverLine.value(x._1)-x._2)  }.reduce(_+_) / pixelOverTheLine.size
    
    val xOverLineAndOverMean = pixelOverTheLine.filter{
        position => 
          position._2 < ourfunctionOverLine.value( position._1) &&
          (  ourfunctionOverLine.value( position._1)  - position._2  > meanVariation+10)
                   
    }
    xOverLineAndOverMean
  }
  
  
  
  
}