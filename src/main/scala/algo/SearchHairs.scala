package algo

import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
import ij.ImagePlus
import org.apache.commons.math3.fitting.PolynomialCurveFitter
import org.apache.commons.math3.fitting.WeightedObservedPoints
import ij.IJ

object SearchHairs {
  import imagej.tools._
  
  def hairsCount( originalimg_high: ImagePlus ):Unit={
    //switch from 16 to 8 bits.
    val originalimg =  new ImagePlus( "original", originalimg_high.getProcessor.createImage())
    val img=originalimg.copyToNewImg("workingcopy")
    
    //Search polynomial function of the root
    //1 - maximum 15
    applyMaximum( img, 15)
    //2 - binary
    val imgBN=createBinary( img, true)
    //3 search white pixels
    val ourWhitePixels=for(  x <- 0  until imgBN.getWidth by 1; y <- 0 until imgBN.getHeight; if(imgBN.getPixel(x,y).isWhite)   )yield{
      (x,y)
    }
    val obs = new WeightedObservedPoints();
    ourWhitePixels.foreach{ p =>
      obs.add( p._1.toDouble, p._2.toDouble )
    }
    println(s"White pixels: ${ourWhitePixels.size}")
    val polyfitter= PolynomialCurveFitter.create(12); //Poly 3
    val coef=polyfitter.fit(obs.toList());
    println(s"coef : ${coef.mkString}");
    val ourfunction = new PolynomialFunction(coef )
    originalimg.updateAndDraw()
    println(s"We have detected : ${hairsCount( originalimg, ourfunction, 10, true)} haires");
    
    imgBN.show()
    imgBN.updateAndDraw()
    
    originalimg.show();
    originalimg.updateAndDraw()
    
    
  }
  
  
  def hairsCount( img: ImagePlus, ourline: PolynomialFunction, delta: Int, debug: Boolean =true ):Int={
    
    println(s"Coefficient for delta $delta :"+ourline.getCoefficients().mkString(";"));

    val x_color=searchPixelValueOnPolyLine( img , ourline, delta)
    val specialPixel=getPixelWithColorFarFromPolyLine( x_color)
   
    if( debug){
      val imgcopy=img.copyToNewImg( s"${img.getTitle}_delta_$delta" )
      val proc=imgcopy.getProcessor
      proc.setColor( java.awt.Color.WHITE)
      
      //can swith to x_color to see the whole line specialPixel
      proc.setColor( java.awt.Color.BLACK)
      x_color.foreach{
        pixel_color => proc.drawPixel(pixel_color._1, delta + ourline.value( pixel_color._1).toInt)       
      }
      proc.setColor( java.awt.Color.WHITE)
      specialPixel.foreach{
        pixel_color => proc.drawPixel(pixel_color._1, delta + ourline.value( pixel_color._1).toInt)       
      }
      imgcopy.show()
      imgcopy.updateAndDraw()
      
      
      val img_x_color=createBlankImage( "x_color_delta_$delta", img)
      val proc_img_x_color= img_x_color.getProcessor
      proc_img_x_color.setColor(java.awt.Color.BLACK)
      x_color.foreach{ x_y => 
        println(s"draw pixel ${x_y._1} , ${x_y._2}")
        proc_img_x_color.drawPixel(x_y._1, x_y._2) }
      
      //test better way to check outside pixel
      val analyse=x_color.tail.zip(x_color).map{
        case (last,current) =>
          if( Math.abs(last._2 - current._2 ) < 10 ) (current._1,current._2) 
          else (current._1,400)
      }
      
      analyse.foreach{ x_y => 
        println(s"draw pixel ${x_y._1} , ${x_y._2}")
        proc_img_x_color.drawPixel(x_y._1, x_y._2) }
//      
//      val polyfitterOverLine= PolynomialCurveFitter.create(48);
//      val obsOverLine = new WeightedObservedPoints();
//      x_color.foreach{ p =>
//        obsOverLine.add( p._1.toDouble, p._2.toDouble )
//      }
//      val ourfunctionOverLine = new PolynomialFunction(polyfitterOverLine.fit(obsOverLine.toList) )
//      x_color.foreach{ x_y => 
//        proc_img_x_color.drawPixel(x_y._1, ourfunctionOverLine.value(x_y._1).toInt) }
      
    
      img_x_color.show()
      img_x_color.updateAndDraw()
      
      IJ.save( imgcopy , "result.tif")
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
    println(s"meanVariation:$meanVariation")
    
    val xOverLineAndOverMean = pixelOverTheLine.filter{
        position => 
          position._2 < ourfunctionOverLine.value( position._1) &&
          (  ourfunctionOverLine.value( position._1)  - position._2  > meanVariation+10)
                   
    }
    xOverLineAndOverMean
  }
  
  
  
  
}