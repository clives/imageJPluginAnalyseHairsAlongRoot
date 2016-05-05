package algo

import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
import org.apache.commons.math3.fitting.PolynomialCurveFitter
import org.apache.commons.math3.fitting.WeightedObservedPoints
import SearchHairs._
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import org.apache.commons.math3.optimization.fitting.PolynomialFitter
import ij.ImagePlus
import ij.IJ
  
object parallelHairLines{
  // (x,delta)=> Option(y)
  type serviceline=(Int,Int)=>Option[Int]
}

 /*
  * define service to generate line in // with main hair
  * the first implementation was based on polynomial regression.
  * Works fine on small picture, but it's just not compatible with large
  * picture ( 
  */
 trait parallelHairLines{  
    import parallelHairLines._  
  
  
    /*
     * @return function (x,delta) that give use the value of y (if exsite) for
     * the position x and the delta delta.
     */
    def create( img: ImagePlus ):serviceline
    
    def name:String
  }

import SearchHairs._
import imagej.tools._


/*
 * 1/ create the binary image using maximum, createBinary as 
 * used in polyRegressionParallelHairLines
 * 2/ assuming that the whole image is black expect the ones around the
 * root we consume the pixel from x: 0->with, y: 0->height)yield{ map{x->List(y color white)}  } 
 */
class searchWhiteToBlackLine extends parallelHairLines{
  
 def name="searchWhiteToBlackLine"
  
 def create( img: ImagePlus) = {
    //Search polynomial function of the root
    //1 - maximum 15
    applyMaximum( img, 15)
    //2 - binary
    val imgBN=createBinary( img, true)
    
    IJ.save( imgBN , DIRECTORY_RESULTIMAGE+ s"BN_Image.tif")
    
    //3 search white pixels
    val xtoMaxY=(for(  x <- 0  until imgBN.getWidth by 1; y <- 0 until imgBN.getHeight; if(imgBN.getPixel(x,y).isWhite)   )yield{
      (x,y)
    }).toList.groupBy{ x_y => x_y._1 }.map{ a => (a._1, a._2.map(_._2).max) }
    
    (x:Int,delta:Int)=>{
      xtoMaxY.get(x).flatMap{ basey=>
        val y = basey +delta
        if( y <0 || y > img.getHeight ) None
        else Some(y)
      }        
    } 
 } 
}


class polyRegressionParallelHairLines extends parallelHairLines{
  
  def name="polyRegressionParallelHairLines"
  
  def create( img: ImagePlus) = {
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
        
    val polyfitter= PolynomialCurveFitter.create(12); //Poly 3
    val coef=polyfitter.fit(obs.toList());

    val ourfunction = new PolynomialFunction(coef )
    
    (x:Int,delta:Int)=>{
      val y=ourfunction.value(x.toDouble).toInt+delta
        if( y <0 || y > img.getHeight ) None
        else Some(y)
    } 
  }
}
  