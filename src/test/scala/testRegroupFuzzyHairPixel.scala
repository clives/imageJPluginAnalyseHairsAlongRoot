import org.scalatest.WordSpec
import ij.IJ
import algo.SearchHairs._
import algo._
import algo.SearchHairs._

import imagej.tools._
  import java.awt.Color._
/*
 * 
 **/
class testRegroupFuzzyHairPixel extends WordSpec {

"searchHaires" when {
    "use delta from 10ss to 16" should {
      
      case class HairPixelDelta(  x: Int, y: Int, prHair: Double, delta: Int){
        override def toString()=s"x: $x, pr: $prHair, detla: $delta"
      }
      
      type DeltaHairs = Map[Int, List[HairPixel]];
      
      //----------------------------------------------
      //----------------------------------------------
      def getFirstHairPixel(deltas: Range, delta_hairpixel:Map[Int, List[HairPixel]] ):Option[HairPixelDelta]={
       val delta=deltas.head
       val ourfirstpixel=(delta_hairpixel.get(delta).map{
         l=>l.filter(_.prHair == 1.0).headOption
       }).flatten
       
       if( ourfirstpixel.isDefined )
         ourfirstpixel.map{ p => HairPixelDelta(p.x,p.y.get,p.prHair, delta )}
       else
         getFirstHairPixel(deltas.tail, delta_hairpixel)  
      }
      
      //----------------------------------------------
      // get neighbour ( raising delta for the moment)
      //----------------------------------------------
      def getNextPixels( currentpixel: HairPixelDelta,hairs: DeltaHairs, deltas: Range )={
        val listx=List( currentpixel.x, currentpixel.x+1, currentpixel.x-1)
        val nextdelta=currentpixel.delta+1
        
        if( deltas.contains(nextdelta)){
          val lPossiblepixels=hairs.get( nextdelta).toList.flatten.filter{ p => listx.contains( p.x)}   
          lPossiblepixels.map{ p=> HairPixelDelta(p.x,p.y.get, p.prHair, nextdelta)}
        }else{
          List.empty
        }
      }
      
      import scala.annotation.tailrec
      
      //----------------------------------------------
      // Consume only 1.0 pr for the moment
      //----------------------------------------------
      @tailrec
      def consume(currentpixel: HairPixelDelta, hairs: DeltaHairs, deltas: Range, currentPixel: List[HairPixelDelta]=List.empty): List[HairPixelDelta]={
        val next=getNextPixels( currentpixel, hairs, deltas).filter(_.prHair==1.0d).headOption
        
        val result= currentPixel ++ next
        
        next match{
          case None => result
          case Some(nextcurrentpixel)=>
            consume(  nextcurrentpixel, hairs, deltas, result)
        }     
      }
      
      
      "using first hairpixel at 1.0d pr" in {
        val img=IJ.openImage("/home/sosthene/Downloads/crop_wt_me10_2.tif")
        val deltaRange=(-220 to 100 by 1)
        val mapDeltaHairPixels=hairsCount(img,deltaRange)(new fuzzySearchOutsidersPixelUsingMultiplePolyRegression(50,1))
        
        
        //start with first pixel on the higher delta       
        val optStartPixel = getFirstHairPixel(  deltaRange, mapDeltaHairPixels)
        println("optStartPixel:"+optStartPixel )
        
        optStartPixel.map{startPixel=>
          val ourhairpixels=consume( startPixel,mapDeltaHairPixels, deltaRange )
          println("Pixels size:"+ourhairpixels.size)
          
          //draw it
          val img_oneHaire=createBlankImage( "oneHairGrouping", img)
          val proc= img_oneHaire.getProcessor
          proc.setColor(255)
          
          ourhairpixels.map{
             pixels =>
               proc.drawPixel(pixels.x, pixels.y)
               
          }
          IJ.save( img_oneHaire , DIRECTORY_RESULTIMAGE + s"oneHair.tif")
        }
      }
    }
  }
}