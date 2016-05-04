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
      
      case class HairPixelDelta(  hp: HairPixel, delta: Int){
        override def toString()=s"hairpixel: $hp, delta: $delta"
        
        
      }
      
      
      
      type DeltaHairs = Map[Int, List[HairPixel]];
      
      //----------------------------------------------
      //----------------------------------------------
      def getFirstHairPixel(deltas: Range, delta_hairpixel:Map[Int, List[HairPixel]] ):Option[HairPixelDelta]={
       val delta=deltas.head
       val ourfirstpixelHigherPr=(delta_hairpixel.get(delta).map{
         l=>
           l.filter(_.prHair == 1.00d).headOption
       }).flatten
       
       if( ourfirstpixelHigherPr.isDefined )
         ourfirstpixelHigherPr.map{ p => HairPixelDelta(p, delta )}
       else
         getFirstHairPixel(deltas.tail, delta_hairpixel)  
      }
      
      //----------------------------------------------
      // get neighbour ( raising delta for the moment)
      //----------------------------------------------
      def getNextPixels( currentpixel: HairPixelDelta,hairs: DeltaHairs, deltas: Range )={
        val listx=List( currentpixel.hp.x, currentpixel.hp.x+1, currentpixel.hp.x-1)
        val nextdelta=currentpixel.delta+1
        
        if( deltas.contains(nextdelta)){
          val lPossiblepixels=hairs.get( nextdelta).toList.flatten.filter{ p => listx.contains( p.x)}   
          lPossiblepixels.map{ p=> HairPixelDelta(p, nextdelta)}
        }else{
          List.empty
        }
      }
      
      import scala.annotation.tailrec
      
      
      //----------------------------------------------
      // remove from allpixelhairs: DeltaHairs the pixels 
      // present in pixeltoremove: List[HairPixelDelta]
      //----------------------------------------------
      def cleanHairsPixels(allpixelhairs: DeltaHairs, pixeltoremove: List[HairPixelDelta]): DeltaHairs={
        val deltastocheck=pixeltoremove.map(_.delta).distinct
        val result=allpixelhairs.map{ 
          delta_phair => 
            val listremoveatdelta=pixeltoremove.filter(_.delta == delta_phair._1).map(_.hp)         
            println("list possible pixel to remove:"+listremoveatdelta.size)
            
            if( listremoveatdelta.nonEmpty ){
              delta_phair._1 -> delta_phair._2.filterNot( listremoveatdelta contains _)
            }else
              delta_phair
        }               
        result
      }
      
      // once we detected a hair, with have to remove all the pixels around
      @tailrec
      def removeHairPixels( pixelstocheck: List[HairPixelDelta], allpixelhairs: DeltaHairs, pixeltoremove: List[HairPixelDelta] =List.empty):List[HairPixelDelta] ={
        
        if( pixelstocheck.isEmpty ) {
          println("Size to remove:"+pixeltoremove.size)
          pixeltoremove
        }else{
          val currentpixel= pixelstocheck.head
          val neighbours = List( currentpixel.hp.x+1, currentpixel.hp.x-1)
          
          //visit if not present in pixelstocheck and pixeltoremove and prHair > 0.5
          val nextToVisit=(allpixelhairs.get(currentpixel.delta).toList.flatten.filter{p=> neighbours contains p.x}.map{
            p => new HairPixelDelta(p, currentpixel.delta)
          }).filter (_.hp.prHair > 0.3).filterNot( pixeltoremove contains _).filterNot( pixelstocheck contains _)
          
          removeHairPixels( pixelstocheck.tail ++ nextToVisit, allpixelhairs, currentpixel::pixeltoremove)
        }
      }
      
      
      //----------------------------------------------
      // Consume only >0.1 pr for the moment
      //
      //----------------------------------------------
      @tailrec
      def consume(currentpixel: HairPixelDelta, hairs: DeltaHairs, deltas: Range, currentPixel: List[HairPixelDelta]=List.empty): List[HairPixelDelta]={
        val next=getNextPixels( currentpixel, hairs, deltas).filter(_.hp.prHair>0.1d).sortBy { -_.hp.prHair }.headOption
        
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
               proc.drawPixel(pixels.hp.x, pixels.hp.y.get)
               
          }
          IJ.save( img_oneHaire , DIRECTORY_RESULTIMAGE + s"oneHair.tif")
          
          //Cleaning:
          val pixeltoremove=removeHairPixels( ourhairpixels,mapDeltaHairPixels)
          proc.setColor(0)
          proc.fill()
          proc.setColor(255)
          pixeltoremove.map{
             pixels =>
               proc.drawPixel(pixels.hp.x, pixels.hp.y.get)
               
          }
          IJ.save( img_oneHaire , DIRECTORY_RESULTIMAGE + s"removeoneHair.tif")
          
          
          //test remove
          val img_removeoneHaire=createBlankImage( "oneHairGrouping", img)
          val proctest= img_removeoneHaire.getProcessor
          proctest.setColor(RED)
          proctest.fill()
          proctest.setColor(BLACK)
          cleanHairsPixels( mapDeltaHairPixels, pixeltoremove).toList.map(_._2).flatten.foreach{ hairpixel =>
            proctest.drawPixel(hairpixel.x, hairpixel.y.get)
          }
          IJ.save( img_removeoneHaire , DIRECTORY_RESULTIMAGE + s"resultRemoveoneHair.tif")
        }
      }
    }
  }
}