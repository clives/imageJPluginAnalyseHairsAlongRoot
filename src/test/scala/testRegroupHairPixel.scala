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
class testRegroupHairPixel extends WordSpec {

"searchHaires" when {
    "use delta from 10ss to 16" should {

      
      "using SMA" in {
        val img=IJ.openImage("./images/hairPixels_001.tif") 
        
        val imgproc=img.getProcessor
        

        
        val ourHairPixels=(for( x <- 0 until imgproc.getWidth; y<- 0 until  imgproc.getHeight )yield{
          ( Pixel(x,y), img.getPixel(x, y).isWhite())
        }).filter{ pixel_color => pixel_color._2  }.map{ pixel_color => pixel_color._1}.toList
        
        
        val width: Int=imgproc.getWidth;
        val height: Int=imgproc.getHeight;
        

        
        def existNeighbour( p:Pixel)={
          val x=p.x
          val y=p.y
          (for( range <- 1 to 4 )yield
          {
            List( 
                Pixel(x+range,y+range), Pixel(x+range,y-range),
                Pixel(x-range,y+range), Pixel(x-range,y-range)
            )  
          }).toList.flatten.distinct.
          filter{ p => p.x > 0 && p.x < width && p.y >0 && p.y < height}.
            filter{ p => p.x > 0 && p.x < width && p.y >0 && p.y < height}.
            filter{ p => ourHairPixels.contains(p)}.nonEmpty
        }
        
        val orderedHairPixels=(ourHairPixels.sortWith{ (b:Pixel, a:Pixel) => 
          a.y > b.y || ( a.y==b.y && a.x > b.x)  
        }).filter { existNeighbour _ }
        
        println(s"size after filter: ${orderedHairPixels.size}, size before: ${ourHairPixels.size}")
        
        val imgKeepResult=imagej.tools.createBlankImage("detectedPixels_clean", img)
        val procKeep=imgKeepResult.getProcessor
        procKeep.setColor( BLACK)
        procKeep.fill()
        procKeep.setColor( WHITE)
        
        orderedHairPixels.foreach { pixel => 
            procKeep.drawPixel(pixel.x, pixel.y)
        }
        IJ.save( imgKeepResult , DIRECTORY_RESULTIMAGE+ s"testResultPixel_clean.tif")
        
        
        /*
         * get next pixels (if exist) under the current one ( we move from 0 to height )
         *    p    
         *   xxx     next to check
         */
        def nextPixels( p:Pixel, hairpixels: List[Pixel])={
          
          (for( range <- 0 to 4;  x<- p.x-range to p.x+range; y<- p.y-range to p.y+range )yield
          {
            Pixel(x,y)
          }).toList.distinct.
            filter{ p => p.x > 0 && p.x < width && p.y >0 && p.y < height}.
            filter{ p => hairpixels.contains(p)}
//          
//          List( 
//              Pixel( p.x -2, p.y),Pixel(p.x-1, p.y),Pixel(p.x+1, p.y),Pixel(p.x+2, p.y),
//              Pixel( p.x -1, p.y+1),Pixel(p.x, p.y+1),Pixel(p.x+1, p.y+1),
//              Pixel( p.x -1, p.y+2),Pixel(p.x, p.y+2),Pixel(p.x+1, p.y+2),
//              Pixel( p.x -1, p.y+3),Pixel(p.x, p.y+3),Pixel(p.x+1, p.y+3),
//              Pixel( p.x -1, p.y+4),Pixel(p.x, p.y+4),Pixel(p.x+1, p.y+4)
//          ).
//            filter{ p => p.x > 0 && p.x < width && p.y >0 && p.y < height}.
//            filter{ p => hairpixels.contains(p)}
        }
        
        def consumeHairPixel( currentpixel: List[Pixel], currentHair: Map[Int, List[Pixel]]= Map.empty):Map[Int, List[Pixel]]={
          
          def consumeOneHair( currentpixel: List[Pixel], lefthairpixels: List[Pixel],currentHair: List[Pixel]=List.empty): List[Pixel] ={
            println("currentpixel:"+currentpixel)
            
            currentpixel match{
            case Nil => 
             // println("---- return currentHair:"+currentHair)
              currentHair
            case x::xs =>
              val nextvalidpixels = nextPixels( x, lefthairpixels).filter{ p => !currentpixel.contains(p)}.filter{ p => !currentHair.contains(p)}
             // println("next valid:"+nextvalidpixels.size+", currenthaire:"+x:: currentHair)
              consumeOneHair( xs ++nextvalidpixels, lefthairpixels, x:: currentHair)
            }
          }
          
          currentpixel match{
            case Nil => currentHair
            case x::xs =>
              val hairpixels=consumeOneHair( List(x), xs)
              val nextid=if( currentHair.isEmpty) 0 else currentHair.keys.max+1
              val nexthairpixel= xs.filter{p => !hairpixels.contains(p)}
              
              println("hairpixels:"+hairpixels.size+", xs:"+xs.size+", left hair pixel:"+nexthairpixel.size)
              
              
              //if( nextid < 2)
              consumeHairPixel(nexthairpixel, currentHair+( nextid->hairpixels)   )
             // else currentHair
          }
        }
        
        
      val allidhairs=consumeHairPixel( orderedHairPixels)
      
      allidhairs.foreach{ idhair =>
      val onehair=imgKeepResult.copyToNewImg("fullHair" )
      
      val procfullhair=onehair.getProcessor
      procfullhair.setColor( BLACK)
     // procfullhair.fill()
      procfullhair.setColor( RED)
    
      
          idhair._2.foreach{ pixel=>
            procfullhair.drawPixel(pixel.x, pixel.y)
            
          }        
      
      IJ.save( onehair , DIRECTORY_RESULTIMAGE+ s"oneHair_id${idhair._1}.tif")
    }

      }
    }
  }
}