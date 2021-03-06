import org.scalatest.WordSpec
import ij.IJ
import algo.SearchHairs._
import algo._
import algo.SearchHairs._
import imagej.tools._
import java.awt.Color._
import ij.ImagePlus
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
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
            
      case class destinationFiles(directoryName: String){       
        
        val resultImage= DIRECTORY_RESULTIMAGE + directoryName
        val fullPixelHairs= DIRECTORY_RESULTIMAGE +  directoryName + "fullhairs/"
        val hairs = DIRECTORY_RESULTIMAGE +  directoryName + "hairs/"
        
        new java.io.File(resultImage).mkdir()
        new java.io.File(fullPixelHairs).mkdir()
        new java.io.File(hairs).mkdir()
        
        cleanDirectory( resultImage)
        cleanDirectory( fullPixelHairs)
        cleanDirectory( hairs)
      }
      
      //----------------------------------------------
      //----------------------------------------------
      def getFirstHairPixel(deltas: Range, delta_hairpixel:Map[Int, List[HairPixel]] ):Option[HairPixelDelta]={
        if( deltas.isEmpty ) None
        else{
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
      }
      
      //----------------------------------------------
      // get Neighbour ( raising delta for the moment)
      // update 05/05/2016 : consume all the nextpixels, delta { -1, 0 , 1}+currentDelta
      //----------------------------------------------
      def getNextPixels( currentpixel: HairPixelDelta,hairs: DeltaHairs, deltas: Range )={
        val listx=List( currentpixel.hp.x, currentpixel.hp.x+1, currentpixel.hp.x-1)
        val nextdeltas=(0 to 1).map( _ + currentpixel.delta )
        
        val result=for( nextdelta <- nextdeltas )yield{
          if( deltas.contains(nextdelta)){
            val lPossiblepixels=hairs.get( nextdelta).toList.flatten.filter{ p => listx.contains( p.x)}               
            lPossiblepixels.map{ p=> HairPixelDelta(p, nextdelta)}
          }else{
            List.empty
          }
        }
        result.flatten.toList.filterNot( _ == currentpixel)
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
            
            if( listremoveatdelta.nonEmpty ){
              delta_phair._1 -> delta_phair._2.filterNot( listremoveatdelta contains _)
            }else
              delta_phair
        }               
        result
      }
      
      // once we detected a hair, with have to remove all the pixels around
      // for the moment, only the ones from the left to the right, not delta up / down...
      // to fix it.
      // threshold: default 0.4 , threshold for prHair (probabilty to be a hair)
      @tailrec
      def removeHairPixels( pixelstocheck: List[HairPixelDelta], allpixelhairs: DeltaHairs, pixeltoremove: List[HairPixelDelta] =List.empty, threshold : Double =0.4):List[HairPixelDelta] ={
        
        if( pixelstocheck.isEmpty ) {
          pixeltoremove
        }else{
          val currentpixel= pixelstocheck.head
          val neighbours = List( currentpixel.hp.x+1, currentpixel.hp.x-1, currentpixel.hp.x)
          
          
         // val allpixeldelta=(allpixelhairs.get(currentpixel.delta) ++ allpixelhairs.get(currentpixel.delta+1) ++ allpixelhairs.get(currentpixel.delta-1)).flatten
          
          val listDelta = (-2 to 2).map( _ + currentpixel.delta)
          
          val nextToVisit=(for( currentdelta <- listDelta )yield{
            (allpixelhairs.get(currentdelta).toList.flatten.filter{p=> neighbours contains p.x}.map{
              p => new HairPixelDelta(p, currentdelta)
            }).filter (_.hp.prHair > threshold).filterNot( pixeltoremove contains _).filterNot( pixelstocheck contains _)
          }).flatten
          
          //visit if not present in pixelstocheck and pixeltoremove and prHair > 0.5
         // val nextToVisit=(allpixeldelta.toList.filter{p=> neighbours contains p.x}.map{
         //   p => new HairPixelDelta(p, currentpixel.delta)
         // }).filter (_.hp.prHair > 0.1).filterNot( pixeltoremove contains _).filterNot( pixelstocheck contains _)
          
          removeHairPixels( pixelstocheck.tail ++ nextToVisit, allpixelhairs, currentpixel::pixeltoremove)
        }
      }
      

      
      //----------------------------------------------
      // Consume only >0.5 pr for the moment
      //
      // update: 5/05/2016 
      //         for zoneFilter( ourmap, Pixel(37,568), Pixel(100,650))  
      //         we consume only downloading the trend, we should be able to consume backward(future version) or
      //         at least on direct Neighbour
      //----------------------------------------------
      @tailrec
      def consume(currentpixel: HairPixelDelta, hairs: DeltaHairs, deltas: Range, currentPixel: List[HairPixelDelta], howtosort: ( HairPixelDelta, HairPixelDelta )=> Boolean
        = (A,B) => A.hp.prHair>B.hp.prHair    
      ): List[HairPixelDelta]={
        val next=getNextPixels( currentpixel, hairs, deltas).filterNot( currentPixel contains _).filter(_.hp.prHair>0.5d).sortWith(howtosort).headOption
        
        
        val result= currentPixel ++ next
        
        next match{
          case None => result
          case Some(nextcurrentpixel)=>
            consume(  nextcurrentpixel, hairs, deltas, result, howtosort)
        }     
      }
      
      /*
       * 


var arrayLength = this.inputWires.length;
var inputIndex=-1;
for (var i = 0; i < arrayLength; i++) {
    if( this.inputWires[i][0]==msg.idSender ){
    	inputIndex = i;
    }
}
       * 
       */
      
      def consumeHairs(allthepixels: DeltaHairs, deltaRange: Range, imgDst: ImagePlus, currentId: Int=0)(implicit dst:destinationFiles):Unit={
        if( currentId > 40){
          IJ.save( imgDst , dst.resultImage + s"full_Hairs.tif")
        }
        else{
          val optStartPixel=getFirstHairPixel(  deltaRange, allthepixels)
          optStartPixel.map{startPixel=>
            
            println(s"Start point for id$currentId, $startPixel")
            
            val howtosortRight= ( A: HairPixelDelta, B: HairPixelDelta )=> {
              if(A.delta == B.delta && A.hp.prHair == B.hp.prHair) A.hp.x > B.hp.x //high x better
              else if( A.delta == B.delta ) A.hp.prHair > B.hp.prHair  //higher pr better
              else A.delta > B.delta //higher delta better
            }
            
            val howtosortLeft= ( A: HairPixelDelta, B: HairPixelDelta )=> {
              if(A.delta == B.delta && A.hp.prHair == B.hp.prHair) A.hp.x < B.hp.x //lowest x better
              else if( A.delta == B.delta ) A.hp.prHair > B.hp.prHair  //higher pr better
              else A.delta > B.delta //higher delta better
            }
            
            val ourhairpixels=
              consume( startPixel,allthepixels, deltaRange, List(startPixel), howtosortRight)  ++
              consume( startPixel,allthepixels, deltaRange, List(startPixel), howtosortLeft)
            
            
            val img_oneHaire=createBlankImage( "oneHairGrouping", imgDst)
            val pixeltoremove=removeHairPixels( ourhairpixels,allthepixels)
            
            val proc_fullimg = imgDst.getProcessor
            val proc= img_oneHaire.getProcessor
            proc.setColor(255)
            proc_fullimg.setColor(255)
            
            val deltas=pixeltoremove.map(_.delta).distinct
            
            
            //Simple filter...
            if( deltas.min < -40 || deltas.max >(20))
            pixeltoremove.map{
               pixels =>
                 proc.drawPixel(pixels.hp.x, pixels.hp.y.get)
                 //proc_fullimg.drawPixel(pixels.hp.x, pixels.hp.y.get)
                 
            }
            IJ.save( img_oneHaire , dst.fullPixelHairs+ s"${currentId}_Hair_from_${deltas.min}_to_${deltas.max}.tif")
            
            proc.setColor(WHITE)
            proc.fill()
            proc.setColor(RED)
            ourhairpixels.map{
               pixels =>
                 proc.drawPixel(pixels.hp.x, pixels.hp.y.get)    
                 proc_fullimg.drawPixel(pixels.hp.x, pixels.hp.y.get)
                 
            }
            IJ.save( img_oneHaire , dst.hairs + s"${currentId}_Hair_from_${deltas.min}_to_${deltas.max}.tif")
            
            
            
            val updatemapDeltaHairPixels = cleanHairsPixels( allthepixels, pixeltoremove) 
            
            consumeHairs( updatemapDeltaHairPixels, deltaRange, imgDst, currentId+1)
          }.getOrElse{
            IJ.save( imgDst , dst.resultImage + s"endfull_Hairs.tif")
            

          }
        }
      }
      
      
      /*
       * for debug purpose we want to filter the zone available for the search of the hairs
       */
      def zoneFilter( fullpicture: DeltaHairs, start: Pixel, end: Pixel):DeltaHairs={
        fullpicture.map{
          delta_pixels => (delta_pixels._1, delta_pixels._2.filter { p => p.containedBy( start, end) })
        }
      }
      
      
      "using first hairpixel at 1.0d pr" in {
        //val img=IJ.openImage("/home/sosthene/Downloads/crop_wt_me10_2.tif")
         val img=IJ.openImage("images/wt_me10_2.tif")
        
        val deltaRange=(-300 to 300 by 1)
        
        cleanResultDirectory()
        implicit val searchtoutside =new fuzzySearchOutsidersPixelUsingMultiplePolyRegression(50,1)
        implicit val dd =new searchWhiteToBlackLine();
        implicit val dst=destinationFiles( "poly_(50,1)_searchWiteToBlack_consumeLeft_limitedZone/")
        
        //
        // save file or read file to get the map
        //
        val mapDeltaHairPixels= if( false){
           val ourmap= hairsCount(img,deltaRange)
           val oos = new ObjectOutputStream(new FileOutputStream("saveMapHairs"))
           oos.writeObject(ourmap)
           oos.close
           ourmap
        }else{
          val ois = new ObjectInputStream(new FileInputStream("saveMapHairs" ))
          val ourmap=ois.readObject().asInstanceOf[DeltaHairs]
         // zoneFilter( ourmap, Pixel(37,568), Pixel(100,650))
          ourmap
        }
        
        
        
//
//  // (3) read the object back in
        
        
        if( true){
        consumeHairs(mapDeltaHairPixels, deltaRange, img.copyToNewImg("workingcopy"))
        
        }else{
    
        val optStartPixel =    Some( HairPixelDelta( HairPixel( 204, 54,1.0,Some(18)),-165))  // getFirstHairPixel(  deltaRange, mapDeltaHairPixels)
        println("optStartPixel:"+optStartPixel )
        
        optStartPixel.map{startPixel=>
          val ourhairpixels=consume( startPixel,mapDeltaHairPixels, deltaRange, List(startPixel) )
          println("Pixels size:"+ourhairpixels.size)
          
          //draw it
          val img_oneHaire=createBlankImage( "oneHairGrouping", img)
          val proc= img_oneHaire.getProcessor
          proc.setColor(255)
          
          ourhairpixels.map{
             pixels =>
               proc.drawPixel(pixels.hp.x, pixels.hp.y.get)
               
          }
          IJ.save( img_oneHaire , dst.resultImage + s"oneHair.tif")
          
          //Cleaning:
          val pixeltoremove=removeHairPixels( ourhairpixels,mapDeltaHairPixels)
          proc.setColor(0)
          proc.fill()
          proc.setColor(255)
          pixeltoremove.map{
             pixels =>
               proc.drawPixel(pixels.hp.x, pixels.hp.y.get)
               
          }
          IJ.save( img_oneHaire , dst.resultImage  + s"removeoneHair.tif")
        }
        
        }
//          
//          
//          //test remove
//          val img_removeoneHaire=createBlankImage( "oneHairGrouping", img)
//          val proctest= img_removeoneHaire.getProcessor
//          proctest.setColor(RED)
//          proctest.fill()
//          proctest.setColor(BLACK)
//          cleanHairsPixels( mapDeltaHairPixels, pixeltoremove).toList.map(_._2).flatten.foreach{ hairpixel =>
//            proctest.drawPixel(hairpixel.x, hairpixel.y.get)
//          }
//          IJ.save( img_removeoneHaire , DIRECTORY_RESULTIMAGE + s"resultRemoveoneHair.tif")
//          
//          
//          //search second hair
//          
//          val updatemapDeltaHairPixels = cleanHairsPixels( mapDeltaHairPixels, pixeltoremove) 
//          //start with first pixel on the higher delta       
//        val optStartPixel2 = getFirstHairPixel(  deltaRange, updatemapDeltaHairPixels)
//        println("optStartPixel2:"+optStartPixel2 )
//        
//        optStartPixel2.map{startPixel2=>
//          val ourhairpixels=consume( startPixel2,updatemapDeltaHairPixels, deltaRange , List(startPixel2))
//          println("Pixels size:"+ourhairpixels.size)
//          
//          println("remove delta -213 result:"+pixeltoremove.filter( _.delta == -213))
//          
//          println("ourhairpixels delta -213 result:"+ourhairpixels.filter( _.delta == -213))
//          
//          
//          //draw it
//          val img_oneHaire=createBlankImage( "oneHairGrouping", img)
//          val proc= img_oneHaire.getProcessor
//          proc.setColor(255)
//          
//          ourhairpixels.map{
//             pixels =>
//               proc.drawPixel(pixels.hp.x, pixels.hp.y.get)
//               
//          }
//          IJ.save( img_oneHaire , DIRECTORY_RESULTIMAGE + s"twoHair.tif")
//          }
          
 //       }
      }
    }
  }
}