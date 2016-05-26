package algo
import ij.IJ
import algo.SearchHairs._
import algo._
import algo.SearchHairs._
import imagej.tools._
import java.awt.Color._
import ij.ImagePlus
import akka.actor.ActorRef

object hairRegrouping {

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
      // Search a pixel that can be part of a hair. 
      // start from high delta to lower, looking for p(hair)=1.0
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
      // update 05/05/2016 : consume all the nextpixels, delta {  0 , 1}+currentDelta
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
      
      
      //-----------------------------------------------------------------------
      // remove from allpixelhairs the pixels 
      // present in pixeltoremove: List[HairPixelDelta]
      //-----------------------------------------------------------------------
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
      
      
      //-----------------------------------------------------------------------
      // once we detected a hair, with have to remove all the pixels around
      // for the moment, only the ones from the left to the right, not delta up / down...
      // to fix it.
      // threshold: default 0.4 , threshold for prHair (probabilty to be a hair)
      // pixelLiberty : 1 => we move to the closest pixels => distance beetwen current
      //                     pixel and the next
      //-----------------------------------------------------------------------
      @tailrec
      def removePossibleHairPixels( pixelstocheck: List[HairPixelDelta], allpixelhairs: DeltaHairs, 
              pixeltoremove: List[HairPixelDelta] =List.empty, threshold : Double =0.4, pixelLiberty: Int=1):List[HairPixelDelta] ={
        
        if( pixelstocheck.isEmpty ) {
          pixeltoremove
        }else{
          val currentpixel= pixelstocheck.head
          val neighbours = (-pixelLiberty to pixelLiberty).map(_ + currentpixel.hp.x);//List( currentpixel.hp.x+1, currentpixel.hp.x-1, currentpixel.hp.x)
          
          
         // val allpixeldelta=(allpixelhairs.get(currentpixel.delta) ++ allpixelhairs.get(currentpixel.delta+1) ++ allpixelhairs.get(currentpixel.delta-1)).flatten
          val listDelta = (-pixelLiberty to pixelLiberty).map( _ + currentpixel.delta)
          
          val nextToVisit=(for( currentdelta <- listDelta )yield{
            (allpixelhairs.get(currentdelta).toList.flatten.filter(_.prHair > threshold).filter{p=> neighbours contains p.x}.map{
              p => new HairPixelDelta(p, currentdelta)
            }).filterNot( pixeltoremove contains _).filterNot( pixelstocheck contains _)
          }).flatten
          
          //visit if not present in pixelstocheck and pixeltoremove and prHair > 0.5
         // val nextToVisit=(allpixeldelta.toList.filter{p=> neighbours contains p.x}.map{
         //   p => new HairPixelDelta(p, currentpixel.delta)
         // }).filter (_.hp.prHair > 0.1).filterNot( pixeltoremove contains _).filterNot( pixelstocheck contains _)
          
          removePossibleHairPixels( pixelstocheck.tail ++ nextToVisit,allpixelhairs, currentpixel::pixeltoremove,  threshold, pixelLiberty)
        }
      }
      

      
      //-----------------------------------------------------------------------
      // Consume only >0.5 pr for the moment
      // direction depending on howtosort 
      // ( we can prefer right or left pixel depending on the sorting)
      //       
      //
      // update: 5/05/2016 
      //         for zoneFilter( ourmap, Pixel(37,568), Pixel(100,650))  
      //         we consume only downloading the trend, we should be able to consume backward(future version) or
      //         at least on direct Neighbour
      //-----------------------------------------------------------------------
      @tailrec
      def consume(currentpixel: HairPixelDelta, hairs: DeltaHairs, deltas: Range, currentPixel: List[HairPixelDelta], howtosort: ( HairPixelDelta, HairPixelDelta )=> Boolean
        = (A,B) => A.hp.prHair>B.hp.prHair    
      ): List[HairPixelDelta]={
        val next=getNextPixels( currentpixel, hairs, deltas).filter(_.hp.prHair>0.5d).filterNot( currentPixel contains _).sortWith(howtosort).headOption
        
        
        val result= currentPixel ++ next
        
        next match{
          case None => result
          case Some(nextcurrentpixel)=>
            consume(  nextcurrentpixel, hairs, deltas, result, howtosort)
        }     
      }
      
      case class PossiblePixelsHair( hairId: Int, pixels: List[HairPixelDelta] )
      
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
              
      /*
       * consume all the hairs, assigning to them ( from 0 to ...) 
       * 
       * dst: where to save the debug files
       * @return 
       */
      def consumeHairs(allthepixels: DeltaHairs, deltaRange: Range, imgDst: ImagePlus, 
          optActor: Option[ActorRef]=None, currentId: Int=0, debug: Boolean=false, thresholdHair:Double= 0.4d)
          (implicit dst:destinationFiles):List[PossiblePixelsHair]={
        if( currentId > 260){
          IJ.save( imgDst , dst.resultImage + s"full_Hairs.tif")
          List.empty[PossiblePixelsHair]
        }
        else{
          val optStartPixel=getFirstHairPixel(  deltaRange, allthepixels)
          val response= optStartPixel.map{startPixel=>
            
              println(s"Start point for id$currentId, $startPixel")
              
              
              
              val ourhairpixels=
                consume( startPixel,allthepixels, deltaRange, List(startPixel), howtosortRight)  ++
                consume( startPixel,allthepixels, deltaRange, List(startPixel), howtosortLeft)
              
              
              //crash compiler: val pixeltoremove=removePossibleHairPixels( ourhairpixels,allthepixels, threshold=01.0d,3)  
              val pixeltoremove=removePossibleHairPixels( ourhairpixels,allthepixels,List.empty[HairPixelDelta], threshold=0.6d,1)              
              val proc_fullimg = imgDst.getProcessor                           
              proc_fullimg.setColor(255)              
              val deltas=pixeltoremove.map(_.delta).distinct
              
              
              //
              //   DEBUG
              //
              if( debug ||  currentId==6){ //generate files for debugging purpose (one for each hair)
                val img_oneHaire=createBlankImage( "oneHairGrouping", imgDst)
                val proc= img_oneHaire.getProcessor
                proc.setColor(255)
                
                
                
                
                // F(detla)=Max(x)-Min(x)
                val fdelta=pixeltoremove.groupBy(_.delta).map{
                  delta_pixel => 
                    (delta_pixel._1,
                    delta_pixel._2.map(_.hp.x).max-
                    delta_pixel._2.map(_.hp.x).min)
                }
                
                val mapDeltaX=pixeltoremove.groupBy(_.delta)
                
                
                //pixels orderd by X
                def evalNbrDifferentHair( pixels: List[HairPixelDelta]):Int={
                  pixels match{
                    case l::Nil => 1
                    case l::ls =>
                      if(  ls.head.hp.x  - l.hp.x  > 1 ) 1+evalNbrDifferentHair(ls)
                      else evalNbrDifferentHair(ls)
                    case _ => 1 
                  }
                }
     
                
                
                //count the nbr of time we have free space beetween each x per delta
                val nbrHairPerDelta=mapDeltaX.map{  case (delta, listphair) =>
                  (delta,evalNbrDifferentHair( listphair.sortBy(_.hp.x) ))
                }
                
                val maxNbrHair=nbrHairPerDelta.map(_._2).max
                val deltaMaxNbrHair = nbrHairPerDelta.filter(_._2==maxNbrHair).map(_._1).toList.sortBy{x=>x}.head //smallest delta with higher nbr hairs
                
                //
                // we have mas nbr hair along delta. we need to consume the hair starting from
                // maxNbrHair different point. let's try to consume each point with each directions (left/right)
                // and pick the largest one
                //
                def consumefullHair(startIntPoint: HairPixelDelta, fullHair: DeltaHairs, deltas: Range, currentPixel: List[HairPixelDelta], howtosort: ( HairPixelDelta, HairPixelDelta )=> Boolean
                  = (A,B) => A.hp.prHair>B.hp.prHair    
                ): List[HairPixelDelta]={
                  val next=getNextPixels( startIntPoint, fullHair, deltas).filterNot( currentPixel contains _).sortWith(howtosort).headOption
                  
                  println(s"start point:$startIntPoint, next:$next");
                  
                  val result= currentPixel ++ next
                  
                  next match{
                    case None => result
                    case Some(nextcurrentpixel)=>
                      consumefullHair(  nextcurrentpixel, fullHair, deltas, result, howtosort)
                  }     
                }
                val startingpoint= mapDeltaX.get( deltaMaxNbrHair).head.head
                println(s"startingpoint: ${startingpoint}")
                val hair1=consumefullHair(  startingpoint, mapDeltaX.map{x=>(x._1, x._2.map(_.hp))}, deltaRange,List.empty[HairPixelDelta],howtosortRight)
                val hair2=consumefullHair(  startingpoint, mapDeltaX.map{x=>(x._1, x._2.map(_.hp))}, deltaRange,List.empty[HairPixelDelta],howtosortLeft)
                
                println(s"haire size: ${hair1} , ${hair2}")
                
                val img_oneHairConsuming=createBlankImage( "oneHairConsuming", imgDst)
                val proc_img_oneHairConsuming=img_oneHairConsuming.getProcessor
                proc_img_oneHairConsuming.setColor(RED)
                hair1.foreach{
                   pixels =>
                     proc_img_oneHairConsuming.drawPixel(pixels.hp.x, pixels.hp.y.get)                     
                }
                proc_img_oneHairConsuming.setColor(BLUE)
                hair2.foreach{
                   pixels =>
                     proc_img_oneHairConsuming.drawPixel(pixels.hp.x, pixels.hp.y.get)                     
                }
                IJ.save( img_oneHairConsuming , dst.fullPixelHairs+ s"${currentId}_img_oneHairConsuming.tif")
                //test picking first pixel
                
                //
                
                
                val minx=fdelta.keys.min
                val miny=fdelta.values.min
                val fdelta_width=fdelta.keys.max-fdelta.keys.min
                val fdelta_height=fdelta.values.max-fdelta.values.min
                
                val img_delta_vs_nbrHair=createBlankImage( "fdelta", fdelta_width, 100)
                val proc_delta_vs_nbrHair=img_delta_vs_nbrHair.getProcessor
                nbrHairPerDelta.foreach{
                  case(delta,x) => proc_delta_vs_nbrHair.drawPixel( delta - minx, x*10)
                  println(s"Draw at x:${delta-minx}, y:${x-miny}")
                }
                IJ.save( img_delta_vs_nbrHair , dst.fullPixelHairs+ s"${currentId}_delta_vs_nbrHair.tif")
                

                val img_delta_vs_x=createBlankImage( "fdelta", fdelta_width, fdelta_height)
                val proc_delta_vs_x=img_delta_vs_x.getProcessor
                
                println(s"New fdelta fdelta_width: $fdelta_width, fdelta_height: $fdelta_height")
                proc_delta_vs_x.setColor(RED) 
                
//                fdelta.foreach{
//                  case(delta,x) => proc_delta_vs_x.drawPixel( delta - minx, x -miny)
//                  println(s"Draw at x:${delta-minx}, y:${x-miny}")
//                }
                
                
//                fdelta.foreach{
//                  delta_x => proc_delta_vs_x.drawPixel( delta_x._1 - minx, delta_x._2 -miny)
//                  println(s"Draw at x:${delta_x._1-minx}, y:${delta_x._2-miny}")
//                }
                
                IJ.save( img_delta_vs_x , dst.fullPixelHairs+ s"${currentId}_delta_vs_min_max_x.tif")
                
                //Simple filter...
                if( deltas.min < -40 || deltas.max >(20))
                pixeltoremove.map{
                   pixels =>
                     proc.drawPixel(pixels.hp.x, pixels.hp.y.get)
                     proc_fullimg.drawPixel(pixels.hp.x, pixels.hp.y.get) //show whole pixels consumed
                     
                }
                
                
                IJ.save( img_oneHaire , dst.fullPixelHairs+ s"${currentId}_Hair_from_${deltas.min}_to_${deltas.max}.tif")
                
                proc.setColor(WHITE)
                proc.fill()
                proc.setColor(RED)
                proc_fullimg.setColor(RED)
                ourhairpixels.map{
                   pixels =>
                     proc.drawPixel(pixels.hp.x, pixels.hp.y.get)    
                     proc_fullimg.drawPixel(pixels.hp.x, pixels.hp.y.get) //show pixel consumed only for hair pixel
                     
                }
              
              
                IJ.save( img_oneHaire , dst.hairs + s"${currentId}_Hair_from_${deltas.min}_to_${deltas.max}.tif")
              }
                
                
              val updatemapDeltaHairPixels = cleanHairsPixels( allthepixels, pixeltoremove) 
              
              val currentPixelHair=PossiblePixelsHair(currentId,ourhairpixels)
              optActor.map{
                actor => actor ! currentPixelHair
              }
              
              currentPixelHair::consumeHairs( updatemapDeltaHairPixels, deltaRange, imgDst,optActor, currentId+1)
          }.getOrElse{
              IJ.save( imgDst , dst.resultImage + s"endfull_Hairs.tif")
              List.empty[PossiblePixelsHair]
          }
          response
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
}