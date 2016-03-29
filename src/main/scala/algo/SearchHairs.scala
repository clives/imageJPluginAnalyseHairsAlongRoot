package algo

import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
import ij.ImagePlus
import org.apache.commons.math3.fitting.PolynomialCurveFitter
import org.apache.commons.math3.fitting.WeightedObservedPoints
import ij.IJ

object SearchHairs {
  import imagej.tools._
  
  final val DIRECTORY_RESULTIMAGE="imageresult/"
  import java.awt.Color._
  
  /*
   * pixelofinterestovery - Y -> list(pixel)
   */
  def followHair( pixelofinterestovery: Map[ Int, List[ Int]])={
    val listdelta = pixelofinterestovery.keys.toList.sortBy { x => -x } //can be generated with by ( 1,3,5,...)
    val maxdelta=pixelofinterestovery.keys.toList.sortBy(-_).head
    val margin =5
    
    
    type Delta = Int
    type IDHAIR=Int
    
    //start from larger delta, get all the hairs then start from the second larger delta ( removing before all the pixels
    //already consumed during the last turn. do it until we have no more delta to consume or the delta is to short ( <0 )
    def getallhairs( deltatostartwith: List[Delta], filtredpixelofinterestovery: Map[ Int, List[ Int]], intersult: List[((Delta,IDHAIR),List[Int])]=List.empty  ): List[((Delta,IDHAIR),List[Int])] ={
      val delta=deltatostartwith.head
      
      if( deltatostartwith.head < 5  || filtredpixelofinterestovery.isEmpty || filtredpixelofinterestovery(delta).isEmpty) return intersult
      
      val starthairatmaxdelta=filtredpixelofinterestovery(delta).foldLeft( List((filtredpixelofinterestovery(delta).head, List.empty[Int]))  ){
      (A,B) => if( Math.abs( A.head._1 - B ) >= 5) (B,List(B)) :: A else ( A.head._1, B:: A.head._2)::A.tail 
      }
      
      val startwith= filtredpixelofinterestovery(delta).head
      
      val allhaires=starthairatmaxdelta.zipWithIndex.flatMap{ startx_id=>
        val (( startx, delta), idhair) = startx_id
        regroupforx( startx, idhair)
      }
      
      val alldelta=allhaires.map(_._1._1).distinct
      val map_delta_x= (for( delta <- alldelta)yield{
        (delta,allhaires.filter( _._1._1 == delta).map( _._2).flatten)
      }).toMap
      
      //remove x already present in map_detla_x for each delta defined
      val newfiltredpixelofinterestovery=filtredpixelofinterestovery.map{ a => 
        val (delta, list_x) = a
        if( map_delta_x.isDefinedAt(delta) ){
          (delta,list_x.filter { x => ! map_delta_x(delta).contains(x) })
        }else a
      }
      
      
      getallhairs( deltatostartwith.tail, newfiltredpixelofinterestovery, intersult  ++ allhaires)
    }
    
    
    
    /*
     * @return map[ delta -> pixel ]
     * should return  List[ Hairid, Map[delta->pixel]] or  Map[  (delta,idhair) -> pixel ]
     */
    def regroupforx( currentx : Int, idhair:Int, currentdeltas: List[Delta] = listdelta, currentresult:Map[(Delta,IDHAIR),List[Int]]=Map.empty ):Map[(Delta,IDHAIR),List[Int]]={ //@return list delta
      if( currentdeltas.isEmpty ) currentresult
      else{
        val currentdelta= currentdeltas.head
        val optallvalidx = pixelofinterestovery.get(currentdelta)
        val result=optallvalidx.map{ allvalidx=> 
          allvalidx.filter{ x =>  Math.abs(x-currentx) < 5}
        }.getOrElse( List.empty[Int] )
        
        val newcurrentresult=currentresult + ( (currentdelta,idhair) -> result)
        
        
        
        if( result.nonEmpty )
          regroupforx( result.reduce(_+_)/ result.size , idhair,  currentdeltas.tail, newcurrentresult);
        else
          currentresult
      }
    }
    
    //regroup by x with margin of 5, keep all the values of x regrouped
    //to improve: the margin should apply not only using the first x but the whole xs matchins x
    //            would permit to avoid missing evaluation when we have a drop of water in the picture
    val starthairatmaxdelta=pixelofinterestovery(maxdelta).foldLeft( List((pixelofinterestovery(maxdelta).head, List.empty[Int]))  ){
      (A,B) => if( Math.abs( A.head._1 - B ) >= 5) (B,List(B)) :: A else ( A.head._1, B:: A.head._2)::A.tail 
    }
    
    println("Starting hairs:"+starthairatmaxdelta)
    
    val startwith= pixelofinterestovery(maxdelta).head
    

    
    val allhaires=starthairatmaxdelta.zipWithIndex.flatMap{ startx_id=>
      val (( startx, delta), idhair) = startx_id
      regroupforx( startx, idhair)
    }  
    
    //remove those haires from data
    allhaires
    
    
    println(s"result: ${allhaires.head}")
    allhaires
    
    getallhairs( listdelta,pixelofinterestovery);
  }
  
  /*
   * rotation - if true, take linear regression of the blank line, get the angle to convert it to
   *            angle, then rotate the image to get the line in // with x, permits to have the haires "right"
   *            (not implemented, only exprience)
   */
  def hairsCount( originalimg_high: ImagePlus, deltas: Range, rotation:Boolean = false):List[ (Int,Int)]={
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
    
    if( rotation ){
      val polyfitter= PolynomialCurveFitter.create(2); //Poly 3
      
    }
    
    val polyfitter= PolynomialCurveFitter.create(12); //Poly 3
    val coef=polyfitter.fit(obs.toList());
    println(s"coef : ${coef.mkString}");
    val ourfunction = new PolynomialFunction(coef )
    originalimg.updateAndDraw()
    
    val deltaVsHaire= deltas.toList.map{ delta =>
      val result= hairsCount( originalimg, ourfunction, delta, true)
      (delta, result)
    }
    
    // Delta ->  List( X of interest)
    val mapDeltaX= deltaVsHaire.map{ x=> (x._1,x._2._1)}.toMap
    val ourhairs=followHair(mapDeltaX)
    val imgfullhair=originalimg.copyToNewImg("fullHair" )
    val procfullhair=imgfullhair.getProcessor
    procfullhair.setColor( WHITE)
    
    val allidhairs=ourhairs.map( _._1._2).distinct
    
    allidhairs.map{ idhair =>
      ourhairs.filter{ delta_xs => delta_xs._1._2==idhair } .foreach{    
        delta_xs =>
          val ((delta,idhair), ourxs) =delta_xs
          ourxs.foreach{ x=>
            procfullhair.drawPixel(x,delta + ourfunction.value(x).toInt)
            
          }        
      }
      IJ.save( imgfullhair , DIRECTORY_RESULTIMAGE+ s"fullHair_id${idhair}.tif")
    }
    
    println(s"We have detected : ${deltaVsHaire} haires");
    
    imgBN.show()
    imgBN.updateAndDraw()
    
    originalimg.show();
    originalimg.updateAndDraw()
    
    deltaVsHaire.map{ x => (x._1, x._2._2)}
  }
  
  
  def hairsCount( img: ImagePlus, ourline: PolynomialFunction, delta: Int, debug: Boolean  ):(List[Int],Int)={
    
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
      
    
      IJ.save( img_x_color , DIRECTORY_RESULTIMAGE + s"img_x_color_delta${"%03d".format(delta)}.tif")
      IJ.save( imgcopy ,     DIRECTORY_RESULTIMAGE+ s"result_delta${"%03d".format(delta)}.tif")
    }
    
    
    
    
    // specialPixel =>  delta -> X,Color , return only the x
    ( specialPixel.map(_._1) , regroupPixelOverLine( specialPixel))
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