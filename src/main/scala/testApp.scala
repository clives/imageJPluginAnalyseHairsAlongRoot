import ij._
import ij._
import ij.plugin.filter._
import ij.process.ImageProcessor
import ij.process.BinaryProcessor
import ij.process.ByteProcessor
import org.apache.commons.math3.fitting.WeightedObservedPoints
import org.apache.commons.math3.fitting.PolynomialCurveFitter
import ij.gui.NewImage
import org.apache.commons.math3.analysis.polynomials.PolynomialFunction



object tools{
  implicit class xxx( ourcolor:Array[Int]){
    def isWhite() = { 
    	 if( ourcolor(0) >0) true;
    	 else false;
    }
}
  
  
  def checkOrientation(img: ImagePlus)={
    // first vertical line at 10 pixel
    val crossFirstVertical =( 0 to img.getHeight()).toList.dropWhile{ oury => !img.getPixel(1, oury).isWhite() }.headOption
      
    val crossSecondVertical =( 0 to img.getHeight()).toList.exists { oury => img.getPixel( img.getWidth-1, oury).isWhite() }
    
    val crossFirstHorizontal =( 0 to img.getWidth).toList.dropWhile { ourx => !img.getPixel(ourx,1).isWhite() }.headOption
      
    val crossSecondHorizontal =( 0 to img.getWidth).toList.exists { ourx => img.getPixel(ourx, img.getHeight-1).isWhite() }
    
    
    println(s" crossFirstVertical:$crossFirstVertical ,crossSecondVertical:$crossSecondVertical")
    println(s" crossFirstHorizontal:$crossFirstHorizontal ,crossSecondHorizontal:$crossSecondHorizontal")
  }

  
 
  
  def haireTravel(  startposition: (Int,Int), colorMargin: Int , img: ImagePlus, imp: ImageProcessor )={
    
    
    val color = img.getPixel( startposition._1, startposition._2)(0)
    
    
    def getAllPossiblePosition( from: (Int,Int) ): List[ (Int,Int)] ={
      List(  (-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (0,1)).map{
        change =>  ( from._1 + change._1, from._2+change._2)
      }
    }
    
    /*
     * consume all pixel compatible with current color
     */
    def consumeAllPixel(  currentposition: (Int,Int), visitedpixels: List[(Int,Int)]  = List.empty,nextpixels: List[ (Int,Int)] = List.empty ):List[(Int,Int)]={
      
      println("currentposition:"+currentposition)
      
      val updatedvisitedpixels = List( currentposition) ++ visitedpixels 
      val currentcolor = img.getPixel( currentposition._1, currentposition._2)(0)
      
      if( visitedpixels.size < 20 ){
        val nextPosition = getAllPossiblePosition( currentposition).filter{ p => !nextpixels.contains(p) && !visitedpixels.contains(p) }.filter{
          position =>
            val color = img.getPixel( position._1, position._2)(0)
            val diffColor=Math.abs(color - currentcolor)
            diffColor < colorMargin 
        }
        
        println("jjj visitedpixels:"+visitedpixels.size)
        
        val allpossibleposition = nextPosition ++ nextpixels
        
        if( allpossibleposition.nonEmpty )
          consumeAllPixel(  allpossibleposition.head, updatedvisitedpixels, allpossibleposition.tail)
        else
          visitedpixels
      }else{
        println("cc visitedpixels:"+visitedpixels.size)
        visitedpixels
      }
    }
    
    
    val resultpixel=consumeAllPixel(  startposition).reverse
    println("result:"+resultpixel)
    var index=0;
    resultpixel.foreach{
      currentposition =>
        imp.drawPixel( currentposition._1, currentposition._2)
        img.updateImage()
        IJ.save(img, "image_"+index+".jpg")
        index = index+1
    }
    IJ.save(img, "result.tiff")
    img.draw()
    /*
     *  3  4  5
     *  2  R  6
     *  1  0  7
     */
    def nextRobotPosition( currentPosition: (Int,Int), newposition:Int)={
      
      
      val updatetuple= newposition %8 match{
        case 0 => ( 0,1) 
        case 1 => ( -1,1)
        case 2 => (-1,0)
        case 3 => (-1,-1)
        case 4 => (0,-1)
        case 5 => (1,-1)
        case 6 => (1,0)
        case 7 => (1,1)
        case x => throw new Exception(s" $x case not possible, issue with algorithm");
      }
      
      ( updatetuple._1 + currentPosition._1 , updatetuple._2 + currentPosition._2)
    }

    def nextRobotPosition4Movement( currentPosition: (Int,Int), newposition:Int)={
      val updatetuple= newposition %4 match{
        case 0 => ( 1,1) 
        case 1 => (-1,0)
        case 2 => (0,-1)
        case 3 => (1,0)
        case x => throw new Exception(s" $x case not possible, issue with algorithm");
      }
      
      ( updatetuple._1 + currentPosition._1 , updatetuple._2 + currentPosition._2)
    }

    
    def robotMovement( currentposition: (Int,Int), currentcolor: Int, currentdirection: Int, pixelvisited: List[(Int,Int)]):List[(Int,Int)]={
  
      println(s"call robotMovement: ${currentposition}, pixelvisited.size:${pixelvisited.size}");
      
      //imp.drawDot( currentposition._1, currentposition._2)
      
      if( pixelvisited.size > 10000 ) return pixelvisited
      

      val nextmovement4=( 0 to 3 ).toList.map{  
        nbrmovement => 
          
          val updateddirection= if( nbrmovement ==0 ){
            currentdirection +1
          }else{
            currentdirection + nbrmovement + 3  //start without movement
          }
          
          val newposition4mov = nextRobotPosition4Movement( currentposition, updateddirection)
          
          val newposition = if( nbrmovement ==0 ) 
            nextRobotPosition( currentposition,  currentdirection + 1 )
            else nextRobotPosition( currentposition,  currentdirection + nbrmovement + 7)// 7=> -1
          
          val color = img.getPixel( newposition._1, newposition._2)(0)
          val diffColor=Math.abs(color - currentcolor)
          if( diffColor >= colorMargin ) println(s"we have diff color, $diffColor");
          println(s"diffColor: $diffColor, newposition:$newposition, color:$color, currentcolor:$currentcolor");
          pixelvisited.contains(newposition) || diffColor >= colorMargin
      }.headOption

      
      //always right => 1+, move only if the diff color is compatible with margin
      val nextmovement=( 0 to 7 ).toList.dropWhile { 
        nbrmovement => 
          
          val newposition = if( nbrmovement ==0 ) 
            nextRobotPosition( currentposition,  currentdirection + 1 )
            else nextRobotPosition( currentposition,  currentdirection + nbrmovement + 7)// 7=> -1
          
          val color = img.getPixel( newposition._1, newposition._2)(0)
          val diffColor=Math.abs(color - currentcolor)
          if( diffColor >= colorMargin ) println(s"we have diff color, $diffColor");
          println(s"diffColor: $diffColor, newposition:$newposition, color:$color, currentcolor:$currentcolor");
          pixelvisited.contains(newposition) || diffColor >= colorMargin
      }.headOption
      
      nextmovement match{
        case Some(mv) =>
         // val futurposition=nextRobotPosition( currentposition,  currentdirection + 1 + mv )
          
          val futurposition = if( mv ==0 ) 
            nextRobotPosition( currentposition,  currentdirection + 1 )
            else nextRobotPosition( currentposition,  currentdirection + mv + 7)// 7=> -1
            
          val color = img.getPixel( futurposition._1, futurposition._2)(0)
          
          if( mv ==0 )
            robotMovement( futurposition, color, currentdirection + 1 + mv, pixelvisited :+ currentposition)
          else 
            robotMovement( futurposition, color, currentdirection + mv +7, pixelvisited :+ currentposition)
          
        case None => 
          println(s"Done at position: ${currentposition}")
          println(s"List positio: $pixelvisited");
          pixelvisited
      }
      
      
      
      //val ( newposition, newdirection)= newRobotPosition( currentdirection, currentposition)
      
    }
    
    
  }
}


object testApp extends App {
  println("test")  
  
  val img=IJ.openImage("/home/sosthene/Downloads/crop_wt_me10_2.tif")
  
 // tools.haireTravel( (48,94), 3, img, img.getProcessor)
  
  //img.show();
  

    import imagej.tools._            

    //img.clone().asInstanceOf[ImagePlus].show
    
    val originalimg =  new ImagePlus( "original", img.getProcessor.createImage())
    originalimg.show
    
    val finalimg =  new ImagePlus( "original", img.getProcessor.createImage())
    finalimg.show
    
    
    imagej.tools.applyMaximum( img, 15)
 
    val imageb =imagej.tools.createBinary( img, true)
    
                
          
    //We have white image, search all white pixels
    val ourWhitePixels=for(  x <- 0  until imageb.getWidth; y <- 0 until imageb.getHeight; if(imageb.getPixel(x,y).isWhite)   )yield{
      (x,y)
    }
    
    println(s"White pixels: ${ourWhitePixels.size}")
    
    //val y = { 540.0, 160.0, -140.0, -360.0, -480.0, -560.0, -540.0, -440.0, -260.0, 0.0, 340.0};              
    val obs = new WeightedObservedPoints();
    ourWhitePixels.foreach{ p =>
      obs.add( p._1.toDouble, p._2.toDouble )
    }
    val polyfitter= PolynomialCurveFitter.create(3);
    val coef=polyfitter.fit(obs.toList());
    val ourfunction = new PolynomialFunction(coef )
    
    println(s"Done ${coef.mkString(",   ")}");
    
    val proceB=finalimg.getProcessor
    proceB.setColor(java.awt.Color.BLACK)
    for( x<- 0 until imageb.getWidth ){
      
      println("coef(1):"+(x*coef(1)))
      println("x + coef (0) + x* coef(1) :"+( x + coef (0) + x* coef(1)) )
      
      
      proceB.drawPixel(x, 10 + (ourfunction.value( x)).toInt )
      //proceB.drawPixel(x, 100 + (coef (0) +( x* coef(1) ) + ( x*x*coef(2)) + (x*x*x*coef(3)) ).toInt) 
    }
     img.show()
    img.updateAndDraw()
   //
    IJ.save(img, "testcropBW.tiff")
    
    //(java.lang.String title, int width, int height, int slices, int options) 
    val blanckimage=createBlankImage( "white", img)
    val pori=originalimg.getProcessor
    val pixelOverTheLine = for( x <- 0 until img.getWidth )yield{
      val valueatx=pori.getPixel(  x , ourfunction.value(x).toInt + 10  )
      
      blanckimage.getProcessor.drawPixel(x , valueatx)
      (x,valueatx)
    }
     
    val polyfitterOverLine= PolynomialCurveFitter.create(8);
    val obsOverLine = new WeightedObservedPoints();
    pixelOverTheLine.foreach{ p =>
      obsOverLine.add( p._1.toDouble, p._2.toDouble )
    }
    val ourfunctionOverLine = new PolynomialFunction(polyfitterOverLine.fit(obsOverLine.toList) )
    
    val meanVariation=(0 until img.getWidth).zip(pixelOverTheLine.map(_._2)).map{ x=> Math.abs(ourfunctionOverLine.value(x._1)-x._2)  }.reduce(_+_) / pixelOverTheLine.size
    
    val xOverLineAndOverMean = pixelOverTheLine.filter{
        position => 
          position._2 < ourfunctionOverLine.value( position._1) &&
          (  ourfunctionOverLine.value( position._1)  - position._2  > meanVariation)
                   
    }
    
    xOverLineAndOverMean.foreach{
      pixel =>
        blanckimage.getProcessor.drawPixel( pixel._1, pixel._2 + 100)
    }
    
    def regroupPixelOverLine( overlinepixel: List[(Int,Int)], count :Int=0):Int ={
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
    
    //(24,112), (25,99), (26,87), (27,86), (28,101), 
    //(61,88), (62,84), (63,80), (64,86), 
    //(102,91), (105,96), (107,98), (110,96), (111,90), (112,79), (113,65), (114,72), (115,91), 
    //(157,120), (158,100), (159,87), (160,112), (161,125), 
    //(201,139), (202,126), (203,105), (204,109), (205,115), 
    //(269,94), (270,91), (271,94), (272,92), 
    //(315,100), (316,81), (317,74), (318,76), (319,114), 
    //(356,110), (357,90), (358,113)) 
    
    val nomber = regroupPixelOverLine( xOverLineAndOverMean.toList)
    
    println(s" nomber: $nomber ")
    println(s" xOverLineAndOverMean: $xOverLineAndOverMean ")
    println(s" meanVariation: $meanVariation ")
    
    for( x <- 0 until img.getWidth )yield{
      blanckimage.getProcessor.drawPixel(x , ourfunctionOverLine.value(x).toInt)      
    }
    //use originalimg and our equation
    
    
    blanckimage.show()
    blanckimage.updateAndDraw()
    finalimg.updateAndDraw()
    
    
//        for (double figure:y){
//            obs.add(1.0, figure);
//        }
//        final PolynomialCurveFitter fitter = PolynomialCurveFitter.create(2);
//        final double[] coeff = fitter.fit(obs.toList());
//        System.out.println("coef="+Arrays.toString(coeff));
        
        
                
                
             //   imgproce.threshold(600)

 // val newimgproce=imgproce//.resize( 160);

//  newimgproce.setColor(java.awt.Color.RED)
//  newimgproce.setLineWidth(10)
  
  /*
  ( 0 to 100 ).toList.foreach{
    x => newimgproce.drawDot(x, x)
  }*/
  

  //create img in background, click on current image should show this one
//  val imgjava=newimgproce.createImage()
  
  //val d=newimgproce.resize(100)
  //d.createImage()
 // img.setImage(imgjava)
  
 // tools.checkOrientation( img)
  
  
  
  //IJ.run(img, "Maximum...", "15")
  //IJ.run(img, "Make Binary","")
 // img.show() 
}