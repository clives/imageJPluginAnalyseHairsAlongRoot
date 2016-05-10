package algo

import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
import org.apache.commons.math3.fitting.PolynomialCurveFitter
import org.apache.commons.math3.fitting.WeightedObservedPoints
import SearchHairs._
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import org.apache.commons.math3.optimization.fitting.PolynomialFitter
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.analysis.UnivariateFunction
  

 /*
  * define service to identify the outsider pixels, the one who can be part of a hair 
  * for her difference of color in compare with the rest of the pixels along the line
  */
 trait SearchOutsiderPixels{  
    /*
     * line : line as (x, color), x: 0 to image with
     * @return outside pixel as  List( x:[a to b]*( x, color), x:[c,d]*(x, color), x:[e,f]*(x, color),.......)
     */
    def getOutsiderPixelAlongTheLine(line: List[LineColor]): List[HairPixel] 
    
    def name:String
    
    //return the approximate line ( Poly regression, sma,...), only used for debugging
    def getAproximateLine(): List[Pixel]
    
    
    implicit def lineColorToPixel( lcs: List[LineColor]): List[Pixel]={
      lcs.map{ lc => Pixel(lc.x, lc.c)}
    }
    
    //from boolean isHair{true,false} to prHair{0.0 to 1.0} , 
    implicit def lineColorToHairPixel( lcs: List[LineColor]): List[HairPixel]={
      lcs.map{ lc => HairPixel( lc.x, lc.c, 1.0d)}
    }
  }
  

/*
 * for the gradient calculation ( plugin epfl,http://bigwww.epfl.ch/thevenaz/differentials/ )
 * they transform the discret data (pixels) into a function using 
 * a spline interpolation,could be a better way to fit the "line".
 * Once the line found, we could use the gradient to get the outliers.
 */
class SearchOutliersUsingSplineInterpolation extends SearchOutsiderPixels{
  
  def name = "SearchOutliersUsingSplineInterpolation"
  
  var ourSplineFunction: Option[UnivariateFunction]=None
  var ourSpline:List[LineColor]=List.empty
  val period=10;
  
  private def sma( data:List[LineColor], result: List[LineColor]=List.empty ):List[LineColor]={
      if( data.isEmpty || data.size < period ) result
      else{
        
        //List( 1,2,3,4) => Sum(  (2-1), (3-2), (4-3) )
        
        val firstList = data.take(period).drop(1)
        val secondList = data.take(period-1)
        
        val currentresult=firstList.map(_.c).zip(secondList.map(_.c)).map{ x => Math.abs(x._1 - x._2)}.reduce( _+_ )
        
       // data.take(period).map(_.c).map(Math.abs )./:(4)((first,second)=>first-second)
        sma( data.tail, result :+ LineColor(  data.head.x, currentresult) )
      }
    }
  
  def getOutsiderPixelAlongTheLine(line: List[LineColor]): List[HairPixel] ={
    val ( ourXs, ourCs) =line.unzip{ x_c => (x_c.x, x_c.c)  } 
    ourSplineFunction=Some(splineInterpolator.interpolate( ourXs.map(_.toDouble).toArray , ourCs.map(_.toDouble).toArray).derivative())
    ourSpline= sma((for( x <- line.map(_.x.toDouble))yield{
      ourSplineFunction.map{ fspline =>
        LineColor( x.toInt, (fspline.value( x) * 2).toInt )
      }
    }).flatten)
    List.empty
  }
  
  def getAproximateLine()={
    println("Spline size:"+ourSpline.size)
    println("Spline:"+ourSpline.take(10))
    ourSpline.map{ sp => Pixel(sp.x, sp.c)}
  }
  
  val splineInterpolator=new org.apache.commons.math3.analysis.interpolation.SplineInterpolator()
  
}


  /*
   * higpass filter, https://github.com/aboisvert/high-pass-filter , lower alpha the better
   */
 class SearchParticularPixelUsingHighPass(alpha: Float) extends SearchOutsiderPixels{
    
    def name=s"HighPass($alpha)"
    var aproximateLine: List[LineColor]=List.empty

    
    def getAproximateLine(): List[Pixel]=aproximateLine
    
   def highPassFilter(x: scala.collection.immutable.IndexedSeq[LineColor], alpha: Float) = {
    val y = new Array[LineColor](x.length)
    y(0) = x(0)
    for (i <- 1 until x.length) {
      y(i) = LineColor( x(i).x, (alpha * (y(i-1).c + x(i).c - x(i-1).c)).toInt)
    }
    y
  }
    /*
     * return pixel > 2*stdDeviation over the colorvalue
     */
    def getOutsiderPixelAlongTheLine( pixelOnTheLine: List[LineColor] ) = {
      val onlycolors= pixelOnTheLine.map(_.c)
      val smacolor= highPassFilter( pixelOnTheLine.toIndexedSeq, 0.5f)
      aproximateLine=smacolor.toList
      
      //println(s" current Std: ${stdDeviation}")
      aproximateLine
    }
  }
  
  /*
   * search "particular" pixel using exponential moving average
   * goes from one side of the image to the other than reverse the
   * exploration ( SMA(10) => the first 10 data are empty, this is why we 
   * move in both direction
   */
  class SearchParticularPixelUsingEMA(period: Int) extends SearchOutsiderPixels{
    
    def name=s"EMA($period)"
    var aproximateLine: List[LineColor]=List.empty
    val multiplier = 2.0d/( period +1.0d)
    
    def getAproximateLine(): List[Pixel]=aproximateLine
    
    //first ema == sma, then last(ema)+(2 / (Time periods + 1) )*currentValue
    // {Close - EMA(previous day)} x multiplier + EMA(previous day).
    private def ema( data:List[Int], currentema: Double ,result: List[Int]=List.empty):List[Int]={
      if( data.isEmpty || data.size < period ) result
      else{
        val currentresult=currentema + multiplier * ( data.head - currentema)
        ema( data.tail, currentresult,result :+ currentresult.toInt )
      }
    }
    
    /*
     * return pixel > 2*stdDeviation over the colorvalue
     */
    def getOutsiderPixelAlongTheLine( pixelOnTheLine: List[LineColor] ) = {
      val onlycolors= pixelOnTheLine.map(_.c)
      val smacolor= ema( onlycolors, onlycolors.take(period).reduce(_+_)/period )
      aproximateLine=pixelOnTheLine.map(_.x).drop(period).zip( smacolor).map{ x=> LineColor(x._1, x._2)}
      val xAndDiffSMA=pixelOnTheLine.drop(period).zip(smacolor).map{ A => (A._1, A._1.c - A._2) }
      
      val medianDiffSMA=xAndDiffSMA.map(_._2).sum / xAndDiffSMA.size
      val stdDeviation=Math.sqrt( xAndDiffSMA.map( _._2).map{ x => Math.pow(x-medianDiffSMA,2) }.sum / xAndDiffSMA.size)      
      val resutl=xAndDiffSMA.filter{ x =>  x._2<0 &&   Math.abs(x._2) > 2*stdDeviation}.map(_._1)
      
      println(s" current Std: ${stdDeviation}")
      resutl
    }
  }

  /*
   * search "particular" pixel using simple moving average
   * goes from one side of the image to the other than reverse the
   * exploration ( SMA(10) => the first 10 data are empty, this is why we 
   * move in both direction
   */
  class SearchParticularPixelUsingSMA(period: Int) extends SearchOutsiderPixels{
    
    def name=s"SMA($period)"
    
    var aproximateLine: List[Pixel]=List.empty
    def getAproximateLine(): List[Pixel]=aproximateLine
    
    private def sma( data:List[Int], result: List[Int]=List.empty ):List[Int]={
      if( data.isEmpty || data.size < period ) result
      else{
        val currentresult=data.take(period).reduce(_+_)/period
        sma( data.tail, result :+ currentresult )
      }
    }
    
    /*
     * return pixel > 2*stdDeviation over the colorvalue
     */
    def getOutsiderPixelAlongTheLine( pixelOnTheLine: List[LineColor] ) = {
      val onlycolors= pixelOnTheLine.map(_.c)
      val smacolor= sma( onlycolors)
      aproximateLine=pixelOnTheLine.map(_.x).drop(period).zip( smacolor).map{ x=> LineColor(x._1, x._2)}
      val xAndDiffSMA=pixelOnTheLine.drop(period).zip(smacolor).map{ A => (A._1, A._1.c - A._2) }
      
      val medianDiffSMA=xAndDiffSMA.map(_._2).sum / xAndDiffSMA.size
      val stdDeviation=Math.sqrt( xAndDiffSMA.map( _._2).map{ x => Math.pow(x-medianDiffSMA,2) }.sum / xAndDiffSMA.size)      
      val resutl=xAndDiffSMA.filter{ x =>  x._2<0 &&   Math.abs(x._2) > 2*stdDeviation}.map(_._1)
      
      println(s" current Std: ${stdDeviation}")
      resutl
    }
  }

  
  /*
   * We want to associate to each pixel on the line a value from 0.0 to 1.0, 1.0 => hair pixel,
   * 0.5 => could be a hair pixel, 0.0 => not a hair pixel
   * fuzzyEval( distance: Double, threshold: Double) : Double
   */
  class fuzzySearchOutsidersPixelUsingMultiplePolyRegression( size:Int, polyLevel:Int, 
      threshold: (Double ) => Double   = (stdDeviation:Double)=>(if( stdDeviation < 5 ) 5 else stdDeviation+5), 
      optFuzzyEval: Option[(Double, Double ) => Double] = None   
  ) 
     extends SearchOutsidersPixelUsingMultiplePolyRegression(size, polyLevel, threshold) {
      
     override def name=s"fuzzySearchOutsidersPixelUsingMultiplePolyRegression($size,$polyLevel)"
     
     
     /*
      * distance : beetween the poly line and the point ( line of color)
      * for the moment  >= threshold => 1.0d
      *              otherwise distance / threshold
      */
     def defaultFuzzyEval=(  distance:Double, threshold:Double)=> {
       val response=if( distance <= 0.0d) 0.0d
       else {
         val div=distance / threshold
         if( div > 1.0d ) 1.0d
         else div
       }
       response
     }
    
    
     override def evalPixel( distance: Double, stdDeviation: Double ):Double={    
       optFuzzyEval.getOrElse(defaultFuzzyEval)( distance, threshold( stdDeviation))       
     }
  }
  
  
  /*
   * same idea as SearchParticularPixelUsingPolyRegression with one difference, 
   * with divide the Line, with the idea to have N similar Lines with a width close
   * the Size parameters
   * 
   * threshold: (Double ) => Double)  :  (  StdDeviation) => Threshold , 
   * Note: delta not used for the moment
   */
  class SearchOutsidersPixelUsingMultiplePolyRegression( size:Int, polyLevel:Int, 
      threshold: (Double ) => Double   = (stdDeviation:Double)=>(if( stdDeviation < 5 ) 5 else stdDeviation) ) extends SearchOutsiderPixels {
    
    def name=s"MultiPolyReg($size,$polyLevel)"
    val EMAperiod=25
    val EMAmultiplier = 2.0d/( EMAperiod +1.0d)
    var aproximateLine: List[Pixel]=List.empty
    def getAproximateLine(): List[Pixel]=aproximateLine
    
    
    //1.0 if > threshold otherwise 0.0d
    def evalPixel( distance: Double, stdDeviation: Double ):Double={
      if( distance > threshold(stdDeviation) ){
        1.0d
      }else
        0.0d
    }
    
    //first ema == sma, then last(ema)+(2 / (Time periods + 1) )*currentValue
    // {Close - EMA(previous day)} x multiplier + EMA(previous day).
    private def ema( data:List[Int], currentema: Double ,result: List[Int]=List.empty):List[Int]={
      if( data.isEmpty || data.size < EMAperiod ) result
      else{
        val currentresult=currentema + EMAmultiplier * ( data.head - currentema)
        ema( data.tail, currentresult,result :+ currentresult.toInt )
      }
    }
    
    /*
     * to improve the linearegression we are going to generate a linearegression
     * evaluate the stdDeviation, if the stdDeviation >  X, we remove the N elements with the 
     * largest distance with the line. To repeat until stdDeviation < X
     */
    def improveLinearRegression( currentpixels: List[LineColor] ):PolynomialFunction={
      val polyfitterOverLine= PolynomialCurveFitter.create(polyLevel);
      val obsOverLine = new WeightedObservedPoints();
      currentpixels.foreach{ p =>
        obsOverLine.add( p.x.toDouble, p.c.toDouble )
      }
      val listPoints=obsOverLine.toList
      val ourfunctionOverLine = new PolynomialFunction(polyfitterOverLine.fit(listPoints) )
      val colormean=currentpixels.map{ x=> Math.abs(ourfunctionOverLine.value( x.x) - x.c)  }.reduce(_+_) / currentpixels.size
      val stdDeviation=Math.sqrt( currentpixels.map{ x=> Math.pow(ourfunctionOverLine.value( x.x) - x.c -colormean,2)  }.sum / currentpixels.size)
      
      
      
      if( stdDeviation < 2 || listPoints.size() < 15){
        //println(s"recursive stdDeviation: $stdDeviation, nbrPixelsleft: ${currentpixels.size}")
        ourfunctionOverLine
      }
      else{
        val pixeltoremove=currentpixels.map{ p =>
              (p,Math.abs(ourfunctionOverLine.value( p.x)  - p.c))                       
        }.sortBy(-_._2).take(5)
        improveLinearRegression( currentpixels.filter { x => !pixeltoremove.map(_._1).contains(x) })
      }
    }
    
    /*
     * consume all the line, consuming at each step a dimension of at maximum blocksize
     * 
     * 
     * @param approximateLine  - the line generated using the poly regression. only for debug
     * 
     * @return ( theHairPixels, approximateLineDebug)
     */
    def consumeline( currentpixels: List[LineColor] ,emapixels: List[LineColor], blocksize: Int, result:List[HairPixel] =List.empty, approximateLine: List[LineColor]=List.empty ): (List[HairPixel],List[LineColor])={
      if( currentpixels.isEmpty ) (result, approximateLine)
      else{
        
        
        //better double blocksize at the end of the line using a almost empty line
        val (ourpixels, nextpixels, ourema, nextema) = if( currentpixels.size < 2*blocksize){
          (currentpixels, List.empty[LineColor], emapixels, List.empty[LineColor])
        }else{
          (currentpixels.take(blocksize),currentpixels.drop(blocksize ),emapixels.take(blocksize), emapixels.drop(blocksize)) 
        }
        

        val polyfitterOverLine= PolynomialCurveFitter.create(polyLevel);
        val obsOverLine = new WeightedObservedPoints();
        ourpixels.foreach{ p =>
          obsOverLine.add( p.x.toDouble, p.c.toDouble )
        }
        
//        //attempt to improve poly adding some values
//        val ourTopNMaxColor=ourpixels.sortBy { x => -x.c }.take(10)
//        ourTopNMaxColor.foreach{ p =>
//          obsOverLine.add( p.x.toDouble, p.c.toDouble )
//        }
//        
//        val ourmaxColor=ourpixels.map(_.c).max
//        val ourminX=ourpixels.map(_.x).min
//        val ourmaxX=ourpixels.map(_.x).max
//        for( x <- ourminX until  ourmaxX by 5){
//          obsOverLine.add( x.toDouble, ourmaxColor )
//        }
        
        val ourfunctionOverLine = improveLinearRegression(  ourpixels)
          
          
          //new PolynomialFunction(polyfitterOverLine.fit(obsOverLine.toList) )
        
        val colormean=ourpixels.map{ x=> Math.abs(ourfunctionOverLine.value( x.x) - x.c)  }.reduce(_+_) / ourpixels.size
        val stdDeviation=Math.sqrt( ourpixels.map{ x=> Math.pow(ourfunctionOverLine.value( x.x) - x.c -colormean,2)  }.sum / ourpixels.size)
        
        //does not use f(x), only mean of the pixels color
        
        //val colormean=ourpixels.map{ x=> x.c  }.reduce(_+_) / ourpixels.size
        //val stdDeviation=Math.sqrt( ourpixels.map{ x=> Math.pow( x.c -colormean,2)  }.sum / ourpixels.size)
        
        
        //val medianDiffSMA=ourpixels.map{ x=> Math.abs(ourfunctionOverLine.value(x.x))  }.reduce(_+_) / ourpixels.size
              
       // val resutl=xAndDiffSMA.filter{ x =>  x._2<0 &&   Math.abs(x._2) > 2*stdDeviation}.map(_._1)
        
        
        val numThreshold = threshold( stdDeviation);// 2* (if( stdDeviation < 7 ) 7 else  stdDeviation)
        
        //println(s"threshold: numThreshold, stdDeviation: $stdDeviation")
        
        val xOverLineAndOverMean = ourpixels.map{ lc =>
    
              
              HairPixel( lc.x, lc.c, evalPixel( ourfunctionOverLine.value( lc.x)  -lc.c, numThreshold))
              
             // position.c < ourfunctionOverLine.value( position.x) &&
             // (   ourfunctionOverLine.value( position.x)  -position.c> (numthreshold) )
                       
        }
        
        //xOverLineAndOverMean.foreach { x => println(s"Keep: $x   Line:${ourfunctionOverLine.value( x.x)}  value:${x.c}" ) }
        
        val createnewapproximate= ourpixels.map{ linecolor => LineColor( linecolor.x, ourfunctionOverLine.value(linecolor.x).toInt)}
        val newapproximateline=approximateLine ++ createnewapproximate ++ createnewapproximate.map{xc => LineColor(xc.x, xc.c -numThreshold.toInt) }
        
        consumeline( nextpixels, nextema,blocksize, result ++ xOverLineAndOverMean, newapproximateline )
      }
    }
    
    def getOutsiderPixelAlongTheLine(pixelOnTheLine: List[LineColor] ) = {
      
      val linesize = pixelOnTheLine.size / Math.floor( pixelOnTheLine.size / size) 
      
      //add ema on final graphics to get an idea of his possible use
      val onlycolors= pixelOnTheLine.map(_.c)
      val emaColor=ema( onlycolors, onlycolors.take(EMAperiod).reduce(_+_)/EMAperiod )
      val emapixels=pixelOnTheLine.map(_.x).drop(0).zip( emaColor).map{ x=> LineColor(x._1, x._2)}
      
      val (response, debug)=consumeline( pixelOnTheLine,emapixels, linesize.toInt )
      
      aproximateLine=debug.map{ lc => Pixel(lc.x, lc.c)} ++ pixelOnTheLine.map(_.x).drop(0).zip( emaColor).map{ x=> Pixel(x._1, x._2+ 50)}
      response
    }
  }
  
  /*
   * search "particular" pixel using a polynomial regression
   * does not work very well on a large image
   */
  class SearchParticularPixelUsingPolyRegression extends SearchOutsiderPixels{
    
    def name="PolyReg"
    
    def getAproximateLine(): List[Pixel]=List.empty
    
    def getOutsiderPixelAlongTheLine(pixelOnTheLine: List[LineColor] ) = {
      val polyfitterOverLine= PolynomialCurveFitter.create(8);
      val obsOverLine = new WeightedObservedPoints();
      pixelOnTheLine.foreach{ p =>
        obsOverLine.add( p.x.toDouble, p.c.toDouble )
      }
      val ourfunctionOverLine = new PolynomialFunction(polyfitterOverLine.fit(obsOverLine.toList) )
      
      val meanVariation=pixelOnTheLine.map{ x=> Math.abs(ourfunctionOverLine.value(x.x)-x.c)  }.reduce(_+_) / pixelOnTheLine.size
      println(s"meanVariation:$meanVariation")
      
      val xOverLineAndOverMean = pixelOnTheLine.filter{
          position => 
            position.c < ourfunctionOverLine.value( position.x) &&
            (  ourfunctionOverLine.value( position.x)  - position.c  > meanVariation+10)
                     
      }
      xOverLineAndOverMean
    }
  }