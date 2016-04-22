package algo

import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
import org.apache.commons.math3.fitting.PolynomialCurveFitter
import org.apache.commons.math3.fitting.WeightedObservedPoints

import SearchHairs._
  

 /*
  * define service to identify the outsider pixels, the one who can be part of a hair 
  * for her difference of color in compare with the rest of the pixels along the line
  */
 trait SearchOutsiderPixels{  
    /*
     * line : line as (x, color), x: 0 to image with
     * @return outside pixel as  List( x:[a to b]*( x, color), x:[c,d]*(x, color), x:[e,f]*(x, color),.......)
     */
    def getOutsiderPixelAlongTheLine(line: List[LineColor]): List[LineColor] 
    
    def name:String
    
    //return the approximate line ( Poly regression, sma,...), only used for debugging
    def getAproximateLine(): List[LineColor]
  }
  

  /*
   * higpass filter, https://github.com/aboisvert/high-pass-filter , lower alpha the better
   */
 class SearchParticularPixelUsingHighPass(alpha: Float) extends SearchOutsiderPixels{
    
    def name=s"HighPass($alpha)"
    var aproximateLine: List[LineColor]=List.empty

    
    def getAproximateLine(): List[LineColor]=aproximateLine
    
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
    
    def getAproximateLine(): List[LineColor]=aproximateLine
    
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
    
    var aproximateLine: List[LineColor]=List.empty
    def getAproximateLine(): List[LineColor]=aproximateLine
    
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
   * same idea as SearchParticularPixelUsingPolyRegression with one difference, 
   * with divide the Line, with the idea to have N similar Lines with a width close
   * the Size parameters
   */
  class SearchOutsidersPixelUsingMultiplePolyRegression( size:Int, polyLevel:Int) extends SearchOutsiderPixels {
    def name=s"MultiPolyReg($size,$polyLevel)"
    
    var aproximateLine: List[LineColor]=List.empty
    def getAproximateLine(): List[LineColor]=aproximateLine
    
    /*
     * consume all the line, consuming at each step a dimension of at maximum blocksize
     * 
     * 
     * @param approximateLine  - the line generated using the poly regression. only for debug
     */
    def consumeline( currentpixels: List[LineColor] , blocksize: Int, result:List[LineColor] =List.empty, approximateLine: List[LineColor]=List.empty ): (List[LineColor],List[LineColor])={
      if( currentpixels.isEmpty ) (result, approximateLine)
      else{
        
        
        //better double blocksize at the end of the line using a almost empty line
        val (ourpixels, nextpixels) = if( currentpixels.size < 2*blocksize){
          (currentpixels, List.empty[LineColor])
        }else{
          (currentpixels.take(blocksize),currentpixels.drop(blocksize )) 
        }
        
        
        val polyfitterOverLine= PolynomialCurveFitter.create(polyLevel);
        val obsOverLine = new WeightedObservedPoints();
        ourpixels.foreach{ p =>
          obsOverLine.add( p.x.toDouble, p.c.toDouble )
        }
        val ourfunctionOverLine = new PolynomialFunction(polyfitterOverLine.fit(obsOverLine.toList) )
        
        val colormean=ourpixels.map{ x=> Math.abs(ourfunctionOverLine.value( x.x) - x.c)  }.reduce(_+_) / ourpixels.size
        val stdDeviation=Math.sqrt( ourpixels.map{ x=> Math.pow(ourfunctionOverLine.value( x.x) - x.c -colormean,2)  }.sum / ourpixels.size)
        
        
        println(s"colormean:$colormean, stdDeviation:$stdDeviation")
        
        //val medianDiffSMA=ourpixels.map{ x=> Math.abs(ourfunctionOverLine.value(x.x))  }.reduce(_+_) / ourpixels.size
              
       // val resutl=xAndDiffSMA.filter{ x =>  x._2<0 &&   Math.abs(x._2) > 2*stdDeviation}.map(_._1)
        
        
        val threshold = stdDeviation + 5
        
        val xOverLineAndOverMean = ourpixels.filter{
            position => 
              position.c < ourfunctionOverLine.value( position.x) &&
              (  ourfunctionOverLine.value( position.x)  - position.c  > (threshold) )
                       
        }
        
        xOverLineAndOverMean.foreach { x => println(s"Keep: $x   Line:${ourfunctionOverLine.value( x.x)}  value:${x.c}" ) }
        
        val createnewapproximate= ourpixels.map{ linecolor => LineColor( linecolor.x, ourfunctionOverLine.value(linecolor.x).toInt)}
        val newapproximateline=approximateLine ++ createnewapproximate ++ createnewapproximate.map{xc => LineColor(xc.x, xc.c -threshold.toInt) }
        
        consumeline( nextpixels, blocksize, result ++ xOverLineAndOverMean, newapproximateline )
      }
    }
    
    def getOutsiderPixelAlongTheLine(pixelOnTheLine: List[LineColor] ) = {
      
      val linesize = pixelOnTheLine.size / Math.floor( pixelOnTheLine.size / size) 
      println(s"linesize:$linesize")
      val (response, debug)=consumeline( pixelOnTheLine, linesize.toInt )  
      aproximateLine=debug;
      response
    }
  }
  
  /*
   * search "particular" pixel using a polynomial regression
   * does not work very well on a large image
   */
  class SearchParticularPixelUsingPolyRegression extends SearchOutsiderPixels{
    
    def name="PolyReg"
    
    def getAproximateLine(): List[LineColor]=List.empty
    
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