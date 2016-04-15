package algo

import org.apache.commons.math3.analysis.polynomials.PolynomialFunction
import org.apache.commons.math3.fitting.PolynomialCurveFitter
import org.apache.commons.math3.fitting.WeightedObservedPoints

import SearchHairs._
  trait SearchParticularPixel{
    def getPixelWithColorFarFromPolyLine(pixelOnTheLine: List[LineColor]):List[LineColor] 
  }
  
  
  /*
   * search "particular" pixel using simple moving average
   * goes from one side of the image to the other than reverse the
   * exploration ( SMA(10) => the first 10 data are empty, this is why we 
   * move in both direction
   */
  class SearchParticularPixelUsingSMA(period: Int) extends SearchParticularPixel{
    
    private def sma( data:List[Int], result: List[Int]=List.empty ):List[Int]={
      if( data.isEmpty || data.size < period ) result
      else{
        val currentresult=data.take(period).reduce(_+_)/period
        sma( data.tail, result :+ currentresult )
      }
    }
    
    /*
     * return pixel > stdDeviation over the colorvalue
     */
    def getPixelWithColorFarFromPolyLine( pixelOnTheLine: List[LineColor] ) ={
      val onlycolors= pixelOnTheLine.map(_.c)
      val smacolor= sma( onlycolors)
      
      val xAndDiffSMA=pixelOnTheLine.drop(period).zip(smacolor).map{ A => (A._1, A._1.c - A._2) }
      
      val medianDiffSMA=xAndDiffSMA.map(_._2).sum / xAndDiffSMA.size
      val stdDeviation=Math.sqrt( xAndDiffSMA.map( _._2).map{ x => Math.pow(x-medianDiffSMA,2) }.sum / xAndDiffSMA.size)      
      xAndDiffSMA.filter{ x => Math.abs(x._2) > stdDeviation}.map(_._1)
    }
  }

  /*
   * search "particular" pixel using a polynomial regression
   * does not work very well on a large image
   */
  class SearchParticularPixelUsingPolyRegression extends SearchParticularPixel{
    
    def getPixelWithColorFarFromPolyLine(pixelOnTheLine: List[LineColor] )={
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