package algo

import ij.ImagePlus


/*
 * use reference image to get an idea of the size/pixel.
 * Main idea: the circle is the best way to "fill" a square with a random orientation ( 0° to 90° )
 * take a random point (centered), get all the pixel on a cercle with diameter D
 * if there is no black pixel, the variation have to be smooth otherwise we should have some "higher" color
 * if that's the case, we move the circle. once the circle does not contains those "higher" color
 * we raise the size until touche again, then move it, then raise again... once moving does not avoid to touch the higher
 * color we have our max size
 */
object EvaluteScale{
  
  /*
   * @return scale Double
   */
  def getScale( refimage: ImagePlus ):Double={
    Double.NaN
  }
  
  case class circleColor( x: Int, y: Int, color: Int)
  case class Position( x:Int, y:Int){
    def move( dx:Int, dy:Int)={
      copy( x=dx+x, y=dy+y)
    }
  }
  
  def consumePixelOverCircle( img: ImagePlus, diameter: Int, position: Position ):List[circleColor]={
    val radius = diameter / 2;
    val r2= radius * radius;
    val imgproc= img.getProcessor
    
    //could improving using symetry ( 1 to x ... )
    val allcirclepixels=(for( x <- -radius to radius)yield{
      val y= (Math.sqrt(r2 - x*x) + 0.5).toInt
      val newposition1 = position.move( x,y)
      val newposition2 = position.move( x,-y)
      
      val color1 = imgproc.getPixel(newposition1.x, newposition1.y)
      val color2 = imgproc.getPixel(newposition2.x, newposition2.y)
      
      List( circleColor(newposition1.x, newposition1.y, color1), circleColor(newposition2.x, newposition2.y, color2))
    }).flatten.toList
   allcirclepixels
  }
  
}