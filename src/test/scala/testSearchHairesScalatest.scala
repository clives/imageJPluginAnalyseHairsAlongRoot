import org.scalatest.WordSpec
import ij.IJ
import algo.SearchHairs._

/*
 * 
 **/
class testSearchHairesScalatest extends WordSpec {

"searchHaires" when {
    "use delta from 10 to 16" should {
      "on picutre" in {
        val img=IJ.openImage("/home/sosthene/Downloads/crop_wt_me10_2.tif")
        
        val results=hairsCount(img,10 to 16 by 2)
        
        assert(  results.filter( _._1 == 10).map(_._2).head === 8) 
        assert(  results.filter( _._1 == 12).map(_._2).head === 8)
        
//        assert(  hairsCount(img,10) === 8)
//        assert(  hairsCount(img,12) === 8)       
//        assert(  hairsCount(img,14) === 6)         
//        assert(  hairsCount(img,16) === 5)
      }
    }
  }
}