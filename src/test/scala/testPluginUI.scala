import org.scalatest.WordSpec
import ij.IJ
import algo.SearchHairs._
import algo._
import ij.plugin.ContrastEnhancer
import ij.plugin.filter.RankFilters

/*
 * experiment with imagej plugin to add ui to manipulate hairs
 * http://rsb.info.nih.gov/ij/plugins/ , http://rsb.info.nih.gov/ij/plugins/panel-window.html
 **/
object testPluginUI {
  def main(args: Array[String]): Unit = {

        val img=IJ.openImage("./images/hairPixels_001.tif") 
        val imgproc=img.getProcessor
        img.show()
  }
}