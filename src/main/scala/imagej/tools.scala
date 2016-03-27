package imagej

import ij.process.BinaryProcessor
import ij.process.ByteProcessor
import ij.ImagePlus
import ij.plugin.filter.RankFilters
import ij.gui.NewImage

object tools {
  
   implicit class binaryimage( ourcolor:Array[Int]){
      def isWhite() = { 
      	 if( ourcolor(0)  ==255) true;
      	 else false;
      }
   }
   
   implicit class addFuncToImagePlus( img: ImagePlus){
     def copyToNewImg(newtitle: String)={
        new ImagePlus( newtitle, img.getProcessor.createImage())
     }
   }
   
  
   /*
    * img - ImagePlus
    * scale  - 
    * newtitle - optional new title, otherwise use actual title
    * @return a 8 bits binary image
    */
   def  createBinary( img: ImagePlus, scale:Boolean, newtitle: Option[String]=None):ImagePlus = { 
        val bproc = new BinaryProcessor(new ByteProcessor(img.getImage())); 
        bproc.autoThreshold();         
        val title=newtitle.getOrElse(img.getTitle)
        new ImagePlus(title, bproc); 
    } 
   
   
   def applyMaximum( img: ImagePlus, value: Int )={
     val MAXFILTER=ij.plugin.filter.RankFilters.MAX
     val imgproce=img.getProcessor
  
     val ourfilter=new RankFilters();
     ourfilter.rank(imgproce, value, MAXFILTER)                 
   }
   
   
   def createBlankImage( title: String, model: ImagePlus)={
     NewImage.createByteImage( title, model.getWidth, model.getHeight, model.getSlice, 0)
   }
   

}