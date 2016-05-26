package tools

import com.itextpdf.text.Rectangle
import com.itextpdf.text.Document
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import com.itextpdf.text.pdf.PdfWriter
import java.io.File
import com.itextpdf.text.pdf.BaseFont
import java.awt.Color
import com.itextpdf.text.Chunk
import com.itextpdf.text.Font
import com.itextpdf.text.Element
import java.awt.geom.Rectangle2D
//import com.itextpdf.text.pdf.DefaultFontMapper
import com.itextpdf.text.pdf.PdfContentByte
import ij.ImagePlus
import ij.IJ
import com.itextpdf.text.Image;
/*
 * basique opeation for the creation of pdf
 */
object PdfDocument {

  val withPdf=500;
    val heightPdf=600;
    val decalagetexte =0;
  
  case class Paragraph(text:String,font: com.itextpdf.text.Font=DEFAULT_FONT );
  
  implicit def toParagraph(paragraph:Paragraph):com.itextpdf.text.Paragraph={
    paragraph.font.setStyle( com.itextpdf.text.Font.NORMAL );
    new com.itextpdf.text.Paragraph(paragraph.text )
  }
  
  val msyh = BaseFont.createFont(BaseFont.HELVETICA,
          BaseFont.WINANSI, BaseFont.NOT_EMBEDDED);

  val DEFAULT_FONT = new com.itextpdf.text.Font(msyh,12,Font.BOLD);

  def initPDF(fileName: String): (Document, PdfWriter) = {
    val documentAssociePdf = new Document();
    val out = new BufferedOutputStream(new FileOutputStream(new File(fileName)));    
    val writerPdf = PdfWriter.getInstance(documentAssociePdf, out);
    documentAssociePdf.open();    
    (documentAssociePdf, writerPdf)
  }
}

class PdfDocument(fileName:String) {
    import PdfDocument._
	val (ourDocument, writerPdf)=initPDF(fileName)

	def addParagraph(text: String)={
    	ourDocument.add(Paragraph(text) )
    }
    
    //direct acces to document, for testing purpose ( griffon,..)
    def getDocument()={
      ourDocument
    }
    
    def close()={
      ourDocument.close();
    }
    
    def newPage()={
      ourDocument.newPage();
    }
    
   
    
    /*
     * want to add a drawChart using ourchart, so would be possible to use it from griffon
     * 
     * tried with griffon ( candle_08..., works)
     * 
     * issue: the position of the graph start from the left bellow
     * not on the current position.
     * 
     * 
     * 09.08.2014:
     * was using jfreechart, want to try to use combineddomain xyplot
     * so I can add latter indicateur,...
     */
    def drawChart( imagej: ImagePlus, width: Int=500, height: Int=270)={
       val cb=writerPdf.getDirectContent();
        //decalagetexte pour espace texte dessous
        val tp=cb.createTemplate( width, height+decalagetexte);
        val g2=tp.createGraphics( width, height+decalagetexte);
        val r2D=new Rectangle2D.Double( 0,0, width, height);
        
        //val chartPanel = new ChartPanel(new JFreeChart("CURRENCY",
        //    JFreeChart.DEFAULT_TITLE_FONT, plot, true));     
        //chartPanel.setPreferredSize(new java.awt.Dimension(width, height));      
        val bf = BaseFont.createFont(BaseFont.HELVETICA, BaseFont.CP1252, BaseFont.NOT_EMBEDDED);
        import java.awt.image.DataBufferByte
        
     //   val pixels = imagej.getBufferedImage.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()
       
     //   ourDocument.add( Image.getInstance( pixels))
        
        cb.beginText();
        
        val departy=decalagetexte-15;
        val increment=15;

        val k=imagej.getBufferedImage.createGraphics();
        g2.drawImage(imagej.getBufferedImage, 0, 0, width, height, null);
       // k.draw(arg0)
       // k.draw((g2,r2D, new java.awt.geom.Point2D.Float(0.f,decalagetexte),new ChartRenderingInfo );
        
       // plot.draw(g2,r2D, new java.awt.geom.Point2D.Float(0.f,decalagetexte),new PlotRenderingInfo(),recup );
        g2.dispose();

     //    cb.addTemplate( tp,0,0);  
         cb.endText()
         
         
         ourDocument.add( Image.getInstance( tp))
    }
    
    

}