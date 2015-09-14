package org.koiroha.ingress.xmpblast

import java.net.URL
import java.text.DecimalFormat
import java.util.ResourceBundle
import javafx.application.Application
import javafx.event.EventHandler
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.Scene
import javafx.scene.control.{ComboBox, TextField}
import javafx.scene.web.{WebErrorEvent, WebEvent, WebView}
import javafx.stage.Stage

import netscape.javascript.JSObject
import org.koiroha.ingress.xmpblast.XmpBlast.{LatLng, XY}

import scala.beans.BeanProperty
import scala.io.Source
import scala.math.{Pi, cos, sin, sqrt}
import scala.util.Try

/**
 * Most effective position to blast Xmp Burster.
*/
class XmpBlast extends Application with Initializable {
	@FXML @BeanProperty
	var latitude:TextField = null
	@FXML @BeanProperty
	var longitude:TextField = null
	@FXML @BeanProperty
	var resonatorN:TextField = null
	@FXML @BeanProperty
	var resonatorNE:TextField = null
	@FXML @BeanProperty
	var resonatorE:TextField = null
	@FXML @BeanProperty
	var resonatorSE:TextField = null
	@FXML @BeanProperty
	var resonatorS:TextField = null
	@FXML @BeanProperty
	var resonatorSW:TextField = null
	@FXML @BeanProperty
	var resonatorW:TextField = null
	@FXML @BeanProperty
	var resonatorNW:TextField = null
	@FXML @BeanProperty
	var level:ComboBox[String] = null
	@FXML @BeanProperty
	var map:WebView = null

	private[this] var resonators:Array[TextField] = null

	def start(primaryStage: Stage) {
		primaryStage.setTitle("Effective Xmp Blast Position")
		val loader = new FXMLLoader(getClass.getResource("/main.fxml"))
		// loader.setRoot(this)
		loader.setController(this)
		val scene: Scene = new Scene(loader.load())
		primaryStage.setScene(scene)
		primaryStage.setResizable(false)
		primaryStage.show()
	}

	@FXML
	override def initialize(location: URL, resources: ResourceBundle): Unit = {
		resonators = Array(
			resonatorN, resonatorNE, resonatorE, resonatorSE,
			resonatorS, resonatorSW, resonatorW, resonatorNW)
		map.getEngine.setOnError(new EventHandler[WebErrorEvent] {
			override def handle(event: WebErrorEvent): Unit = {
				System.err.println(event.toString)
			}
		})
		map.getEngine.setOnAlert(new EventHandler[WebEvent[String]] {
			override def handle(event: WebEvent[String]): Unit = {
				System.err.println(event.toString)
			}
		})

		val content = Source.fromInputStream(getClass.getResourceAsStream("/map.html"), "UTF-8").getLines().mkString("\n")
		map.getEngine.loadContent(content)
		map.getEngine.executeScript("window").asInstanceOf[JSObject].setMember("java", new Callback())

		level.getItems.addAll((1 to 8).map{ _.toString }:_*)
		level.getSelectionModel.selectLast()
	}

	@FXML
	protected def updateGoogleMap(): Unit = {
		try {
			val lat = latitude.getText.toDouble
			val lng = longitude.getText.toDouble
			setLocation(lat, lng)
			map.getEngine.executeScript(s"move($lat,$lng);")
			movePortal(lat, lng)
		} catch {
			case ex:NumberFormatException => ex.printStackTrace()
		}
	}

	@FXML
	protected def resetMapCenter():Unit = {
		val jsobj = map.getEngine.executeScript("map.getCenter();").asInstanceOf[JSObject]
		val lat = jsobj.call("lat").toString.toDouble
		val lng = jsobj.call("lng").toString.toDouble
		setLocation(lat, lng)
		movePortal(lat, lng)
	}

	private[this] def setLocation(lat:Double, lng:Double):Unit = {
		val nf = new DecimalFormat(".000000")
		latitude.setText(nf.format(lat))
		longitude.setText(nf.format(lng))
	}

	private[this] def movePortal(lat:Double, lng:Double):Unit = {
		map.getEngine.executeScript(s"set_portal($lat,$lng);")
		val res = resonators.zipWithIndex.map{ case (r, i) =>
			val d = Try{ r.getText.toInt }.toOption.getOrElse(-1)
			if(d < 0)  LatLng(Double.NaN, Double.NaN)
			else LatLng(lat, lng).resolve(XY(0, d).rotate(- 2 * math.Pi / 8 * i))
		}
		res.zipWithIndex.foreach{ case (ll, i) =>
			val xmp = XmpBlast.XmpPower(level.getSelectionModel.getSelectedIndex)
			val maxDamage = xmp.map{ _._2 }.max // * res.count(!_.lat.isNaN)
			val blastRange = xmp.zipWithIndex.map{ case (x, j) =>
				val lowerLayer = xmp.drop(j+1).headOption.map{ _._2 }.getOrElse(0)
				x._1 -> (x._2 - lowerLayer) / maxDamage.toDouble
			}.map{ case (r, x) => s"$r,$x" }.mkString("[", "],[", "]")
			map.getEngine.executeScript(s"set_resonator($i,${ll.lat},${ll.lng},${!ll.lat.isNaN},[$blastRange]);")
		}
	}

	class Callback {
		def getLocation:Array[Double] = Array(latitude.getText.toDouble, longitude.getText.toDouble)
		def setLocation(lat:Double, lng:Double):Unit = XmpBlast.this.setLocation(lat, lng)
		def resetMapCenter():Unit = XmpBlast.this.resetMapCenter()
		def error(o:AnyRef):Unit = System.err.println(o)
	}

}
object XmpBlast {
	def main(args:Array[String]) = Application.launch(classOf[XmpBlast], args:_*)

  case class XY(x:Double, y:Double){
    def distance(p:XY):Double = sqrt((x-p.x)*(x-p.x)+(y-p.y)*(y-p.y))
    def rotate(theta:Double):XY = XY(x*cos(theta)-y*sin(theta), x*sin(theta)+y*cos(theta))
	  def x8brust(p:XY):Double = (distance(p) match {
		  case r if r <= 32 => 2250
		  case r if r <= 65 => 1800
		  case r if r <= 98 => 1350
		  case r if r <= 131 => 900
		  case r if r <= 164 => 450
		  case _ => 0
	  }) / 2250.0
	  def blast(xmp:Array[(Int,Int)], resonators:Array[XY]):Double = resonators.map { p =>
		  val d = distance(p)
		  xmp.find {_._1 <= d}.map {_._2}.getOrElse(0)
	  }.sum
  }

	case class LatLng(lat:Double, lng:Double) {
		def resolve(p:XY):LatLng = {
			val r = 6378150.0    // radius of the earth [m]
			val l = 2 * Pi * r   // periphery of the earth [m]
			val d = 360 / l      // degree of unit meter
			val lat = this.lat + p.y * d
			val lng = this.lng + p.x * 360 / (r * cos(lat / 180.0 * Pi) * 2 * Pi)
			LatLng(lat, lng)
		}
	}

	case class Cell(ll0:LatLng, ll1:LatLng, damage:Double)

	def getDamage(center:LatLng, resonators:Array[XY], xmp:Array[(Int,Int)]):Seq[Cell] = {
		val region = xmp.map{ _._1 }.max + 40
		val step = 1
		val maxDamage = xmp.map{ _._2 }.max
		val damages = for(y <- -region to region by step; x <- -region to region by step) yield {
			val p = XY(x, y)
			p -> p.blast(xmp, resonators)
		}
		val actualMaxDamage = damages.map{ _._2 }.max
		damages.map{ case (p, d) =>
			val ll = center.resolve(p)
			val ll0 = ll.resolve(XY(-step/2, -step/2))
			val ll1 = ll.resolve(XY(step/2, step/2))
			Cell(ll0, ll1, d / actualMaxDamage)
		}.filter{ _.damage > 0.0 }
	}

	/*
	/**
	 *
	 * @param center latitude/longitude of target portal.
	 * @param resonators resonator distance from portal location.
	 */
	case class Position(center:XY, resonators:Array[XY]) {

		def writeAsKml(out:PrintWriter):Unit = {
			val region = 45
			val step = 2
			val damages = for(y <- -region to region by step; x <- -region to region by step) yield {
				XY(x, y) -> resonators.map {_.x8brust(XY(x, y))}.sum / resonators.length
			}

			val max = damages.map{ _._2 }.max
			val min = damages.map{ _._2 }.min

			def color(d:Double):String = {
				val rate =  (d - min)/(max - min)
				val threshold = 0.80
				if(rate >= threshold)  f"${((rate-threshold)/(1-threshold)*255).toInt}%02x0000ff"
				else f"${(rate*255).toInt}%02xff0000"
			}

			val style = damages.map { _._2 }.distinct.map{ damage =>
				val id = (damage*255).toInt
				s"""  <Style id="c$id">
         |    <LineStyle><width>0</width></LineStyle>
         |    <PolyStyle><color>${color(damage)}</color></PolyStyle>
        |  </Style>""".stripMargin('|')
			}.mkString("\n")

			val mesh = damages.map { case (XY(x, y), damage) =>
				val p1 = XY(x-1, y-1).toLatLng(center)
				val p2 = XY(x+1, y+1).toLatLng(center)
				f"""  <Placemark>
		   |    <description>
		   |      ${if(x<0) "西" else "東"}%sへ ${abs(x)}%.1f[m],
      |      ${if(y<0) "南" else "北"}%sへ ${abs(y)}%.1f[m]:
     |      damage rate ${damage*100}%.1f%%</description>
			                                                                                                                               |    <styleUrl>#c${(damage*255).toInt}</styleUrl>
				                                                                                                                                                                       |    <Polygon>
				                                                                                                                                                                       |      <outerBoundaryIs>
				                                                                                                                                                                       |        <LinearRing>
				                                                                                                                                                                       |          <coordinates>
				                                                                                                                                                                       |            ${p1.y},${p1.x},$damage
					  |            ${p1.y},${p2.x},$damage
					  |            ${p2.y},${p2.x},$damage
					  |            ${p2.y},${p1.x},$damage
					  |          </coordinates>
					  |        </LinearRing>
					  |      </outerBoundaryIs>
					  |    </Polygon>
					  |  </Placemark>""".stripMargin('|')
			}.mkString("\n")

			val points = (XY(0,0) +: resonators).map{ p =>
				val ll = p.toLatLng(center)
				s"""  <Placemark>
				   |    <Point>
				   |      <coordinates>${ll.y},${ll.x},0</coordinates>
				                                        |    </Point>
				                                        |  </Placemark>""".stripMargin('|')
			}.mkString("\n")

			print(s"""<?xml version="1.0" encoding="UTF-8"?>
	<kml xmlns="http://www.opengis.net/kml/2.2">
	<Folder>
	  <name>X8 Burster</name>
	$style
	$points
	$mesh
	</Folder>
	</kml>
	""")
		}
	}

		/**
		 */
		val resonators = (0 until 8).map{ i => XY(0, 40).rotate(2*Pi*i/8) }
	*/

  val XmpPower = Array(
	  Array(  7 ->  125, 15 ->  100, 23 ->   75,  31 ->  50,  39 ->  25 ),
		Array(  8 ->  250, 17 ->  200, 26 ->  150,  35 -> 100,  44 ->  50 ),
		Array( 10 ->  417, 21 ->  334, 32 ->  251,  43 -> 168,  54 ->  85 ),
		Array( 13 ->  750, 27 ->  600, 41 ->  450,  55 -> 300,  69 -> 150 ),
		Array( 17 -> 1000, 35 ->  800, 53 ->  600,  71 -> 400,  89 -> 200 ),
		Array( 21 -> 1250, 43 -> 1000, 65 ->  750,  87 -> 500, 109 -> 250 ),
		Array( 26 -> 1500, 53 -> 1200, 80 ->  900, 107 -> 600, 134 -> 300 ),
		Array( 32 -> 2250, 65 -> 1800, 98 -> 1350, 131 -> 900, 164 -> 450 )
	)

}
