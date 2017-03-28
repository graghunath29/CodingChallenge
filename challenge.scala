import scala.io.Source
import play.api.libs.json._
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import java.io._

object codeChallenge {
	
	val matchedFile = new PrintWriter(new File("results.txt" ))
	val noMatchesFile = new PrintWriter(new File("nomatches.txt"))
		

	def main(args: Array[String]) {
		
		//Read lines from prducts and listings files
		val products = Source.fromFile("products.txt").getLines().map(Json.parse).toList
		val listings = Source.fromFile("listings.txt").getLines().map(Json.parse).toList
		
		//Create a map of manufacturer -> (family,model, etc)
		val manufModelMap = organizeProducts(products)
		var outList = ArrayBuffer[(String, String)]()	//Producnt Name, Listing pairs	

		listings.foreach {listing =>

			val title = (listing \ "title").asOpt[String].getOrElse("")
			val listman = (listing \ "manufacturer").asOpt[String].getOrElse("").toLowerCase
			
			if (manufModelMap contains listman.split(" ")(0)) {
				//match title with a product by calling matchprods
				val prod = matchprods(manufModelMap(listman.split(" ")(0)), title)
				if(prod.pn != "") {  
					outList += ((prod.pn, listing.toString))
				}
				
			}
			else noMatchesFile.write((listing \ "manufacturer").asOpt[String].getOrElse("") + " *** Manufacturer not found\n")
		}
		//Generate a map product_name -> list of listings
		val outMap = outList.groupBy(_._1)
		writeResultToFile(matchedFile, outMap)

		matchedFile.close
		noMatchesFile.close
		
	}

	//Iterates through product name and associated list of listings and outputs lines to the results file
	def writeResultToFile(pw: PrintWriter, outMap: scala.collection.immutable.Map[String,ArrayBuffer[(String, String)]]) {
		for ((k,v) <- outMap) {
			matchedFile.write("{\"product_name\":\"" + k +"\",\"listings\":[")
			var listlist = new String("")
			for(l <- v) {
				listlist += l._2 + ","
			}
			matchedFile.write(listlist.dropRight(1))
			matchedFile.write("]}\n")
		}
	}

	case class Product(	val pn: String,
					val manuf: String,
					val fam: String,
					val mod: String,
					val ann_date: String) 

	//Converts list of products to map of manufacturer to list of products of that manufacturer 
	def organizeProducts(products: List[JsValue]) = {
		var manufModelMap = scala.collection.mutable.Map[String,ArrayBuffer[Product]]()
		products.foreach{product => 
			val man = (product \ "manufacturer").asOpt[String].getOrElse("").toLowerCase
			val model = (product \ "model").asOpt[String].getOrElse("").toLowerCase
			val family = (product \ "family").asOpt[String].getOrElse("").toLowerCase
			val ann_date = (product \ "announced_date").asOpt[String].getOrElse("")
			val prod_name = (product \ "product_name").asOpt[String].getOrElse("")
			
		
			if (!manufModelMap.contains(man)) manufModelMap(man) = new ArrayBuffer[Product]
				var prodList = manufModelMap(man)
				val x = new Product(prod_name, man, family, model, ann_date) 
				prodList += x
				//Longest model length first in prodList. e.g NX1000, NX100, NX10, so that when we 
				//compare the longest one matches first if it exists, else NX10 will match with 
				//string NX1000 which is wrong
				manufModelMap(man) = prodList.sortWith(_.mod.length > _.mod.length)
		}
		manufModelMap
	}

	//Given a title and list of products, returns Product associated with title, else empty Product
	def matchprods(prodList: ArrayBuffer[Product], title: String) = {
		var matches = List[Product]()
		var cleanedTitle = title.replaceAll ("[-\\s]", "") 	//Get rid of spaces and hyphens
		var x = 0

		cleanedTitle = cleanedTitle.toLowerCase
		

		prodList.foreach { prod =>
			if(cleanedTitle.contains(prod.mod.toLowerCase.replaceAll("[-\\s]", ""))) {
				matches = prod :: matches
			}
		
		}

		
		var p : Product = new Product("","","","","") 
		if (matches.length == 1)
			p = matches(0)
		else if (matches.length >1) { 
			//println( "Multiple Matches : " + matches.length)
			val revMatches = matches.reverse
			if(revMatches(0).mod.length > revMatches(1).mod.length)
				p = revMatches(0)
			}
		else if (matches.length == 0) 
			noMatchesFile.write(title+"\n")
		p
	}
}