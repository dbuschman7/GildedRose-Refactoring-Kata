package com.gildedrose

import org.scalatest.FunSuite
import java.util.Arrays

class OldVersusNewComparisonTest extends FunSuite {

  test("Try some example for each product type") {

    //    val name = "Aged Brie"
    //    val name = "Backstage passes to a TAFKAL80ETC concert"
    //    val name = "Sulfuras, Hand of Ragnaros"
    val name = "Common Item"
    //    val name = "Conjured Item"
    val sellIn = 1
    val quality = 10

    val diffs = (-5 to 55).flatMap { sellIn ⇒

      val first = {
        val item = new Item(name, sellIn, quality)
        val temp = new GildedRose(Array(item))
        temp.updateQuality()
        ImmutableItem.fromItem(temp.items.head) // need to convert to case class for true comparisons
      }

      val second = GildedRoseFunctional.updateQuality(Seq(ImmutableItem(name, sellIn, quality))).head
      if (first != second) {
        println(f"Original   - ${name}%30s  ${sellIn}%3d ${quality}%3d")
        println(f"Old        - ${first.name}%30s  ${first.sellIn}%3d ${first.quality}%3d")
        println(f"Functional - ${second.name}%30s  ${second.sellIn}%3d ${second.quality}%3d")
        println()
        Some(1)
      } else None
    }

    if (diffs.size > 0) fail("Diffs found")
  }

  ignore("Compare both algothrithms ") {

    case class TestRun(original: ImmutableItem, left: ImmutableItem, right: ImmutableItem) {
      val (good: Boolean, msg: Option[String]) = {
        ((left.name == right.name), (left.sellIn == right.sellIn), (left.quality == right.quality)) match {
          case (true, true, true) ⇒ (true, None) // good match
          case (false, _, _)      ⇒ (false, Option(s"Name mismatch for ${original} - Left(${left.name})  Right(${right.name})"))
          case (_, false, _)      ⇒ (false, Option(s"SellIn mismatch for ${original} - Left(${left.sellIn})  Right(${right.sellIn})"))
          case (_, _, false)      ⇒ (false, Option(s"Quality mismatch for ${original} - Left(${left.quality})  Right(${right.quality})"))
        }
      }
    }

    // setup the tests
    val results: Seq[TestRun] = for {
      name ← Seq("Aged Brie", "Backstage passes to a TAFKAL80ETC concert", "Sulfuras, Hand of Ragnaros", "Common Item") // "Conjured Item"
      sellIn ← -5 to 50
      quality ← 0 to 50
    } yield {
      val original = ImmutableItem(name, sellIn, quality)

      // old - mutable OO 
      val first = {
        val item = new Item(name, sellIn, quality)
        val temp = new GildedRose(Array(item))
        temp.updateQuality()
        ImmutableItem.fromItem(temp.items.head) // need to convert to case class for true comparisons
      }

      // new  - immutable functional  
      val second = {
        val item = ImmutableItem(name, sellIn, quality)
        GildedRoseFunctional.updateQuality(Seq(item)).head
      }

      TestRun(original, first, second)
    }

    // summarize results 
    val resultsToDisplay = 10
    val successes = results.filter(_.good).size
    val diffCount = results.filterNot(_.good).size
    val productsWithDiffs = results.filterNot(_.good).map(_.original.name).toSet
    val productsWithSuccess = results.filter(_.good).map(_.original.name).toSet

    val errors = results
      .filterNot(_.good)
      .groupBy(_.original.name)
      .map { case (name, msgs) ⇒ (name, msgs.take(resultsToDisplay)) }

    println("*")
    println("* Products in Success")
    println("****************************")
    println("")
    errors.map {
      case (name, msgs) ⇒
        msgs.map(_.right).map(println)
        println("")
        println("****************************")
    }
    println("")
    println("****************************")
    println("* Products in Error")
    println("****************************")
    println("")
    errors.map {
      case (name, msgs) ⇒
        msgs.flatMap(_.msg).map(println)
        println("")
        println("****************************")
    }
    println("")
    println("****************************")
    println(s"Total successes     - ${successes}")
    println(s"Total diffs         - ${diffCount}")
    println(s"Products with Diffs - ${productsWithDiffs.mkString("[", ", ", "]")}")
    println("****************************")
    println("")

    if (diffCount > 0) fail(s"Found ${diffCount} diffs")
  }
}