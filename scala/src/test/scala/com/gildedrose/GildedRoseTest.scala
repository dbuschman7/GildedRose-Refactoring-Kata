package com.gildedrose

import org.scalatest._

class GildedRoseTest extends FunSuite with Matchers {

  import GildedRoseFunctional._


  test("decrementSellIn") {
    decrementSellIn(ImmutableItem("name", 5, 1)) should be(ImmutableItem("name", 4, 1))
    decrementSellIn(ImmutableItem("name", 1, 1)) should be(ImmutableItem("name", 0, 1))
    decrementSellIn(ImmutableItem("name", -1, 1)) should be(ImmutableItem("name", -2, 1))
  }

  test("constrainAge0to50") {
    constrainQuality0to50(ImmutableItem("name", 1, -1)) should be(ImmutableItem("name", 1, 0))
    constrainQuality0to50(ImmutableItem("name", 1, 0)) should be(ImmutableItem("name", 1, 0))
    constrainQuality0to50(ImmutableItem("name", 1, 1)) should be(ImmutableItem("name", 1, 1))
    constrainQuality0to50(ImmutableItem("name", 1, 50)) should be(ImmutableItem("name", 1, 50))
    constrainQuality0to50(ImmutableItem("name", 1, 51)) should be(ImmutableItem("name", 1, 50))
  }

  test("ageBrie") {
    ageBrie(ImmutableItem("name", 1, -1)) should be(ImmutableItem("name", 1, 0))
    ageBrie(ImmutableItem("name", 1, 1)) should be(ImmutableItem("name", 1, 2))
  }

  test("ageDownBy") {
    // normal
    ageDownBy(1)(ImmutableItem("name", 1, 10)) should be(ImmutableItem("name", 1, 9))
    ageDownBy(2)(ImmutableItem("name", 1, 10)) should be(ImmutableItem("name", 1, 8))
    ageDownBy(3)(ImmutableItem("name", 1, 10)) should be(ImmutableItem("name", 1, 7))

    // double time
    ageDownBy(1)(ImmutableItem("name", -1, 10)) should be(ImmutableItem("name", -1, 8))
    ageDownBy(2)(ImmutableItem("name", -1, 10)) should be(ImmutableItem("name", -1, 6))
    ageDownBy(3)(ImmutableItem("name", -1, 10)) should be(ImmutableItem("name", -1, 4))

  }

  test("ageSulfuras") {
    ageSulfuras(ImmutableItem("name", 1, 1)) should be(ImmutableItem("name", 1, 80))
    ageSulfuras(ImmutableItem("name", 1, 2)) should be(ImmutableItem("name", 1, 80))
    ageSulfuras(ImmutableItem("name", 1, -1)) should be(ImmutableItem("name", 1, 80))
  }

  test("ageBackstage") {
    ageBackstage(ImmutableItem("name", 50, 1)) should be(ImmutableItem("name", 50, 2))
    ageBackstage(ImmutableItem("name", 11, 1)) should be(ImmutableItem("name", 11, 2))
    ageBackstage(ImmutableItem("name", 10, 1)) should be(ImmutableItem("name", 10, 2))
    ageBackstage(ImmutableItem("name", 6, 1)) should be(ImmutableItem("name", 6, 3))
    ageBackstage(ImmutableItem("name", 5, 1)) should be(ImmutableItem("name", 5, 3))
    ageBackstage(ImmutableItem("name", 4, 1)) should be(ImmutableItem("name", 4, 4))
    ageBackstage(ImmutableItem("name", 1, 1)) should be(ImmutableItem("name", 1, 4))
    ageBackstage(ImmutableItem("name", 0, 1)) should be(ImmutableItem("name", 0, 4))
    ageBackstage(ImmutableItem("name", -1, 1)) should be(ImmutableItem("name", -1, 0))
  }

  test("updateQuality - aged brie") {
    updateQuality(Seq(ImmutableItem("Aged Brie", 10, 10))).head should be(ImmutableItem("Aged Brie", 9, 11))
    updateQuality(Seq(ImmutableItem("Aged Brie", 10, 50))).head should be(ImmutableItem("Aged Brie", 9, 50))
  }

  test("updateQuality - backstage passes") {
    // level 1
    updateQuality(Seq(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 20, 10))).head should be(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 19, 11))

    // level 2 
    updateQuality(Seq(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 8, 49))).head should be(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 7, 50))
    updateQuality(Seq(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 8, 20))).head should be(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 7, 22))

    // level 3
    updateQuality(Seq(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 2, 20))).head should be(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 1, 23))

    // level 4
    updateQuality(Seq(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", 0, 50))).head should be(ImmutableItem("Backstage passes to a TAFKAL80ETC concert", -1, 0))
  }

  test("updateQuality - sulfuras") {
    updateQuality(Seq(ImmutableItem("Sulfuras, Hand of Ragnaros", 10, 10))).head should be(ImmutableItem("Sulfuras, Hand of Ragnaros", 10, 80))
  }

  test("updateQuality - common items ") {
    updateQuality(Seq(ImmutableItem("Some other item", 10, 65))).head should be(ImmutableItem("Some other item", 9, 50))
    updateQuality(Seq(ImmutableItem("Some other item", 10, 10))).head should be(ImmutableItem("Some other item", 9, 9))
    updateQuality(Seq(ImmutableItem("Some other item", -1, 10))).head should be(ImmutableItem("Some other item", -2, 8))
  }

  test("updateQuality - conjured") {
    updateQuality(Seq(ImmutableItem("Conjured item", 10, 65))).head should be(ImmutableItem("Conjured item", 9, 50))
    updateQuality(Seq(ImmutableItem("Conjured item", 10, 10))).head should be(ImmutableItem("Conjured item", 9, 8))
    updateQuality(Seq(ImmutableItem("Conjured item", -1, 10))).head should be(ImmutableItem("Conjured item", -2, 6))
  }

}