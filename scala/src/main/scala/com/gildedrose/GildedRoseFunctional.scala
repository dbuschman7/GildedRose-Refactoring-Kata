package com.gildedrose

case class ImmutableItem(name: String, sellIn: Int, quality: Int)
object ImmutableItem {
  def fromItem(in: Item) = ImmutableItem(in.name, in.sellIn, in.quality)
}

object GildedRoseFunctional {

  // Rule : "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
  def updateQuality(items: Seq[ImmutableItem] = Seq.empty): Seq[ImmutableItem] = {

    def rulesToApply(name: String): Seq[ImmutableItem ⇒ ImmutableItem] = name match {
      case n if n.startsWith("Backstage passes") ⇒ Seq(decrementSellIn, ageBackstage, constrainQuality0to50)
      case n if n.startsWith("Aged Brie")        ⇒ Seq(decrementSellIn, ageBrie, constrainQuality0to50)
      case n if n.startsWith("Sulfuras")         ⇒ Seq(ageSulfuras) // do no decrement sellIn or constrain
      case n if n.startsWith("Conjured")         ⇒ Seq(decrementSellIn, ageDownBy(2), constrainQuality0to50)
      case _                                     ⇒ Seq(decrementSellIn, ageDownBy(1), constrainQuality0to50)
    }

    items.map { item ⇒
      rulesToApply(item.name)
        .foldLeft(item) { case (item, transform) ⇒ transform(item) }
    }
  }

  // 
  // Internal Rules for items
  // ////////////////////////////////////

  // Rule : At the end of each day our system lowers both values for every item [ NOTE : sellIn ONLY done here ] 
  private[gildedrose] def decrementSellIn(in: ImmutableItem) = in.copy(sellIn = in.sellIn - 1)

  // Rule : The Quality of an item is never negative
  // Rule : The Quality of an item is never more than 50
  // Legacy : impl deos not check out-of-bounds conditions for either of these, can create item with 0 < quality > 50
  private[gildedrose] def constrainQuality0to50(in: ImmutableItem) = in.copy(quality = Math.max(0, Math.min(in.quality, 50)))

  // Rule : "Aged Brie" actually increases in Quality the older it gets
  // Legacy : follows normal ageBy but in reverse, so after with negative sellIn, quality is increased by 2 
  private[gildedrose] def ageBrie: ImmutableItem => ImmutableItem = ageDownBy(-1)

  // Rule : Once the sell by date has passed, Quality degrades twice as fast
  // Rule : At the end of each day our system lowers both values for every item [ NOTE : quality ONLY done here ] 
  // Rule : "Conjured" items degrade in Quality twice as fast as normal items
  private[gildedrose] def ageDownBy(ageBy: Int)(item: ImmutableItem): ImmutableItem = {
    val newQuality = item.sellIn match {
      case in if in >= 0  ⇒ item.quality - ageBy
      case in if in < 0 ⇒ item.quality - (ageBy * 2)
    }
    item.copy(quality = newQuality)
  }

  // Rule : however "Sulfuras" is a legendary item and as such its Quality is 80 and it never alters.
  // Legacy : does not check this value so the quality is not guaranteed to set at 80 per the rule
  private[gildedrose] def ageSulfuras(item: ImmutableItem): ImmutableItem = item.copy(quality = 80)

  // Rule : "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
  //         Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
  //         Quality drops to 0 after the concert
  private[gildedrose] def ageBackstage(item: ImmutableItem): ImmutableItem = item.sellIn match {
    case in if in >= 10 ⇒ item.copy(quality = item.quality + 1)
    case in if in >= 5  ⇒ item.copy(quality = item.quality + 2)
    case in if in >= 0  ⇒ item.copy(quality = item.quality + 3)
    case _              ⇒ item.copy(quality = 0)
  }

}