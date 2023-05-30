import scala.collection.mutable

class ShoppingCart {
  private val itemPrices: Map[String, Double] = Map(
    "Soup" -> 0.65,
    "Bread" -> 0.80,
    "Milk" -> 1.30,
    "Apples" -> 1.00
  )

  private val specialOffers: Map[String, (mutable.Map[String, Int], Double) => Double] = Map(

    "Apples" -> ((itemsCount, price) => itemsCount("Apples") * price * 0.1),
    "Bread" -> ((itemsCount, price) => if (itemsCount("Soup") >= 2)
      Math.min(Math.floor(itemsCount("Soup") / 2), itemsCount("Bread")) * (price / 2) else 0)
  )

  def calculateTotalPrice(basket: Seq[String]): String = {
    val itemsCount: mutable.Map[String, Int] = mutable.Map.empty.withDefaultValue(0)
    val subtotal: Double = basket.foldLeft(0.0) { (total, item) =>
      itemsCount(item) += 1
      total + itemPrices(item)
    }

    val discountedItems: Seq[Option[String]] = itemsCount.keys.flatMap { item =>
      specialOffers.get(item).map { discountFn =>
        //val count = itemsCount(item)
        val discount = discountFn(itemsCount, itemPrices(item))
        val discountmessage =
          Map("Apples" -> s"10% off:" ,
          "Bread" -> s"1/2 off when buying 2 cans of soup:")

        if (discount > 0) Some(s"$item ${discountmessage(item)} ${formatPrice(discount)}") else None

      }.filter(_.isDefined)
    }.toSeq


    val totalPrice: Double = subtotal - discountedItems.map(getPrice).sum

    val output = new StringBuilder
    output.append(s"Subtotal: ${formatPrice(subtotal)}\n")
    if (discountedItems.nonEmpty ) {
      output.append(discountedItems.flatten.mkString("", "\n", "\n"))
    } else {
      output.append("(No offers available)\n")
    }
    output.append(s"Total price: ${formatPrice(totalPrice)}")

    output.toString()
  }

  private def getPrice(discountedItem: Option[String]): Double = {
    discountedItem match {
      case Some(str) => str.split(" [/s£]").last.toDouble
      case None => 0.0
    }
  }

    private def formatPrice(price: Double): String = f"£$price%.2f"
  }

  object PriceBasket {
    def main(args: Array[String]): Unit = {
      try{
      if (args.length == 0) {
        println("Please provide a list of items in the basket.")
      } else {
        val basket = args.toList
        val shoppingCart = new ShoppingCart()
        val totalPrice = shoppingCart.calculateTotalPrice(basket)
        println(totalPrice)
      }}
      catch {
        case _: NoSuchElementException => println("One or more of your items are not found in our inventory")

      }

    }

}