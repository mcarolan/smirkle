package net.mcarolan.smirkle
import enumeratum._

object Domain {

  sealed trait Colour extends EnumEntry

  object Colour extends Enum[Colour] {
    case object Red extends Colour
    case object Blue extends Colour
    case object Green extends Colour
    case object Yellow extends Colour
    case object Purple extends Colour
    case object Orange extends Colour

    override def values: IndexedSeq[Colour] = findValues
  }

  sealed trait Shape extends EnumEntry

  object Shape extends Enum[Shape] {
    case object One extends Shape
    case object Two extends Shape
    case object Three extends Shape
    case object Four extends Shape
    case object Five extends Shape
    case object Six extends Shape

    override def values: IndexedSeq[Shape] = findValues
  }

}
