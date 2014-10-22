package adhoc

import org.scalatest.prop.GeneratorDrivenPropertyChecks._
import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

trait SpreadSheetColumn {

  def columnIndex2String(index: Int): String = {
    val Base = 26
    val A = 'A'.toInt

    @tailrec
    def toSpreadSheetBase(acc: List[Char], remainder: Int): List[Char] =
      if (remainder == 0) acc
      else toSpreadSheetBase(unitToChar(remainder % Base) :: acc, remainder / Base)

    def unitToChar(number: Int): Char =
      (A - 1 + number).toChar

    toSpreadSheetBase(List(), index + 1).mkString
  }

  def string2ColumnIndex(name: String): Int = {
    val Base = 26
    val A = 'A'.toInt

    @tailrec
    def fromSpreadSheetBase(acc: Int, unitValue: Int, remainder: String): Int =
      if (remainder.isEmpty) acc
      else fromSpreadSheetBase(unitValue * spreadSheetValueOf(remainder.head) + acc, Base * unitValue, remainder tail)

    def spreadSheetValueOf(character: Char): Int =
      character - A + 1

    fromSpreadSheetBase(0, 1, name.reverse) - 1
  }
}

class SpreadsheetColumnCalculatorSpec extends WordSpecLike with Matchers {

  "Column calculator" should {

    "yield A for 0" in new SpreadSheetColumn {
      columnIndex2String(0) should be("A")
    }

    "yield B for 1" in new SpreadSheetColumn {
      columnIndex2String(1) should be("B")
    }

    "yield AA for 26" in new SpreadSheetColumn {
      columnIndex2String(26) should be("AA")
    }

    "yield AB for 27" in new SpreadSheetColumn {
      columnIndex2String(27) should be("AB")
    }

    "yield XFD for 16383" in new SpreadSheetColumn {
      columnIndex2String(16383) should be("XFD")
    }

    "yield 0 for A" in new SpreadSheetColumn {
      string2ColumnIndex("A") should be(0)
    }

    "yield 1 for B" in new SpreadSheetColumn {
      string2ColumnIndex("B") should be(1)
    }

    "yield 26 for AA" in new SpreadSheetColumn {
      string2ColumnIndex("AA") should be(26)
    }

    "yield 16383 for XFD" in new SpreadSheetColumn {
      string2ColumnIndex("XFD") should be(16383)
    }

    "round trip" in new SpreadSheetColumn {
      forAll((n: Int) => {
          whenever(n > 1) {
            val nameForN = columnIndex2String(n)
            string2ColumnIndex(nameForN) == n
          }
        })
    }
  }
}
