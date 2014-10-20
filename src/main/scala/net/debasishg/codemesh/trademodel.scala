package net.debasishg.codemesh.domain.trade
package model

import java.util.{ Date, Calendar }

import scalaz._
import Scalaz._
import \/._

trait TradeModel {this: RefModel =>

  case class Trade(account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal, tradeDate: Date = Calendar.getInstance.getTime, 
    valueDate: Option[Date] = None, taxFees: Option[List[(TaxFeeId, BigDecimal)]] = None, 
    netAmount: Option[BigDecimal] = None)

  // various tax/fees to be paid when u do a trade
  sealed trait TaxFeeId
  case object TradeTax extends TaxFeeId
  case object Commission extends TaxFeeId
  case object VAT extends TaxFeeId
  case object Surcharge extends TaxFeeId

  def makeTrade(account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal): \/[String, Trade] =
      right(Trade(account, instrument, refNo, market, unitPrice, quantity))
}

object TradeModel extends ExecutionModel with OrderModel with RefModel with TradeModel with ContractNoteModel 
