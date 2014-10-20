package net.debasishg.codemesh.domain.trade
package model

import scalaz._
import Scalaz._
import \/._

trait ContractNoteModel {this: TradeModel =>
  val isAddressDefined = true
  case class ContractNote(trade: Trade)

  def makeContractNote(trade: Trade): \/[String, ContractNote] =
    if (isAddressDefined) right(ContractNote(trade)) else left("Address not defined")
}
