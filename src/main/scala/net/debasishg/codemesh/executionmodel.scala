package net.debasishg.codemesh.domain.trade
package model

trait ExecutionModel {this: RefModel =>
  case class Execution(account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal)
}
