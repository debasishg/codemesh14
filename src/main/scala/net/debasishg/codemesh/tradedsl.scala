package net.debasishg.codemesh.domain.trade
package dsl

import scalaz._
import Kleisli._
import Scalaz._

object TradeDsl {

  import model.TradeModel._

  /**
   * processes client orders in unstructured form and generates a list of Order objects
   */
  val clientOrders: Kleisli[List, ClientOrderSheet, Order] = kleisli(fromClientOrders)

  /**
   * executes an Order in a market on behalf of a specific broker account
   * generates a sequence of Executions
   */
  def execute(market: Market, brokerAccount: Account): Kleisli[List, Order, Execution] = kleisli { order =>
    order.items.map {item =>
      Execution(brokerAccount, item.ins, "e-123", market, item.price, item.qty)
    }
  }

  /**
   * allocates an execution to a List of client accounts
   * generates a List of trades
   */
  def allocate(accounts: List[Account]): Kleisli[List, Execution, \/[String, Trade]] = kleisli { execution =>
    val q = execution.quantity / accounts.size
    accounts.map {account =>
      makeTrade(account, execution.instrument, "t-123", execution.market, execution.unitPrice, q)
    }
  }

  def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account]) = {
    // client orders         executed at market by broker        & allocated to client accounts
    clientOrders      >=>    execute(market, broker)       >=>   allocate(clientAccounts)
  }
}
