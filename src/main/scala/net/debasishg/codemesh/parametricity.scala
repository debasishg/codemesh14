package net.debasishg.codemesh

import java.util.{ Date, Calendar }
import scala.language.higherKinds

import scalaz._
import Scalaz._

/**
 * 1. <b>aggregate:</b>
 *    (a) functional design of aggregates using algebraic data types. What's an ADT ?
 *        An ADT is just a data type defined by one or more data constructors, each of which may 
 *        contain zero or more arguments. We say that the data type is the sum or union of its data 
 *        constructors, and each data constructor is the product of its arguments, hence the name 
 *        algebraic data type.
 *    (b) lean unlike rich model in OO - just the content required to model the structure. Functions to
 *        to manipulate ADTs are outside the ADT in modules. Since functions are not constrained to live within
 *        classes, reuse is better and so is modularity.
 *    (c) functional updates - lens : focus is on composition even with updates
 *
 * 2. <b>repository:</b>
 *    (a) injecting a repository within the domain APIs
 *
 * 3. <b>specification:</b>
 *    (a) compositional validation through uses of monad transformers
 *
 * 4. <b>parametricity & abstraction:</b>
 * Theme : Power of parametricity / parametric polymorphism
 */
object CodeMesh {

  def today = Calendar.getInstance.getTime

  case class Account(no: String, name: String)

  sealed trait Currency
  case object USD extends Currency
  case object AUD extends Currency
  case object SGD extends Currency
  case object INR extends Currency

  case class Money(amount: BigDecimal) {
    def +(m: Money): Money = Money(amount + m.amount)
    def /(f: BigDecimal): Money = Money(amount / f)
  }

  object Money {
    def apply(m: Int): Money = Money(m: BigDecimal)
  }

  implicit val MoneyMonoid = new Monoid[Money] {
    def zero: Money = Money(0)
    def append(m1: Money, m2: => Money): Money = m1 + m2
  }

  def inBaseCcy(e: (Currency, Money)): (Currency, Money) = e match {
    case (USD, x) => (USD -> x)
    case (AUD, x) => (USD -> x / 1.2)
    case (SGD, x) => (USD -> x / 1.3)
    case (INR, x) => (USD -> x / 60)
  }

  case class Transaction(entries: Map[Currency, Money])

  implicit val TransactionMonoid = new Monoid[Transaction] {
    def zero: Transaction = Transaction(Map.empty[Currency, Money])
    def append(t1: Transaction, t2: => Transaction): Transaction = Transaction(t1.entries |+| t2.entries)
  }

  case class Balance(txn: Transaction)

  implicit val BalanceMonoid = new Monoid[Balance] {
    def zero: Balance = Balance(TransactionMonoid.zero)
    def append(b1: Balance, b2: => Balance): Balance = Balance(b1.txn |+| b2.txn)
  }

  case class BaseCcyValue(txn: Transaction)

  implicit val BaseCcyValueMonoid = new Monoid[BaseCcyValue] {
    def zero: BaseCcyValue = BaseCcyValue(TransactionMonoid.zero)
    def append(b1: BaseCcyValue, b2: => BaseCcyValue): BaseCcyValue = {
      val t = b1.txn |+| b2.txn
      BaseCcyValue(
        Transaction(
          t.entries.foldLeft(Map.empty[Currency, Money]) { (acc, e) => 
            acc |+| Map(inBaseCcy(e)) 
          }
        )
      )
    }
  }

  def mapReduce[F[_], A, B](as: F[A], f: A => B)(implicit fd: Foldable[F], m: Monoid[B]) = fd.foldMap(as)(f)
 
  val t1 = Transaction(
    Map(
      USD -> Money(BigDecimal(100)),
      AUD -> Money(BigDecimal(100))
    )
  )
  val t2 = Transaction(
    Map(
      SGD -> Money(BigDecimal(100)),
      USD -> Money(BigDecimal(100))
    )
  )
  val t3 = Transaction(
    Map(
      USD -> Money(BigDecimal(100))
    )
  )

  /**
   * a function that's based on concrete abstractions / data types, not usable outside the body
   * of this specific implementation. Nothing interesting with this specific implementation. But can we 
   * find anything interesting in the types and the overall behavior of this function ? Let's try to
   * figure out some hotspots :-
   * 
   * (a) a list as an input and a fold operation on the list
   */
  def computeBalance(txns: List[Transaction]) = Transaction(
    txns.foldLeft (Map.empty[Currency, Money]) { (acc, txn) =>
      mergeMoneyMaps(acc, txn.entries)
    }
  )

  private def mergeMoneyMaps(m1: Map[Currency, Money], m2: Map[Currency, Money]) = m2.foldLeft (m1) { (acc, e) =>
    val (c, m) = e
    acc + ((c, acc.get(c).map(_ + m).getOrElse(m)))
  }

  def computeBaseCcyValue(txns: List[Transaction]) = Transaction(
    txns.foldLeft (Map.empty[Currency, Money]) { (acc, txn) =>
      mergeMoneyMaps2(acc, txn.entries)
    }
  )

  private def mergeMoneyMaps2(m1: Map[Currency, Money], m2: Map[Currency, Money]) = {
    val m = m2.foldLeft (m1) { (acc, e) =>
      val (c, m) = e
      acc + ((c, acc.get(c).map(_ + m).getOrElse(m)))
    }
    m.toList.map(inBaseCcy).groupBy(_._1).mapValues(_.map(_._2)).mapValues(x => x.foldLeft(Money(0))(_ + _))
  }

  val bal = mapReduce(List(t1, t2, t3), Balance)
  val bvl = mapReduce(List(t1, t2, t3), BaseCcyValue)

  val bal2 = computeBalance(List(t1, t2, t3))
  val bvl2 = computeBaseCcyValue(List(t1, t2, t3))
}
