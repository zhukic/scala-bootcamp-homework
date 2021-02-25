package typeclass

object Typeclass {

  trait HashCoder[T] {
    def hash(entity: T): Int
  }

  object HashCoder {

    def apply[T](implicit hashCoder: HashCoder[T]): HashCoder[T] = hashCoder
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit hashCoder: HashCoder[A]): Int = hashCoder.hash(x)
  }

  implicit val stringHashCoder: HashCoder[String] = str => str.hashCode

  println("abs".hash)
}

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = (money1, money2) => money1.amount.compareTo(money2.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit val userShow: Show[User] = user => s"User(id = ${user.id}, name = ${user.name})"

  implicit class ShowSyntax[T](val entity: T) {
    def show(implicit show: Show[T]): String = show.show(entity)
  }

  User("1", "Oleg").show
}

object Task3 extends App {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  object Parse {
    def apply[T](implicit parse: Parse[T]): Parse[T] = parse
  }

  final case class User(id: String, name: String)

  implicit val userParser: Parse[User] = entityStr => entityStr.split("\\s").toList match {
    case id :: name :: Nil => Right(User(id, name))
    case _ => Left(s"Unexpected user format - $entityStr")
  }

  implicit class ParseSyntax(val entity: String) {
    def parse[Y: Parse]: Either[Error, Y] = Parse[Y].parse(entity)
  }

  println("1 Oleg".parse[User])
}

object Task4 extends App {

  trait TypeSafeEquals[T] {
    def equals(entity: T, other: T): Boolean
  }

  object TypeSafeEquals {
    def apply[T](implicit typeSafeEquals: TypeSafeEquals[T]): TypeSafeEquals[T] = typeSafeEquals
  }

  implicit class EntityComparator[T : TypeSafeEquals](entity: T) {
    def ===(other: T): Boolean = TypeSafeEquals[T].equals(entity, other)
  }

  implicit val stringEquals: TypeSafeEquals[String] = (a, b) => a.equals(b)
  implicit val intEquals: TypeSafeEquals[Int] = (a, b) => a.equals(b)

  "1" === "2"
  2 === 3
  // "1" === 1 won't compile
}

object AdvancedHomework {
  trait Functor[F[_]] {
    def flatMap[A, B](x: F[A])(f: A => F[B]): F[B]
  }
}
