import scala.meta
import scala.meta._
import scala.annotation.StaticAnnotation
import org.scalacheck._

trait TestFunctions {
  def commitLeft(): Unit
  def commitRight(): Unit
  def checkEffects(): Boolean
  //implicit def genImpure1[T, U]: Gen[ImpureFunction1[T, U]]
}

class rewrites extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"object $name { ..$stats }" = defn
    val stats1 = stats.flatMap {
      case rule @ q"..$mods def $name[..$tparams](...$paramss): $tpe = $body" =>
        val q"Rewrite(${left: Term}, ${right: Term})" = body
        val params = paramss.head // TODO
        val test = q"""
          property(${name.toString}) = forAll {
            (..$params) => {
              val left = $left
              this.commitLeft()
              var right = $right
              this.commitRight()
              (left == right) && this.checkEffects()
            }
          }
        """
        List(test)
      case other =>
        Nil
    }
    val suitName: Type.Name = Type.Name("TestSuit")

    val stats2 = stats ++ List(q"abstract class $suitName extends Properties(${name.toString}) with TestFunctions { ..$stats1 }")

    q"object $name { ..$stats2 }"
  }
}
