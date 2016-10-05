import scala.meta
import scala.meta._
import scala.annotation.StaticAnnotation

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
        List(other)
    }
    q"object $name extends TestedRules(${name.toString}) { ..$stats1 }"
  }
}
