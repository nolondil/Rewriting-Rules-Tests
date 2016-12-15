import scala.meta
import scala.meta._
import scala.annotation.StaticAnnotation
import org.scalacheck._

trait TestFunctions {
  def cleanUp(): Unit
  def commitLeft(): Unit
  def commitRight(): Unit
  def checkEffects(): Boolean
}

class rewrites extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    /*def createSubClass(args: scala.collection.immutable.Seq[scala.meta.Type.Arg], ret: scala.meta.Type, functionName: String) = {
      val listType = args.mkString(",")
      val extensionTpe = t"($listType) => $ret"
      val retTpe = ret.toString
      val listArgs = (1 to args.length).zip(args).map(_ match { case (n, tpe) => Term.Param(Nil, Term.Name("x"+n), Some(tpe), None)}).toSeq
      val listArgsName = listArgs.map(_.name.toString).mkString(",")
      val ctorParams = scala.collection.immutable.Seq(Term.Param(Nil, Term.Name("x"), Some(t"String"), None), Term.Param(Nil, Term.Name("f"), Some(extensionTpe), None))
      val impureName : Type.Name = Type.Name(functionName + "Impure")
      val n = args.size
      q"""
        class $impureName (..$ctorParams) extends (($listType) => $retTpe) {
          override def apply(..$listArgs) = {
            f($listArgsName)
          }
        }
      """
    }*/
    val q"object $name { ..$stats }" = defn
    val stats1 = stats.flatMap {
      case rule @ q"..$mods def $name[..$tparams](...$paramss): $tpe = Rewrite(${left: Term}, ${right: Term})"=>
        val params = paramss.head // TODO
        val test = q"""
          property(${name.toString + "Validity"}) = forAll {
            (..$params) => {
              this.cleanUp()
              val left = $left
              this.commitLeft()
              var right = $right
              this.commitRight()
              this.checkEffects() && left == right
            }
          }
        """
        val performance = q"""
          property(${name.toString + "Performance"}) = forAll {
            (..$params) => {
              lazy val left = $left
              lazy val right = $right              
              val t0 = System.nanoTime()
              left
              val t1 = System.nanoTime()
              right
              val t2 = System.nanoTime()
              val test = 
                if ((t1 - t0) >= (t2 - t1)) "faster"
                else "slower"
              collect(test)(true)
            }
          }
        """
        List(test, performance)
      case other =>
        Nil
    }
    val suitName: Type.Name = Type.Name("TestSuit")

    val properties =  stats1

    val stats2 = stats ++ List( q"import org.scalacheck._",
                                q"import Arbitrary._",
                                q"import Gen._",
                                q"import Prop._",
                                q"""abstract class $suitName extends Properties(${name.toString}) with TestFunctions
                                { ..$properties }""")

    q"object $name { ..$stats2 }"
  }
}
