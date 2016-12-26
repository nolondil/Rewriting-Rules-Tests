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
      def createSubPossibilities(
        currentNames: Seq[String],
        left: Seq[Term.Param],
        newDefinitions: scala.collection.immutable.Seq[(Term.Name => (Term.Param, scala.collection.immutable.Seq[Stat]))]
        ) : Stream[(Seq[String], scala.collection.immutable.Seq[(Term.Name => (Term.Param, scala.collection.immutable.Seq[Stat]))])] = {
        val findFunction = """^_root_.scala.Function[0-9]+$""".r
        left match {
          case Nil => {
            currentNames match {
              case Nil => Stream.empty
              case _ => Stream((currentNames, newDefinitions))
            }
          }
          case current :: leftAfter => 
            current.decltpe match {
            case Some(tpe: Type.Function) => {
              val name = current.name
              /*val subTypeName = Type.fresh(name + "Impure")
              val newParam = param"$name : $subTypeName"*/
              def unchanged(n: Term.Name) : (Term.Param, scala.collection.immutable.Seq[Stat]) = (current, Nil)
              createSubPossibilities(
                currentNames,
                leftAfter, newDefinitions :+ (unchanged _)
              ) #::: 
              createSubPossibilities(
                currentNames :+ name.toString,
                leftAfter,
                newDefinitions :+ createNewDefinitions(tpe, name)
              )
            }
            case Some(Type.Apply(tpe, args)) if findFunction.pattern.matcher(tpe.toString).matches => {
              args match {
                case (argTpes :+ retTpe) => {
                  val funTpe = Type.Function(argTpes, retTpe)
                  val name = current.name
                  /*val subTypeName = Type.fresh(name + "Impure") // plus bas
                  val newParam = param"$name : $subTypeName"*/
                  def unchanged(n: Term.Name) : (Term.Param, scala.collection.immutable.Seq[Stat]) = (current, Nil)              
                  createSubPossibilities(
                    currentNames,
                    leftAfter,
                    newDefinitions :+ (unchanged _)
                  ) #::: 
                  createSubPossibilities(
                    currentNames :+ name.toString,
                    leftAfter,
                    newDefinitions :+ createNewDefinitions(funTpe, name)
                  )        
                }
              }
            }
            case _ => {
              def unchanged(n: Term.Name) : (Term.Param, scala.collection.immutable.Seq[Stat]) = (current, Nil)              
              createSubPossibilities(currentNames, leftAfter, newDefinitions :+ (unchanged _))
            }
          }
        }
      }

      def createImpurePossibilities(params : Seq[Term.Param]) : Stream[(Seq[String], scala.collection.immutable.Seq[(Term.Name => (Term.Param, scala.collection.immutable.Seq[Stat]))])] = {
        createSubPossibilities(Nil, params, Nil)
      }

      def createNewDefinitions(funTpe : Type.Function, name: Term.Param.Name) : Term.Name => (Term.Param, scala.collection.immutable.Seq[Stat]) = {
        def retFun(bufferName: Term.Name): (Term.Param, scala.collection.immutable.Seq[Stat]) = {
          val subTypeName = Type.fresh(name + "Impure")
          val newParam = param"$name : $subTypeName"
          val newTpeArgs = List(subTypeName)
          val genTArgs = List(funTpe)
          val genTpe = t"Gen[..$newTpeArgs]"
          val genFun = q"arbitrary[..$genTArgs]"
          val enumFun = enumerator"fun <- $genFun"
          val createInstance = ctor"${Ctor.Ref.Name(subTypeName.toString)}(fun)"
          val genName = Pat.fresh(subTypeName.toString + "Gen")
          val valGen = q"val $genName : $genTpe = for {..${List(enumFun)}} yield $createInstance"
          val arbName = Pat.fresh(subTypeName.toString + "Arb")
          val arbTpe = t"Arbitrary[..$newTpeArgs]"
          val argGen = arg"${genName.name}"
          val createArb = ctor"Arbitrary($argGen)"
          val lazyArb = q"implicit lazy val $arbName : $arbTpe = $createArb"
          val ctorArgs = List(param"val fun : $funTpe")
          val funCtorCall = ("(" + funTpe.toString + ")").parse[Ctor.Call].get
          val (applyParams, args) = (((funTpe.params) zip (1 to funTpe.params.length)) map { 
            case (t, i) => val name = Term.Name("x" + i);  val retArg : Term.Arg = arg"$name"; (param"$name : $t", retArg) 
          }).unzip
          val addToBuffer = q"$bufferName.append(${name.toString})"
          val applyFunction = q"fun(...${scala.collection.immutable.Seq(args)})"
          val redefApply = q"def apply(..$applyParams) : ${funTpe.res} = {..${List(addToBuffer, applyFunction)}}"
          val template = template"..${List(funCtorCall)} { ..${List(redefApply)} }"
          val caseClass = q"""case class $subTypeName (val fun: $funTpe) extends $template"""
          (newParam, List(valGen, lazyArb, caseClass))
        }
        retFun
      }

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
        val impureTest = createImpurePossibilities(params).flatMap {
          case (names, defs) => {
            val bufferName = Pat.fresh("buffer")
            val bufferDecl = q"var $bufferName = scala.collection.mutable.ListBuffer.empty[String]"
            val (args, seqDefs) = defs.map(_(bufferName.name)).unzip
            val allDefs = seqDefs.flatten
            val impureTest = q"""
              property(${name.toString + names.mkString("") + "Impures"}) = forAll {
                (..$args) => {
                  val left = $left
                  val leftInfos = ${bufferName.name}.toVector
                  ${bufferName.name} = scala.collection.mutable.ListBuffer.empty[String]
                  val right = $right
                  val rightInfos = ${bufferName.name}.toVector
                  if (leftInfos != rightInfos) {
                    println("buffer: " + ${bufferName.toString})
                    println(leftInfos)
                    println(rightInfos)
                  }
                  leftInfos == rightInfos
                }
              }
            """
            List(bufferDecl) ++ (allDefs :+ impureTest)
          }
        }
        
        List(test, performance) ++ impureTest
      case other =>
        Nil
    }

    val suitName: Type.Name = Type.Name("TestSuit")

    val properties = stats1

    val stats2 = stats ++ List( q"import org.scalacheck._",
                                q"import Arbitrary._",
                                q"import Gen._",
                                q"import Prop._",
                                q"""abstract class $suitName extends Properties(${name.toString}) with TestFunctions
                                { ..$properties }""")

    q"object $name { ..$stats2 }"
  }
}
