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

    trait TestCases
    case class GeneratedTest(val validity: Stat, val efficiency: Stat, val functionImpurity: scala.collection.immutable.Seq[Stat], val generalImpurity: Stat) extends TestCases
    object EmptyTests extends TestCases

    def aggregateTest(accu : (scala.collection.immutable.Seq[Stat], scala.collection.immutable.Seq[Stat], scala.collection.immutable.Seq[Stat], scala.collection.immutable.Seq[Stat]), tests: TestCases) = {
      tests match {
        case EmptyTests => accu
        case GeneratedTest(validity, efficiency, functionImpurity, generalImpurity) => {
          (accu._1 :+ validity, accu._2 :+ efficiency, functionImpurity ++ accu._3, accu._4 :+ generalImpurity)
        }
      }
    }

    val impureTypes : scala.collection.mutable.Map[String, Type.Name] = scala.collection.mutable.Map[String, Type.Name]()
    val impureTypesDefs : scala.collection.mutable.ListBuffer[Stat] = scala.collection.mutable.ListBuffer[Stat]()

    def createImpureSubtype(tpe: Type.Function) : Type.Name = {
      val tpeName = Type.fresh("Impure")
      val newTpeArgs = List(tpeName)
      val funCtorCall = ("(" + tpe.toString + ")").parse[Ctor.Call].get
      val ctorArgs = List(param"val fun : $tpe")
      /*val buffer = q"var buffer : _root_.scala.collection.mutable.ListBuffer[String]"
      val name = q"var name : String"*/
      val addToBuffer = q"buffer.append(name)"
      val (applyParams, args) = (((tpe.params) zip (1 to tpe.params.length)) map { 
          case (t, i) => val name = Term.Name("x" + i);  val retArg : Term.Arg = arg"$name"; (param"$name : $t", retArg) 
        }).unzip
      val applyFunction = q"fun(...${scala.collection.immutable.Seq(args)})"
      val redefApply = q"def apply(..$applyParams) : ${tpe.res} = {..${List(addToBuffer, applyFunction)}}"
      val template = template"..${List(funCtorCall)} { ..${List(redefApply)} }"
      val caseClass = q"""case class $tpeName (val fun: $tpe, var buffer : _root_.scala.collection.mutable.ListBuffer[String] = _root_.scala.collection.mutable.ListBuffer[String](), var name: String = "") extends $template"""
      val genTArgs = List(tpe)
      val genTpe = t"Gen[..$newTpeArgs]"
      val genFun = q"arbitrary[..$genTArgs]"
      val enumFun = enumerator"fun <- $genFun"
      val createInstance = ctor"${Ctor.Ref.Name(tpeName.toString)}(fun)"
      val genName = Pat.fresh(tpeName.toString + "Gen")
      val valGen = q"val $genName : $genTpe = for {..${List(enumFun)}} yield $createInstance"
      val arbName = Pat.fresh(tpeName.toString + "Arb")
      val arbTpe = t"Arbitrary[..$newTpeArgs]"
      val argGen = arg"${genName.name}"
      val createArb = ctor"Arbitrary($argGen)"
      val lazyArb = q"implicit lazy val $arbName : $arbTpe = $createArb"
      impureTypes(tpe.toString) = tpeName
      impureTypesDefs.append(caseClass, valGen, lazyArb)
      return tpeName 
    }

    def handleTpFunction(tpe: Type.Function) : Type.Name = {
      impureTypes.get(tpe.toString) match {
        case Some(tpName : Type.Name) => tpName
        case None => createImpureSubtype(tpe)
      }
    }

    def bind(paramName: Term.Param.Name, tpName : Type.Name) : Term.Name => (Term.Param, scala.collection.immutable.Seq[Stat]) = {
      val newParam = param"$paramName : ${Some(tpName)}"
      val paramAsTerm : Term  = Term.Name(paramName.toString)
      val bindName = q"$paramAsTerm.name = ${paramName.toString}"
      def bindAll(bufferName: Term.Name) : (Term.Param, scala.collection.immutable.Seq[Stat]) = {
        val bindBuffer = q"$paramAsTerm.buffer = $bufferName"
        (newParam, scala.collection.immutable.Seq(bindName, bindBuffer))
      }
      bindAll
    }

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
            val tpName= handleTpFunction(tpe)
            val defs = bind(current.name, tpName)
            def unchanged(n: Term.Name) : (Term.Param, scala.collection.immutable.Seq[Stat]) = (current, Nil)
            createSubPossibilities(
              currentNames,
              leftAfter,
              newDefinitions :+ (unchanged _)
            ) #::: 
            createSubPossibilities(           
              currentNames :+ name.toString,
              leftAfter,
              newDefinitions :+ defs
            )
          }
          case Some(Type.Apply(tpe, args)) if findFunction.pattern.matcher(tpe.toString).matches => {
            args match {
              case (argTpes :+ retTpe) => {
                val tpe = Type.Function(argTpes, retTpe)
                val name = current.name
                val tpName= handleTpFunction(tpe)
                val defs = bind(current.name, tpName)
                def unchanged(n: Term.Name) : (Term.Param, scala.collection.immutable.Seq[Stat]) = (current, Nil)
                createSubPossibilities(
                  currentNames,
                  leftAfter,
                  newDefinitions :+ (unchanged _)
                ) #::: 
                createSubPossibilities(         
                  currentNames :+ name.toString,
                  leftAfter,
                  newDefinitions :+ defs
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
    
    val q"object $name { ..$stats }" = defn
    val (statsValidity, statsEfficiency, statsFunctionImpurity, statsGeneralImpurity) : (scala.collection.immutable.Seq[Stat], scala.collection.immutable.Seq[Stat], scala.collection.immutable.Seq[Stat], scala.collection.immutable.Seq[Stat]) = stats.map({
      case rule @ q"..$mods def $name[..$tparams](...$paramss): $tpe = Rewrite(${left: Term}, ${right: Term})"=>
        val params = paramss.head // TODO
        val test = q"""
          property(${name.toString + "Validity"}) = forAll {
            (..$params) => {
              val left = $left
              var right = $right
              left == right
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
              val test = {
                if ((t1 - t0) > (t2 - t1)) "faster"
                else if ((t1-t0) == (t2 -t1)) "equivalent"
                else "slower"
              }
              collect(test)(true)
            }
          }
        """
        val impureTestDefs :  Stream[(Seq[String], scala.collection.immutable.Seq[(Term.Name => (Term.Param, scala.collection.immutable.Seq[Stat]))])] = createImpurePossibilities(params)
 
        val impureTest = (impureTestDefs.flatMap {
          case (names, defs) => {
            val bufferName = Pat.fresh("buffer")
            val bufferDecl = q"var $bufferName = _root_.scala.collection.mutable.ListBuffer.empty[String]"
            val (args, seqDefs) = defs.map(_(bufferName.name)).unzip
            val allDefs = seqDefs.flatten
            val impureTest = q"""
              property(${name.toString + "{" + names.mkString("+")  + "}Impures"}) = forAll {
                (..$args) => {
                  { ..$allDefs }                  
                  ${bufferName.name}.clear                                  
                  val left = $left
                  val leftInfos = ${bufferName.name}.toList
                  ${bufferName.name}.clear                                  
                  val right = $right
                  val rightInfos = ${bufferName.name}.toList
                  leftInfos == rightInfos
                }
              }
            """
            List(bufferDecl, impureTest)
          }
        })
        val generalImpureTest = q"""
          property(${name.toString + "ImpureGeneral"}) = forAll { (..$params) => 
            this.cleanUp()
            val left = $left
            this.commitLeft()
            val right = $right
            this.commitRight()
            this.checkEffects()
          }        
          """
        
        GeneratedTest(test, performance, impureTest, generalImpureTest)
      case other =>
        EmptyTests
    }).foldLeft((scala.collection.immutable.Seq[Stat](), scala.collection.immutable.Seq[Stat](),scala.collection.immutable.Seq[Stat](), scala.collection.immutable.Seq[Stat]()))(aggregateTest)

    val validityName: Type.Name = Type.Name("ValidityTests")
    val efficiencyName: Type.Name = Type.Name("EfficiencyTests")
    val functionImpurityName: Type.Name = Type.Name("FunctionImpurityTests")
    val generalImpurityName: Type.Name = Type.Name("GeneralImpurityTests")

    val stats2 = stats ++ List( q"import org.scalacheck._",
                                q"import Arbitrary._",
                                q"import Gen._",
                                q"import Prop._",
                                q"""abstract class $validityName extends org.scalacheck.Properties(${name.toString + "Validity"})
                                { ..$statsValidity }""",
                                q"""abstract class $efficiencyName extends org.scalacheck.Properties(${name.toString + "Efficiency"})
                                { ..$statsEfficiency }""",
                                q"""class $functionImpurityName extends org.scalacheck.Properties(${name.toString + "FunctionImpurity"})
                                { ..${scala.collection.immutable.Seq(impureTypesDefs:_*) ++ statsFunctionImpurity.toVector} }""",
                                q"""abstract class $generalImpurityName extends org.scalacheck.Properties(${name.toString + "GeneralImpurity"}) with TestFunctions
                                { ..$statsGeneralImpurity }""")

    val rewritten = q"object $name { ..$stats2 }"

    import scala.io._
    import java.io._

    val file = new File("withAzgaga.scala")
    val fw = new FileWriter(file)
    fw.write(rewritten.toString)
    fw.close

    rewritten
  }
}
