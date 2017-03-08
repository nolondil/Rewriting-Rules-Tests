import scala.meta
import scala.meta._
import scala.annotation.StaticAnnotation
import org.scalacheck._

trait TestFunctions {
  // Called at the start of the test
  def cleanUp(): Unit
  // Meant to observe the side effects of "from" (left)
  def commitLeft(): Unit
  // Meant to observe the side effects of "to" (right)
  def commitRight(): Unit
  // Verify the state of side effects from both sides
  def checkEffects(): Prop
}

class rewrites extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    import scala.collection.immutable.Seq
    import scala.collection.mutable.Map
    import scala.collection.mutable.ListBuffer

    // Represent all test generated from a single def
    // or None if it is not a rule
    trait TestCases
    case class GeneratedTest(
      val correctness: Stat,
      val efficiency: Stat,
      val functionImpurity: Seq[Stat],
      val generalImpurity: Stat
    ) extends TestCases
    object EmptyTests extends TestCases

    /**
     * @param accu accumulator of all tests for a collection
     *              1st: correctness
     *              2nd: efficiency
     *              3rd: function impurity
     *              4th: user defined impurity
     * @param tests current item in the collection
     *
     * this is a method to use with a foldLeft after mapping all
     * elements form the definition to some tests
     */
    def aggregateTest(
      accu : (Seq[Stat], Seq[Stat], Seq[Stat], Seq[Stat]),
      tests: TestCases
    ) = {
      tests match {
        // Nothing to add
        case EmptyTests => accu
        // Split all tests and add them to their respective
        // group
        case GeneratedTest(
          correctness,
          efficiency,
          functionImpurity,
          generalImpurity
        ) => {
          (
            accu._1 :+ correctness,
            accu._2 :+ efficiency,
            functionImpurity ++ accu._3,
            accu._4 :+ generalImpurity
          )
        }
      }
    }

    def generateSpeedTest(
      params: Seq[Term.Param],
      leftComputation: Term,
      rightComputation: Term
    ) : Seq[Stat] = {
      val valDefs = params.map {
        case Term.Param(_, name, decltpe, _) => {
          val valName = Pat.Var.Term(Term.Name(name.toString))
          val valTpe = decltpe.get.toString.parse[Type].get
          q"val $valName = arbitrary[$valTpe].sample.get"
        }
      }
      val checks = Seq(
        q"val t0 = System.nanoTime()",
        q"val discardedLeft = $leftComputation",
        q"val t1 = System.nanoTime()",
        q"val discardedRight = $rightComputation",
        q"val t2 = System.nanoTime()",
        q"val leftTime = t1 - t0",
        q"val rightTime = t2 - t1",
        q"val minTime = Math.min(leftTime, rightTime)",
        q"val minMeasurableTime = 50000",
        q"""val improvement = if (minTime < minMeasurableTime) {
          val iterations = (minTime/minMeasurableTime).toInt
          val t0It = System.nanoTime()
          for (_ <- 1 to iterations) {
            $leftComputation
          }
          val t1It = System.nanoTime()
          for (_ <- 1 to iterations) {
            $rightComputation
          }
          val t2It = System.nanoTime()
          val leftTimeIt = t1It - t0It
          val rightTimeIt = t2It - t1It
          leftTimeIt.toDouble/rightTimeIt.toDouble
        } else {
          leftTime.toDouble/rightTime.toDouble
        }
        """,
        q"improvement"
      )
      valDefs ++ checks
    }
    
    // Attributes names for all impure classes
    val funName = Term.Name("fun")
    val bufferName = Term.Name("buffer")
    val effectName = Term.Name("effect")

    // Common constructor parameters
    val bufferParam = param"""
      var $bufferName: _root_.scala.collection.mutable.ListBuffer[String] = null
    """
    val effectParam = param"""
      var $effectName: String = null
    """

    // New impure type maping
    val impureTypes = Map[String, Type.Name]()
    // All new type definitions
    val impureTypesDefs = ListBuffer[Stat]()


    // regex to find when a Type.Apply is a Type.Function
    val findFunction = """^_root_.scala.Function[0-9]+$""".r.pattern.matcher(_)

    /**
     * @param tpe function type that we want to create an impure subclass
     *
     * It will add the definition of the new subtype to the impureTypesDefs
     * and create a mapping inside impureTypes for further references
     */
    def createImpureSubtype(tpe: Type.Function) : Type.Name = {
      // create list of parameters from the types of Type.Function
      // and a list of the name for a call with the parameters
      val (applyParams, args) = (((tpe.params) zip (1 to tpe.params.length)) map { 
          case (t, i) => { 
            val name = Term.Name("x" + i)
            val retArg : Term.Arg = arg"$name"
            (param"$name : $t", retArg)
          }
        }).unzip
      // Creation of the parent class constructor call
      val funCtorCall = ("(" + tpe.toString + ")").parse[Ctor.Call].get

      // New type name
      val tpeName = Type.fresh("Impure")

      // Constructor parameters      
      val funParam = param"val $funName: $tpe"
      val ctorParamss = List(List(funParam, bufferParam, effectParam))
      
      // Call needed for the redefinition of the apply method
      // Buffer manipulation
      val addToBuffer = q"""$bufferName.append($effectName + "(" + List(..${args}).mkString(", ")  + ")")"""
      // Call to the function
      val applyFunction = q"$funName(...${Seq(args)})"
      // New apply method
      val redefApply = q"""
        def apply(..$applyParams) : ${tpe.res} = {
          ..${List(addToBuffer, applyFunction)}
        }
      """

      // Definition of the new class
      val template = template"..${List(funCtorCall)} { ..${List(redefApply)} }"
      val caseClass = q"""
          case class $tpeName (...$ctorParamss) extends $template
      """
      
      // Scalacheck generators
      // Type parameters for Arbitrary usage
      val genTArgs = List(tpe)
      // Type parameters for the new Gen
      val newTpeArgs = List(tpeName)
      // Type of the new Generator
      val genTpe = t"Gen[..$newTpeArgs]"
      // Call to get a random generated function from the ScalaCheck
      val genFun = q"arbitrary[..$genTArgs]"
      
      // Generator definition
      val enumFun = enumerator"fun <- $genFun"
      val createInstance = ctor"${Ctor.Ref.Name(tpeName.toString)}(fun)"
      val genName = Pat.fresh(tpeName.toString + "Gen")
      val valGen = q"""
        val $genName : $genTpe = 
          for {..${List(enumFun)}}
          yield $createInstance
      """
      
      // Arbitrary definition
      val arbName = Pat.fresh(tpeName.toString + "Arb")
      val arbTpe = t"Arbitrary[..$newTpeArgs]"
      val argGen = arg"${genName.name}"
      val createArb = ctor"Arbitrary($argGen)"
      val lazyArb = q"implicit lazy val $arbName : $arbTpe = $createArb"

      // Add to the map of types
      impureTypes(tpe.toString) = tpeName

      // Add the definitions to the buffer
      impureTypesDefs.append(caseClass, valGen, lazyArb)
      
      return tpeName 
    }

    def handleTpFunction(tpe: Type.Function) : Type.Name = {
      impureTypes.get(tpe.toString) match {
        // Already subtyped so we can serve it back
        case Some(tpName : Type.Name) => tpName
        // Not already subtyped so we have to create it
        case None => createImpureSubtype(tpe)
      }
    }

    def bind(
      paramName: Term.Param.Name,
      tpName : Type.Name
    ): Term.Name => (Term.Param, Seq[Stat]) = {
      val newParam = param"$paramName : ${Some(tpName)}"
      val paramAsTerm : Term  = Term.Name(paramName.toString)
      val bindName = q"$paramAsTerm.$effectName = ${paramName.toString}"
      def bindAll(actualBuffer: Term.Name) : (Term.Param, Seq[Stat]) = {
        val bindBuffer = q"$paramAsTerm.$bufferName = $actualBuffer"
        (newParam, scala.collection.immutable.Seq(bindName, bindBuffer))
      }
      bindAll
    }

    /**
     * @param currentNames name of parameters that have impurity
     * @param left remaining parameters to process
     * @param newDefinitions binding between buffers and instances
     *
     * function that will go through the list of parameters to 
     * create all combination of impure functions
     */
    def createSubPossibilities(
      currentNames: Seq[String],
      left: Seq[Term.Param],
      newDefinitions: Seq[(Term.Name => (Term.Param,Seq[Stat]))]
      ): Stream[(Seq[String], Seq[(Term.Name => (Term.Param, Seq[Stat]))])] = {
      
      def builder(
        tpe: Type.Function,
        param: Term.Param,
        currentNames: Seq[String],
        leftAfter: Seq[Term.Param],
        newDefinitions: Seq[(Term.Name => (Term.Param,Seq[Stat]))]
      ): Stream[(Seq[String], Seq[(Term.Name => (Term.Param, Seq[Stat]))])] = {
        val tpName= handleTpFunction(tpe)
        val defs = bind(param.name, tpName)
        def unchanged(n: Term.Name) : (Term.Param, Seq[Stat]) = (param, Nil)
        createSubPossibilities(
          currentNames,
          leftAfter,
          newDefinitions :+ (unchanged _)
        ) #::: 
        createSubPossibilities(           
          currentNames :+ param.name.toString,
          leftAfter,
          newDefinitions :+ defs
        )
      }

      left match {
        case Nil => {
          currentNames match {
            case Nil => Stream.empty
            case _ => Stream((currentNames, newDefinitions))
          }
        }
        case current :: leftAfter => 
          current.decltpe match {
          case Some(tpe: Type.Function) =>
            builder(
              tpe,
              current,
              currentNames,
              leftAfter,
              newDefinitions
            )
          case Some(Type.Apply(tpe, args))
            if findFunction(tpe.toString).matches => {
              args match {
                case (argTpes :+ retTpe) => 
                  builder(
                    Type.Function(argTpes, retTpe),
                    current,
                    currentNames,
                    leftAfter,
                    newDefinitions
                  )
              }
          }
          case _ =>
            def unchanged(n: Term.Name): (Term.Param, Seq[Stat]) = 
              (current, Nil)              
            createSubPossibilities(
              currentNames,
              leftAfter,
              newDefinitions :+ (unchanged _))
        }
      }
    }

    def createImpurePossibilities(params : Seq[Term.Param]):
      Stream[(Seq[String], Seq[(Term.Name => (Term.Param, Seq[Stat]))])] = {
        createSubPossibilities(Nil, params, Nil)
    }
    
    // deconstruction of the object into its syntax tree
    val q"object $name extends { ..$parentClass } with ..$traits { ..$stats }" = defn

    // creation of all tests
    val (
      statsValidity,
      statsEfficiency,
      statsFunctionImpurity,
      statsGeneralImpurity
    ): (Seq[Stat],Seq[Stat], Seq[Stat], Seq[Stat]) = 
      stats.map({
      case rule @ q"""..$mods def $name[..$tparams](...$paramss): $tpe = 
        Rewrite(${left: Term}, ${right: Term})""" =>
          // parameters of the rule
          val params = paramss.head

          val correctness = q"""
            property(${name.toString + "Correctness"}) = forAll {
              (..$params) => {
                $left =? $right  
              }
            }
          """

          val thirdEfficiency = q"""
            property(${name.toString + "Efficiency"}) = {
              val nTries = 100
              val improvements = (0 to nTries).map(i => {
                ..${generateSpeedTest(params, left, right)}
              })
              val average = improvements.sum / improvements.length
              val sorted = improvements.sorted
              val median = sorted(nTries/2)
              val sd = Math.sqrt((sorted.map(x => (x - average)*(x - average)).sum)/(1 + nTries).toDouble)
              val worst = sorted.head
              val best = sorted.last
              collect(
                "Best: " + "%.3f".format(best),
                " worst: " +"%.3f".format(worst),
                " avg: " + "%.3f".format(average),
                " median: " + "%.3f".format(median),
                " Stddev: " + "%.3f".format(sd))(true)
            }
          """
          
          val otherEfficiency = q"""
            property(${name.toString + "Efficiency"}) = {
              val collectSpeed = 
                forAll {
                  (..$params) => {
                    val t0 = System.nanoTime()
                    val left = $left
                    val t1 = System.nanoTime()
                    val speedOriginal = t1 - t0
                    val right = $right
                    val t2 = System.nanoTime()
                    val speedSubstituing = t2 - t1
                    collect(speedOriginal, speedSubstituing)(true)
                  }
                }

              val nTries = 100
              val speeds = for (i <- 0 to nTries)
              yield collectSpeed(Gen.Parameters.default).collected.head
              val improvements = speeds.map {
                case result => 
                  val castedResult = result.asInstanceOf[(Long, Long)]
                  (castedResult._2).toDouble / castedResult._1.toDouble
              }
              val average = improvements.sum / improvements.length
              val sorted = improvements.sorted
              val median = sorted(nTries/2)
              val sd = Math.sqrt((sorted.map(x => (x - average)*(x - average)).sum)/nTries.toDouble)
              val worst = sorted.head
              val best = sorted.last
              collect("Best: " + best, " worst: " + worst, " avg: " + average," median: " + median, " Stddev: " + sd)(true)
            }
          """

          val efficiency = q"""
            property(${name.toString + "Efficiency"}) = forAll {
              (..$params) => {           
                val t0 = System.nanoTime()
                val left = $left
                val t1 = System.nanoTime()
                val right = $right
                val t2 = System.nanoTime()
                val test = {
                  if ((t1 - t0) > (t2 - t1)) "faster"
                  else if ((t1 - t0) == (t2 - t1)) "equivalent"
                  else "slower"
                }
                collect(test)(true)
              }
            }
          """

          val impureTestDefs = createImpurePossibilities(params)
 
          val functionImpurities = (impureTestDefs.flatMap {
            case (names, defs) => {
              val bufferName = Pat.fresh("buffer")
              val bufferDecl = q"""
                var $bufferName = _root_.scala.collection.mutable.ListBuffer.empty[String]
              """
              val (args, seqDefs) = defs.map(_(bufferName.name)).unzip
              val allDefs = seqDefs.flatten
              val impureTest = q"""
                property(${name.toString + "{" + names.mkString("+")  + "}Impures"}) = 
                  forAll {
                    (..$args) =>
                      { ..$allDefs }                  
                      ${bufferName.name}.clear                                  
                      val left = $left
                      val leftInfos = ${bufferName.name}.toList
                      ${bufferName.name}.clear                                  
                      val right = $right
                      val rightInfos = ${bufferName.name}.toList
                      leftInfos =? rightInfos
                  }
              """
              List(bufferDecl, impureTest)
            }
          })

          val userDefinedImpurity = q"""
            property(${name.toString + "ImpureGeneral"}) = 
              forAll { 
                (..$params) => 
                  this.cleanUp()
                  val left = $left
                  this.commitLeft()
                  val right = $right
                  this.commitRight()
                  this.checkEffects()
              }        
            """
          
          GeneratedTest(
            correctness,
            thirdEfficiency,
            functionImpurities,
            userDefinedImpurity
          )
      case other =>
        EmptyTests
    }).foldLeft((Seq[Stat](), Seq[Stat](), Seq[Stat](), Seq[Stat]()))(aggregateTest)

    val correctness: Type.Name = Type.Name("Correctness")
    val efficiency: Type.Name = Type.Name("Efficiency")
    val functionImpurity: Type.Name = Type.Name("FunctionSideEffects")
    val generalImpurity: Type.Name = Type.Name("GeneralSideEffects")

    val statsWithTest = stats ++ 
      List(q"import org.scalacheck._",
           q"import Arbitrary._",
           q"import Gen._",
           q"import Prop._",
           q"""class $correctness extends Properties(${name.toString + "Correctness"})
              { ..$statsValidity }
           """,
           q"""class $efficiency extends Properties(${name.toString + "Efficiency"})
              { ..$statsEfficiency }
           """,
           q"""class $functionImpurity extends Properties(${name.toString + "FunctionSideEffects"})
              { ..${Seq(impureTypesDefs:_*) ++ statsFunctionImpurity.toVector} }
           """,
           q"""abstract class $generalImpurity extends Properties(${name.toString + "GeneralSideEffects"}) with TestFunctions
              { ..$statsGeneralImpurity }
           """
           )
 
    q"object $name { ..$statsWithTest }"
  }
}
