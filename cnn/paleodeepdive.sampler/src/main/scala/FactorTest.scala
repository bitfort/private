package org.hazy.gibbs

import scala.collection.immutable.{ Map, List }
import scala.collection.mutable._
import scala.util.Random
import scala.math.{ abs, exp, log }
import scala.io.Source
import java.io._

//
// Utils
//
object Util {
  def checkAndAddSet[K, V](ht: HashMap[K, Set[V]], k: K, v: V) {
    if (!(ht contains k)) { ht(k) = new HashSet[V]() }
    ht(k).add(v)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
// ***************************************************
// There are three major parts of factor graphs
// Variables, Factors, and Weights.
// These are the classes.

// TODO: Template the variable types
class Variable(id0: Int, value0: Boolean, isevid0: Boolean) {
  var _value = value0
  def value: Boolean = return _value
  def id = id0
  //def setTrue() = synchronized { _value = true }
  //def setFalse() = synchronized { _value = false }
  def setTrue() = { _value = true }
  def setFalse() = { _value = false }

  def isevid = isevid0

  override def toString() =
    "" + id + "->" + _value
}

//
// Weights may be tied together across several factors.
//
class Weight(id: Int, w0: Double) {
  def id0 = id
  var w = w0 // this is a log-scale weight
}

// 
// the factor returns the weight if any variable is true 
// 
abstract class Factor(id0: Int, vars0: Array[Variable], w0: Weight, funcid0: String) {
  def id = id0
  def vars = vars0
  def funcID = funcid0
  def w = w0 // this could be tied
  /* Note variable weights are different from weights */
  def WeightVariable(u: Variable): (Double, Double)
  def EvaluateFactor(): Boolean /* evaluate this factor */
}

// 
// Several simple types of Factors
//
class OrFactor(id0 : Int, vars0: Array[Variable], w0 : Weight, funcid0: String) extends Factor(id0 : Int, vars0: Array[Variable], w0 : Weight, funcid0: String) { 
  override def WeightVariable(u : Variable) : (Double,Double) = { 
    // If everythign aside from u is false, then u is influential
    if (vars.filter(x => x != u).forall(v => !v.value)) { 
      return (w.w,0.0)
    }
    return (0.0,0.0)
  }
  override def EvaluateFactor() : Boolean = { return vars.exists(v => v.value) }
}

class AndFactor(id0 : Int, vars0: Array[Variable], w0 : Weight, funcid0: String) extends Factor(id0 : Int, vars0: Array[Variable], w0 : Weight, funcid0: String) { 
  override def WeightVariable(u : Variable) : (Double,Double) = { 
    // if everything but u is true, then u is influential.
    if (vars.filter(x => x != u).forall(v => v.value)) { 
      return (w.w,0.0)
    }
    return (0.0,0.0)
  }
  override def EvaluateFactor() : Boolean = { return vars.forall(v => v.value) }
}

class ImplyFactor(id0 : Int, vars0: Array[Variable], w0 : Weight, funcid0: String) extends Factor(id0 : Int, vars0: Array[Variable], w0 : Weight, funcid0: String) {
  var imps   = vars0.slice(0,vars.size - 1)
  var output = vars0(vars.size - 1)

  override def WeightVariable(u : Variable) : (Double,Double) = { 

    val n = vars.size
    if(n==1){
      return (w.w,0.0)
    }else{
      // if either the body is false or the output ...
      if (!imps.filter(x => x != u).forall(v => v.value) || ((output != u) && output.value)) { 
        //println("0.0 0.0")
        return (0.0,0.0)
      } else { 
        if( output == u ) { 
         //println(w.w + "0.0")
	       return (w.w,0.0)
        } else { 
          //println("0.0 " + w.w)
	       return (0.0,w.w)
        }
      }
    }
  } 
  override def EvaluateFactor() : Boolean = { 
    val n = vars.size
    if(n==1){
      return vars(n-1).value
    }else{
      return vars.take(n - 1).forall(v => !v.value) || vars(n-1).value
    }
  }
}

// ****************************************************
// A Ce factor subsumes the above hardcoded factors. 
// A Ce factor is an Orfactor but allows Not gates (signs) for each input.
class CeFactor(id0: Int, vars0: Array[Variable], w0: Weight, signs0: Map[Variable, Boolean], funcid0: String) extends Factor(id0: Int, vars0: Array[Variable], w0: Weight, funcid0: String) {
  def signs = signs0

  override def WeightVariable(u: Variable): (Double, Double) = {
    //synchronized {
      // If everything aside from u is false (after signing), then u is influential
      if (vars.filter(x => x != u).forall(v => !(v.value ^ signs(v)))) {
        return if (signs(u)) (0.0, w.w) else (w.w, 0.0)
      }
      return (0.0, 0.0)
    //}
  }
  override def EvaluateFactor(): Boolean = { return vars.exists(v => v.value ^ signs(v)) }
}


// 
// A FactorManager keeps track of the associations between variables, factors, and weights.
// It is also where we put in samplers for now
class FactorManager {
  val factorMap = new HashMap[Variable, Set[Factor]]
  val weightMap = new HashMap[Weight, Set[Factor]]
  val variables = new HashSet[Variable]
  val evids     = new HashMap[Variable, Boolean]
  def r = Random

  def registerVar(v: Variable, f: Factor) {
    if (!(variables contains v)) {
      variables.add(v)
      factorMap(v) = new HashSet[Factor]()
    }
    factorMap(v).add(f)
  }

  def registerWeight(f: Factor) {
    if (!(weightMap contains f.w)) {
      weightMap(f.w) = new HashSet[Factor]()
    }
    weightMap(f.w).add(f)
  }

  def addFactor(f: Factor) {
    for (v <- f.vars) {
      registerVar(v, f)
      registerWeight(f)
    }
  }

  // This is the individual uncollapsed Gibbs sampler code.
  def sampleVariable(v: Variable) {
    var (pos: Set[Double], neg: Set[Double]) = factorMap(v).map(_.WeightVariable(v)).unzip
    var pos0 = pos.foldLeft(0.0)(_ + _)
    var neg0 = neg.foldLeft(0.0)(_ + _)
    var s = r.nextDouble()

    // if( s*(exp(pos0)+exp(neg0)) <= exp(pos0) ) { v.setTrue() } else { v.setFalse() }
    if (s * (1 + exp(neg0 - pos0)) <= 1.0) { v.setTrue() } else { v.setFalse() }
  }

  // Sample all known variables
  def sampleVariables() { for (v <- r.shuffle(variables.toList)) { sampleVariable(v) } }

  // Sample variables that are in the mask
  def sampleVariablesMask(mask: Set[Variable]) {
    for (v <- r.shuffle(variables.toList.filter(u => mask contains u))) {
      sampleVariable(v)
    }
  }

  // Parallel sampling
  def sampleVariablesMaskParallel(masks: Array[MaskVariable], nThreads: Int) {
    var t = new Array[Thread](nThreads)
    // For all threads, set them up
    for (i <- 0 until nThreads) {
      t(i) = new Thread(new Sample(this, masks(i).variables, i))
      t(i).start()
    }

    // Joins all the threads
    for (i <- 0 until nThreads) { t(i).join() }
    
    // Merges the result
    for (i <- 0 until nThreads) {
      t(i) = new Thread(new MergeResult(this, masks(i).variables, i))
      t(i).start()
    }
    
    // Joins all the threads
    for (i <- 0 until nThreads) { t(i).join() }

  }

  /* Calculates the probability that a variable is true */
  def marginals(nSamples: Int, mask: Set[Variable]): List[(Variable, Double)] = {
    var trueCount = new HashMap[Variable, Int]()
    for (v <- variables) { trueCount(v) = 0 }

    for (i <- 0 until nSamples) {
      sampleVariablesMask(mask)

      for (v <- variables) {
        if (v.value) { trueCount(v) += 1 }
      }
    }
    return trueCount.toList.map(x => (x._1, x._2 / nSamples.toDouble))
  }

  def histogram(nSamples: Int, mask: Set[Variable]) {
    var trueCount = new HashMap[Variable, Int]()
    for (v <- variables) { trueCount(v) = 0 }

    for (i <- 0 until nSamples) {
      sampleVariablesMask(mask)

      for (v <- variables) {
        if (v.value) { trueCount(v) += 1 }
      }
    }
    println(trueCount.toList.map(x => (x._1.id, x._2)) + " of " + nSamples)
  }


  //
  // this computes the marginal probability that each factor is true
  // 
  def marginalsFactor(nSamples: Int, mask: Set[Variable], factorSet: scala.collection.immutable.Set[Factor], nThreads: Int): Map[Factor, Double] = {
    var trueCount = new HashMap[Factor, Int]()
    for (f <- factorSet) { trueCount(f) = 0 }


    
    var masks = new Array[MaskVariable](nThreads)
    for (i <- 0 until nThreads) masks(i) = new MaskVariable(Set.empty[Variable])

    var i = 0
    for (v <- r.shuffle(variables.toList.filter(mask contains _))) {
      // allocate v to i % nThreads mask
      masks(i % nThreads).variables.add(v)
      i += 1
    }

    for (i <- 0 until nSamples) {
      
      //sampleVariablesMask(mask)

      sampleVariablesMaskParallel(masks, nThreads)

      for (f <- factorSet) {
        if (f.EvaluateFactor()) { trueCount(f) += 1 }
      }
    }

    return trueCount.toList.map(x => (x._1, x._2 / nSamples.toDouble)).toMap
  }

  // histogram file
  def MarginalsFile(nSamples: Int, mask: Set[Variable], nThreads: Int, szOut: String) {

    var trueCount = new HashMap[Variable, Int]()
    val startTime = System.nanoTime

    var sum_first_20 = new HashMap[Variable, Double]()
    var square_sum_first_20 = new HashMap[Variable, Double]()
    var sum_last_50 = new HashMap[Variable, Double]()
    var square_sum_last_50 = new HashMap[Variable, Double]()

    var NEPOCH_FIRST_20 = nSamples * 0.2
    var NEPOCH_LATS_50  = nSamples * 0.5

    for (v <- variables) { 
      if (!v.isevid){
        trueCount(v) = 0 
        sum_first_20(v) = 0
        //square_sum_first_20(v) = 0
        sum_last_50(v) = 0
        //square_sum_last_50(v) = 0
      }
    }

    var masks = new Array[MaskVariable](nThreads)
    for (i <- 0 until nThreads) masks(i) = new MaskVariable(Set.empty[Variable])

    // Partition variables into different masks
    var i = 0
    for (v <- r.shuffle(variables.toList.filter(mask contains _))) {
      // allocate v to i % nThreads mask
      if(!v.isevid){
        masks(i % nThreads).variables.add(v)
        i += 1
      }
    }

    val time1 = System.nanoTime
    println("partition " + (time1 - startTime) / 1e9)

    for (i <- 0 until nSamples) {

      sampleVariablesMaskParallel(masks, nThreads)
      
      for (v <- variables) {
        if (!v.isevid){
          if (v.value) { trueCount(v) += 1 }
        }
      }

      if (i < NEPOCH_FIRST_20){
        for(v <- variables){
          if(!v.isevid){
            if(v.value){
              sum_first_20(v) += 1.0
            }
          }
        }
      }

      if (i>NEPOCH_LATS_50){
        for(v <- variables){
          if(!v.isevid){
            if(v.value){
              sum_last_50(v) += 1.0
            }
          }
        }
      }

      if (i % 10 == 1) { print(".") }
    }
    val endTime = System.nanoTime
  
    val time_s = (endTime - startTime) / 1e9

    println("\ntime = " + time_s + " iterations = " + nSamples + " active vars = " + mask.size)
    println("samples/s = " + nSamples / time_s + " tokens/s = " + (nSamples * mask.size) / time_s)

    var mean_first_20 = 0.0
    var var_first_20 = 0.0
    var mean_last_50 = 0.0
    var var_last_50 = 0.0
    var t = 0.0

    var rej_0_95 = 0
    var rej_0_90 = 0
    var nvariable = 0

    for(v <- variables){
      if(!v.isevid){
        mean_first_20 = sum_first_20(v)/NEPOCH_FIRST_20
        mean_last_50 = sum_last_50(v)/NEPOCH_LATS_50
        var_first_20 = mean_first_20 - mean_first_20*mean_first_20
        var_last_50 =  mean_last_50 - mean_last_50*mean_last_50

        if(var_first_20 + var_last_50 == 0){
          t= (mean_first_20 - mean_last_50) * 10000
        }else{
          t = (mean_first_20 - mean_last_50)/(Math.sqrt(var_first_20+var_last_50))
        }

        if (t > 1.96 || t < -1.96){
          rej_0_95 = rej_0_95 + 1
        }


        if (t > 1.65 || t < -1.65 ){
          rej_0_90 = rej_0_90 + 1
        }

        nvariable = nvariable + 1
        //println(t)
      }
    }

    println("[INFERENCE INFO] " + "# NOT_CONVERGE (p=0.95) " + rej_0_95 + "/" + nvariable)
    println("[INFERENCE INFO] " + "# NOT_CONVERGE (p=0.90) " + rej_0_90 + "/" + nvariable)

    val cesFormat = trueCount.toList.map(x => x._1.id + "\t" + x._1.value + "\t" + (x._2 / nSamples.toDouble))
    Util.printToFile(new File(szOut))(p => {
      cesFormat.foreach(p.println)
    })
  }


  def cnnLearningFunction(nIterations : Int, nSamples : Int, szOut : String, nThreads : Int, _alpha : Double, _diminish : Double, _mu : Double) {

    val weights = new HashMap[Weight, HashMap[Int, Double]]

    val gradients = new HashMap[Factor, HashMap[Variable, Double]]
    val var2facs = new HashMap[Variable, Array[Factor]]

    val var2value = new HashMap[Variable, Double]

    // init variable value
    for(variable <- variables){
      var2facs(variable) = Array[Factor]()

        if(variable.value == true){
          var2value(variable) = 1.0
        }else{
          var2value(variable) = 0.0
        }
    }

    // init model and gradient
    val factors = variables.map( factorMap(_).toList ).foldLeft(List[Factor]())(_ union _).toSet
    val ww  = factors.map(_.w).toSet

    for(w <- ww){
      weights(w) = new HashMap[Int, Double]
    }

    for(factor <- factors){
      //weights(factor.w) = new HashMap[Int, Double]
      gradients(factor) = new HashMap[Variable, Double]
      var ct = 0
      for(variable <- factor.vars){
        if (ct != 0){
          var2facs(variable) = var2facs(variable) :+ factor
        }
        ct = ct + 1
        weights(factor.w)(ct) = 0.1
        gradients(factor)(variable) = 0.0
      }
    }

    // sort factors (large id first -- large id :=: large layer number)
    val tasks = factors.toList.sortWith(_.id > _.id)
    val tasks2 = factors.toList.sortWith(_.id < _.id)

    for(iteration <- 0 until nIterations) {

      println("---------------")

      for(factor <- tasks){

        //println(factor.id)
        
        // first, get the gradient to the output of this factor
        var headgrad = 0.0
        var ct = 0
        for(f2 <- var2facs(factor.vars(0))){
          headgrad = headgrad + gradients(f2)(factor.vars(0))
          ct = ct + 1
        }
        if(ct == 0){
          headgrad = 1.0
        }


        // second, scalculate the current Wx
        var current_sum = 0.0
        ct = 0
        for(variable <- factor.vars){
          if (ct != 0){
            current_sum = current_sum + weights(factor.w)(ct)*var2value(variable)
          }
          ct = ct + 1
        }


        // calculate the gradient and update the weight
        ct = 0
        for(variable <- factor.vars){
          if (ct != 0){
            var local_grad = 0.0

            if(factor.funcID == "SOFTMAX"){
              local_grad = - var2value(factor.vars(0))*var2value(variable) + Math.exp(current_sum)/(Math.exp(current_sum) + 1) * var2value(variable)
              //println("~~~~ " + local_grad)

            }
            if(factor.funcID == "SUMTANH"){

              var v1 = (Math.exp(current_sum) -Math.exp(-current_sum))*(Math.exp(current_sum) -Math.exp(-current_sum))
              var v2 = (Math.exp(current_sum) +Math.exp(-current_sum))*(Math.exp(current_sum) +Math.exp(-current_sum))

              local_grad = var2value(variable) * (1 - v1*v1/v2/v2)
            }
          
            //println(local_grad)
            var final_grad = local_grad * headgrad

            gradients(factor)(variable) = final_grad
            weights(factor.w)(ct) = weights(factor.w)(ct) - _alpha * final_grad

          }
          ct = ct + 1
        }
      }

      // forward-propogate
      for(factor <- tasks2){
        
        var current_sum = 0.0
        var ct = 0
        for(variable <- factor.vars){
          if (ct != 0){
            current_sum = current_sum + weights(factor.w)(ct)*var2value(variable)
          }
          ct = ct + 1
        }

        if(factor.vars(0).isevid == false){
          if(factor.funcID == "SOFTMAX"){
            var2value(factor.vars(0)) = Math.exp(current_sum) / (Math.exp(current_sum) + 1) 
          }
          if(factor.funcID == "SUMTANH"){
            var2value(factor.vars(0)) = (1-Math.exp(-2*current_sum))/(1+Math.exp(-2*current_sum))
          }
        }else{
          //println(factor.vars(0).isevid)
          if(factor.funcID == "SOFTMAX"){
            var now = Math.exp(current_sum) / (Math.exp(current_sum) + 1) 
            println("WANT:" + var2value(factor.vars(0)) + "   NOW:" + now)
          }
          if(factor.funcID == "SUMTANH"){
            //println(current_sum)
            var now = (1-Math.exp(-2*current_sum))/(1+Math.exp(-2*current_sum))
            println("WANT:" + var2value(factor.vars(0)) + "   NOW:" + now)
          }
        }
      }
    }

    for(w <- ww){
      println("##### WEIGHT FAMILY " + w.id0)
      for(i <- weights(w)){
        println("     " + i._1 + " : " + i._2)
      }
    }


  }





  // marginal factor
  // learning 
  def learningFunction(nIterations : Int, nSamples : Int, szOut : String, nThreads : Int, _alpha : Double, _diminish : Double, _mu : Double) {

    var alpha = _alpha
    var diminish = _diminish
    var mu = _mu

    // Compute the variabels, the factors that touch the variables, and the query weights
    val query_variables = variables.filterNot( (evids contains _) )
    val evid_variables = variables.filter( (evids contains _) )
    val query_factors   = evid_variables.map( factorMap(_).toList ).foldLeft(List[Factor]())(_ union _).toSet
    val query_weights   = query_factors.map(_.w).toSet

    // The core learning loop.
    for(iteration <- 0 until nIterations) {

      var weightchanges = new HashMap[Weight, Double]
      for(w <- query_weights) { 
        weightchanges(w) = 0.0
      }

      // 1. Compute the marginal without conditioning.
      val f0     = marginalsFactor(nSamples, variables, query_factors, nThreads)
      
      // 2. Condition the factor graph
      for(v <- evids) { 
        if(v._2) { 
          //println("+")
          v._1.setTrue() 
        }else { 
          //println("-")
          v._1.setFalse() 
        } 
      }

      // 3. Compute the conditional probability
      val f1     = marginalsFactor(nSamples, query_variables, query_factors, nThreads)

      // Statistics from the Gibb Sample
      
      // Compute the difference in the expectation for each factor.
      for(f <- query_factors) { 
        //println("COND: " + f1(f) + "   FREE: " + f0(f) + "   WEIGHT: " + f.w.w)
        f.w.w += alpha*(f1(f) - f0(f)) 
        weightchanges(f.w) += (f1(f) - f0(f))
      }

      var normgrad = 0.0
      var maxgrad = 0.0
      for(w <- query_weights) { 
        normgrad += weightchanges(w)*weightchanges(w)
        if( Math.abs(weightchanges(w)) > maxgrad ){
          maxgrad = Math.abs(weightchanges(w))
        }
      }
      normgrad = math.sqrt(normgrad)
      println("[LEARNING INFO] EPOCH " + iteration + " norm (l_2) =" + normgrad + " norm (l_inf) =" + maxgrad )

      //println("-------------")
      
      // Reguarlize the active weights using \ell_2 prox
      for(w <- query_weights) { w.w *= 1.0/(1.0+mu*alpha) }
      
      // end of iteration book keeping, diminish the step rule
      alpha *= diminish
      
    }

    for(v <- evids) { if(v._2) { v._1.setTrue() } else { v._1.setFalse() } }

    //for(w <- query_weights) { 
    //   println(w.id0 + " -> " + w.w)
    //}

    Util.printToFile(new File(szOut))(p => {
      query_weights.foreach(w => p.println(w.id0 + " " + w.w))
    })

  }

}

class Sample(fm: FactorManager, mask: Set[Variable], id0: Int) extends Runnable {
  def id = id0
  def r = Random

  def run() {
    for (v <- mask) {
      var (pos: Set[Double], neg: Set[Double]) = fm.factorMap(v).map(_.WeightVariable(v)).unzip
      var pos0 = pos.foldLeft(0.0)(_ + _)
      var neg0 = neg.foldLeft(0.0)(_ + _)
      var s = r.nextDouble()
      if (s * (1 + exp(neg0 - pos0)) <= 1.0) { v.setTrue() } else { v.setFalse() }
    }
    Thread.`yield`()
  }
}

class MergeResult(fm: FactorManager, mask: Set[Variable], id0:Int) extends Runnable {
  def run() {
    fm.variables ++= mask
    Thread.`yield`()
  }
}

class MaskVariable(v0: Set[Variable]) {
  def variables = v0
}

object CeParser {

  // *****************************************
  // * Helper functions to Parse Ce's format *
  // *****************************************
  def parseWeight(szWeightName: String): Map[Int, Weight] = {
    return Source.fromFile(szWeightName).getLines().map(_.split("\t")).map(
      v =>
        // [WEIGHT ID] [VALUE] [IS_FIXED_WEIGHT]
        // new Weight(v(0).toInt, v(1).toDouble, v(2).toBoolean)
        (v(0).toInt, new Weight(v(0).toInt, v(1).toDouble))).toMap
  }
}
