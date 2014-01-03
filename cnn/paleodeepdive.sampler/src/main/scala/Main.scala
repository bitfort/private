package org.hazy.gibbs

import java.io.File

object GibbsSampler extends App {

  // Parse command line arguments
  case class Config(variablesFile: File, factorsFile: File, weightsFile: File, 
    numLearningEpoch: Int, numInfEpoch: Int, numLearningSamplePerEpoch: Int, 
    numThreads: Int, outputFile: File, alpha:Double, diminish:Double, mu:Double)

  val parser = new scopt.OptionParser[Config]("gibbs_sampler") {
    head("gibbs_sampler", "")
    opt[File]('v', "variables") required() valueName("<file>") action { (x, c) =>
      c.copy(variablesFile = x) } text("variables file (required)")
    opt[File]('f', "factors") required() valueName("<file>") action { (x, c) =>
      c.copy(factorsFile = x) } text("factors file (required) ")
    opt[File]('w', "weights") required() valueName("<file>") action { (x, c) =>
      c.copy(weightsFile = x) } text("factors file (required)")
    opt[File]('o', "output") required() valueName("<file>") action { (x, c) =>
      c.copy(outputFile = x) } text("output file (required)")

    opt[Int]('l', "learning_epochs") required() action { (x, c) =>
      c.copy(numLearningEpoch = x) } text("number of epochs for learning (required)")

    opt[Int]('i', "inference_epochs") required() action { (x, c) =>
      c.copy(numInfEpoch = x) } text("number of epochs for inference (required)")

    opt[Int]('s', "learning_samples_per_epoch") required() action { (x, c) =>
      c.copy(numLearningSamplePerEpoch = x) } text("number of samples for learning per epoch")

    opt[Int]('t', "threads") required() action { (x, c) =>
      c.copy(numThreads = x) } text("number of threads (required)")


    opt[Double]('a', "alpha") action { (x, c) =>
      c.copy(alpha = x) } text("alpha")

    opt[Double]('d', "diminish") action { (x, c) =>
      c.copy(diminish = x) } text("diminish")

    opt[Double]('m', "mu") action { (x, c) =>
      c.copy(mu = x) } text("mu")

  }

  val initialConfig = Config(null, null, null, 100, 10, 1000, 1, null, 0.1, 0.96, 0.01)
  
  parser.parse(args, initialConfig) map { config =>
    val fm = DBParser.parse(config.weightsFile.getCanonicalPath, 
      config.variablesFile.getCanonicalPath,
      config.factorsFile.getCanonicalPath)

    println("[SAMPLER PARAMETER] alpha    =" + config.alpha)
    println("[SAMPLER PARAMETER] diminish =" + config.diminish)
    println("[SAMPLER PARAMETER] mu       =" + config.mu)


    fm.cnnLearningFunction(config.numLearningEpoch, config.numLearningSamplePerEpoch, 
      config.outputFile.getCanonicalPath + ".weights", config.numThreads, config.alpha, config.diminish, config.mu)


    /*
    fm.learningFunction(config.numLearningEpoch, config.numLearningSamplePerEpoch, 
      config.outputFile.getCanonicalPath + ".weights", config.numThreads, config.alpha, config.diminish, config.mu)

    fm.MarginalsFile(config.numInfEpoch, fm.variables, config.numThreads, 
      config.outputFile.getCanonicalPath)
    */

  } getOrElse {
    System.exit(1)
  }

}





