package scala.spores

protected object Feedback {
  val IncorrectSporeHeader =
    "Incorrect spore header: Only val defs allowed at this position."

  val IncorrectSporeBody =
    "Incorrect spore body: expected function literal or `delayed`."

  val InvalidOuterReference =
    "Only stable paths can be captured inside a spore."

  val InvalidLazyMember =
    "The path of a captured variable inside a spore cannot contain lazy members."
}
