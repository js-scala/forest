package forest.backends

/**
 * Forest expression language operations
 * 
 * Expressions are objects which can be tested (in “if” statements), iterated (in “for” statement), represented,
 * and which can contain fields (which are expressions too)
 */
trait ExpOps {
  /** Expression in the expression language */
  type Exp
  
  /**
   * @return an collection of objects from 'exp (used in “for” statements)
   */
  def _iterate(exp: Exp): Iterable[Exp]
  
  /**
   * @return a boolean interpretation of 'exp (used in “if” statements)
   */
  def _test(exp: Exp): Boolean
  
  /**
   * @return a textual representation of the value of 'exp
   */
  def _show(exp: Exp): String
  
  /**
   * @return the expression represented by the field 'field in 'exp (used to evaluate “foo.bar”)
   */
  def _get(exp: Exp, field: String): Exp
}