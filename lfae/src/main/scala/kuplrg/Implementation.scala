package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value =
    
    def strict(v: Value): Value =
      v match
        case ExprV(expr, env) => strict(interp(expr, env))
        case _ => v
      
    expr match
      case Num(number) => NumV(number)
      case Add(left, right) => 
        (strict(interp(left, env)), strict(interp(right, env))) match
          case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
          case _ => error("invalid operation")
      case Mul(left, right) =>
        (strict(interp(left, env)), strict(interp(right, env))) match
          case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
          case _ => error("invalid operation")
      case Id(name) => env.getOrElse(name, error("free identifier"))
      case Fun(param, body) => CloV(param, body, env)
      case App(fun, arg) =>
        strict(interp(fun, env)) match
          case CloV(x, e, nenv) => interp(e, nenv + (x -> ExprV(arg, env)))
          case _ => error("not a function")
}
