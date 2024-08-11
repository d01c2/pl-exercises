package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n) => NumV(n)
    case Add(e1, e2) => 
      (interp(e1, env), interp(e2, env)) match
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _ => error("invalid operation")
    case Mul(e1, e2) =>
      (interp(e1, env), interp(e2, env)) match
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ => error("invalid operation")
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case Fun(x, e) => CloV(x, e, env)
    case App(e0, e1) =>
      interp(e0, env) match
        case NumV(n) => error("not a function")
        case CloV(x, e2, fenv) =>
          interp(e2, fenv + (x -> interp(e1, env)))

  def interpDS(expr: Expr, env: Env): Value = expr match
    case Num(n) => NumV(n)
    case Add(e1, e2) => 
      (interpDS(e1, env), interpDS(e2, env)) match
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _ => error("invalid operation")
    case Mul(e1, e2) =>
      (interpDS(e1, env), interpDS(e2, env)) match
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ => error("invalid operation")
    case Id(x) => env.getOrElse(x, error("free identifier"))
    case Fun(x, e) => CloV(x, e, env)
    case App(e0, e1) =>
      interpDS(e0, env) match
        case NumV(n) => error("not a function")
        case CloV(x, e2, fenv) =>
          interpDS(e2, env + (x -> interp(e1, env)))
}
