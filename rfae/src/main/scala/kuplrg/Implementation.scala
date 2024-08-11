package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = expr match
    case Num(n)  => NumV(n)
    case Bool(b) => BoolV(b)
    case Id(x)   => env.getOrElse(x, error("free identifier"))
    case Add(e1, e2) =>
      (interp(e1, env), interp(e2, env)) match
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _                    => error("invalid operation")
    case Mul(e1, e2) =>
      (interp(e1, env), interp(e2, env)) match
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _                    => error("invalid operation")
    case Div(e1, e2) =>
      (interp(e1, env), interp(e2, env)) match
        case (NumV(n1), NumV(n2)) =>
          if n2 != 0 then NumV(n1 / n2)
          else error("invalid operation")
        case _ => error("invalid operation")
    case Mod(e1, e2) =>
      (interp(e1, env), interp(e2, env)) match
        case (NumV(n1), NumV(n2)) =>
          if n2 != 0 then NumV(n1 mod n2)
          else error("invalid operation")
        case _ => error("invalid operation")
    case Eq(e1, e2) =>
      (interp(e1, env), interp(e2, env)) match
        case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
        case _                    => error("invalid operation")
    case Lt(e1, e2) =>
      (interp(e1, env), interp(e2, env)) match
        case (NumV(n1), NumV(n2)) => BoolV(n1 < n2)
        case _                    => error("invalid operation")
    case Fun(x, e) => CloV(x, e, () => env)
    case Rec(x0, x1, e0, e1) =>
      lazy val newEnv: Env = env + (x0 -> CloV(x1, e0, () => newEnv))
      interp(e1, newEnv)
    case App(e0, e1) =>
      interp(e0, env) match
        case CloV(x, e2, fenv) =>
          interp(e2, fenv() + (x -> interp(e1, env)))
        case _ =>
          error("not a function")
    case If(e0, e1, e2) =>
      interp(e0, env) match
        case BoolV(true)  => interp(e1, env)
        case BoolV(false) => interp(e2, env)
        case _            => error("not a boolean")
}
