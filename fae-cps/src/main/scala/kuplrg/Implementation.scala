package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  def numAdd(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
      case _ => error("invalid operation")

  def numMul(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
      case _ => error("invalid operation")

  def lookupId(x: String, env: Env): Value =
    env.getOrElse(x, error("free identifier"))

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value = 
    expr match
      case Num(number) => k(NumV(number))
      case Add(left, right) => interpCPS(left, env, {
        lv => interpCPS(right, env, {
          rv => k(numAdd(lv, rv))
        })
      })
      case Mul(left, right) => interpCPS(left, env, {
        lv => interpCPS(right, env, {
          rv => k(numMul(lv, rv))
        })
      })
      case Id(name) => k(lookupId(name, env))
      case Fun(param, body) => k(CloV(param, body, env))
      case App(fun, arg) => interpCPS(fun, env, {
        f => f match
          case CloV(p, b, e) =>
            interpCPS(arg, env, {
              a => interpCPS(b, e + (p -> a), k)
            })
          case _ => error("not a function")
      })

  def reduce(k: Cont, s: Stack): (Cont, Stack) = 
    (k, s) match
      case (EmptyK, s) => (EmptyK, s)
      case (EvalK(env, expr, k), s) => 
        expr match
          case Num(number) => (k, NumV(number) :: s)
          case Add(left, right) => (EvalK(env, left, EvalK(env, right, AddK(k))), s)
          case Mul(left, right) => (EvalK(env, left, EvalK(env, right, MulK(k))), s)
          case Id(name) => (k, lookupId(name, env) :: s)
          case Fun(param, body) => (k, CloV(param, body, env) :: s)
          case App(fun, arg) => (EvalK(env, fun, EvalK(env, arg, AppK(k))), s)
      case (AddK(k), l :: r :: s) => (k, numAdd(l, r) :: s)
      case (MulK(k), l :: r :: s) => (k, numMul(l, r) :: s)
      case (AppK(k), v :: f :: s) => 
        f match
          case CloV(p, b, e) => (EvalK(e + (p -> v), b, k), s)
          case _ => error("not a function")
}
