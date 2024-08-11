package kuplrg

import scala.collection.View.Empty

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  def numAdd(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
      case _                    => error("invalid operation")

  def numMul(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
      case _                    => error("invalid operation")

  def lookupId(x: String, env: Env): Value =
    env.getOrElse(x, error("free identifier"))

  def reduce(k: Cont, s: Stack): (Cont, Stack) =
    (k, s) match
      case (EmptyK, s) => (EmptyK, s)
      case (EvalK(env, expr, k), s) =>
        expr match
          case Num(number) => (k, NumV(number) :: s)
          case Add(left, right) =>
            (EvalK(env, left, EvalK(env, right, AddK(k))), s)
          case Mul(left, right) =>
            (EvalK(env, left, EvalK(env, right, MulK(k))), s)
          case Id(name) =>
            (k, lookupId(name, env) :: s)
          case Fun(param, body) =>
            (k, CloV(param, body, env) :: s)
          case App(fun, arg) =>
            (EvalK(env, fun, EvalK(env, arg, AppK(k))), s)
          case Vcc(name, body) =>
            (EvalK(env + (name -> ContV(k, s)), body, k), s)
      case (AddK(k), l :: r :: s) => (k, numAdd(l, r) :: s)
      case (MulK(k), l :: r :: s) => (k, numMul(l, r) :: s)
      case (AppK(k), v :: f :: s) =>
        f match
          case CloV(p, b, e) => (EvalK(e + (p -> v), b, k), s)
          case ContV(nk, ns) => (nk, v :: ns)
          case _             => error("not a function")
      case _ => error("invalid operation")
}
