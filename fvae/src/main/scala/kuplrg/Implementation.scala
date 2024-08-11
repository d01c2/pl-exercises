package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  type BOp = (BigInt, BigInt) => BigInt

  def numBOp(x: String)(op: BOp)(left: Value, right: Value): Value =
    (left, right) match
      case (NumV(left), NumV(right)) => NumV(op(left, right))
      case _                         => error("invalid operation")

  val numAdd: (Value, Value) => Value = numBOp("+")(_ + _)
  val numMul: (Value, Value) => Value = numBOp("*")(_ * _)

  def interp(expr: Expr, env: Env): Value =
    expr match
      case Num(number) => NumV(number)
      case Add(left, right) =>
        numAdd(interp(left, env), interp(right, env))
      case Mul(left, right) =>
        numMul(interp(left, env), interp(right, env))
      case Val(name, init, body) =>
        interp(body, env + (name -> interp(init, env)))
      case Id(name)         => env.getOrElse(name, error("free identifier"))
      case Fun(param, body) => CloV(param, body, env)
      case App(fun, arg) =>
        interp(fun, env) match
          case NumV(number) => error("not a function")
          case CloV(param, body, fenv) =>
            interp(body, fenv + (param -> interp(arg, env)))

  def interpDS(expr: Expr, env: Env): Value =
    expr match
      case Num(number) => NumV(number)
      case Add(left, right) =>
        numAdd(interpDS(left, env), interpDS(right, env))
      case Mul(left, right) =>
        numMul(interpDS(left, env), interpDS(right, env))
      case Val(name, init, body) =>
        interpDS(body, env + (name -> interpDS(init, env)))
      case Id(name)         => env.getOrElse(name, error("free identifier"))
      case Fun(param, body) => CloV(param, body, env)
      case App(fun, arg) =>
        interpDS(fun, env) match
          case NumV(number) => error("not a function")
          case CloV(param, body, fenv) =>
            interpDS(body, env + (param -> interpDS(arg, env)))
}
