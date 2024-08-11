package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type =
    expr match
      case Num(number) => NumT
      case Add(left, right) =>
        (typeCheck(left, tenv), typeCheck(right, tenv)) match
          case (NumT, NumT) => NumT
          case _ => error()
      case Mul(left, right) =>
        (typeCheck(left, tenv), typeCheck(right, tenv)) match
          case (NumT, NumT) => NumT
          case _ => error()
      case Val(name, init, body) =>
        typeCheck(body, tenv + (name -> typeCheck(init, tenv)))
      case Id(name) =>
        tenv.getOrElse(name, error())
      case Fun(param, ty, body) =>
        ArrowT(ty, typeCheck(body, tenv + (param -> ty)))
      case App(fun, arg) =>
        typeCheck(fun, tenv) match
          case ArrowT(paramTy, bodyTy) =>
            if (typeCheck(arg, tenv) == paramTy) bodyTy
            else error()
          case _ => error()

  def interp(expr: Expr, env: Env): Value = 
    expr match
      case Num(number) => NumV(number)
      case Add(left, right) =>
        (interp(left, env), interp(right, env)) match
          case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
          case _ => error()
      case Mul(left, right) =>
        (interp(left, env), interp(right, env)) match
          case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
          case _ => error()
      case Val(name, init, body) =>
        interp(body, env + (name -> interp(init, env)))
      case Id(name) =>
        env.getOrElse(name, error())
      case Fun(param, ty, body) => CloV(param, body, env)
      case App(fun, arg) =>
        interp(fun, env) match
          case CloV(p, b, e) => 
            interp(b, e + (p -> interp(arg, env)))
          case _ => error()
}
