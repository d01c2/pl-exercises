package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  /* Helper Functions */
  def mustSame(lty: Type, rty: Type): Unit =
    if (lty != rty) error("type mismatch")

  def numAdd(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
      case _                    => error("invalid operation")

  def numMul(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
      case _                    => error("invalid operation")

  def numDiv(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) =>
        if (n2 != 0) NumV(n1 / n2)
        else error("invalid operation")
      case _ => error("invalid operation")

  def numMod(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) =>
        if (n2 != 0) NumV(n1 mod n2)
        else error("invalid operation")
      case _ => error("invalid operation")

  def numEq(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
      case _                    => error("invalid operation")

  def numLt(lv: Value, rv: Value): Value =
    (lv, rv) match
      case (NumV(n1), NumV(n2)) => BoolV(n1 < n2)
      case _                    => error("invalid operation")

  /* Implementation */
  def typeCheck(expr: Expr, tenv: TypeEnv): Type =
    expr match
      case Num(number) => NumT
      case Bool(bool)  => BoolT
      case Add(left, right) =>
        mustSame(typeCheck(left, tenv), NumT)
        mustSame(typeCheck(right, tenv), NumT)
        NumT
      case Mul(left, right) =>
        mustSame(typeCheck(left, tenv), NumT)
        mustSame(typeCheck(right, tenv), NumT)
        NumT
      case Div(left, right) =>
        mustSame(typeCheck(left, tenv), NumT)
        mustSame(typeCheck(right, tenv), NumT)
        NumT
      case Mod(left, right) =>
        mustSame(typeCheck(left, tenv), NumT)
        mustSame(typeCheck(right, tenv), NumT)
        NumT
      case Eq(left, right) =>
        mustSame(typeCheck(left, tenv), NumT)
        mustSame(typeCheck(right, tenv), NumT)
        BoolT
      case Lt(left, right) =>
        mustSame(typeCheck(left, tenv), NumT)
        mustSame(typeCheck(right, tenv), NumT)
        BoolT
      case Val(name, init, body) =>
        val initTy: Type = typeCheck(init, tenv)
        val bodyTy: Type = typeCheck(body, tenv + (name -> initTy))
        bodyTy
      case Id(name) =>
        tenv.getOrElse(name, error())
      case Fun(p, pty, body) =>
        val nty: Type = typeCheck(body, tenv + (p -> pty))
        ArrowT(pty, nty)
      case Rec(x, p, pty, rty, body, scope) =>
        mustSame(
          typeCheck(body, tenv + (x -> ArrowT(pty, rty)) + (p -> pty)),
          rty
        )
        val scopeTy: Type = typeCheck(scope, tenv + (x -> ArrowT(pty, rty)))
        scopeTy
      case App(fun, arg) =>
        typeCheck(fun, tenv) match
          case ArrowT(paramTy, retTy) =>
            mustSame(typeCheck(arg, tenv), paramTy)
            retTy
          case _ => error()
      case If(cond, thenExpr, elseExpr) =>
        mustSame(typeCheck(cond, tenv), BoolT)
        mustSame(typeCheck(thenExpr, tenv), typeCheck(elseExpr, tenv))
        typeCheck(thenExpr, tenv)

  def interp(expr: Expr, env: Env): Value =
    expr match
      case Num(number)      => NumV(number)
      case Bool(bool)       => BoolV(bool)
      case Add(left, right) => numAdd(interp(left, env), interp(right, env))
      case Mul(left, right) => numMul(interp(left, env), interp(right, env))
      case Div(left, right) => numDiv(interp(left, env), interp(right, env))
      case Mod(left, right) => numMod(interp(left, env), interp(right, env))
      case Eq(left, right)  => numEq(interp(left, env), interp(right, env))
      case Lt(left, right)  => numLt(interp(left, env), interp(right, env))
      case Val(name, init, body) =>
        interp(body, env + (name -> interp(init, env)))
      case Id(name)          => env.getOrElse(name, error())
      case Fun(p, pty, body) => CloV(p, body, () => env)
      case Rec(x, p, pty, rty, body, scope) =>
        lazy val nenv: Env = env + (x -> CloV(p, body, () => nenv))
        interp(scope, nenv)
      case App(fun, arg) =>
        interp(fun, env) match
          case CloV(x, e, nenv) => interp(e, nenv() + (x -> interp(arg, env)))
          case _                => error()
      case If(cond, thenExpr, elseExpr) =>
        interp(cond, env) match
          case BoolV(bool) =>
            if (bool) interp(thenExpr, env)
            else interp(elseExpr, env)
          case _ => error()

}
