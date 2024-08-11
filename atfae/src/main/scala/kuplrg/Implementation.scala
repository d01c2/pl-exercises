package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def mustSameTy(ty1: Type, ty2: Type): Boolean =
    if (ty1 == ty2) true else error("Type Mismatch")

  def mustValid(ty: Type, tenv: TypeEnv): Boolean = ???

  def typeCheck(expr: Expr, tenv: TypeEnv): Type =
    expr match
      case Num(n)  => NumT
      case Bool(b) => BoolT
      case Add(e1, e2) =>
        mustSameTy(typeCheck(e1, tenv), NumT)
        mustSameTy(typeCheck(e2, tenv), NumT)
        NumT
      case Mul(e1, e2) =>
        mustSameTy(typeCheck(e1, tenv), NumT)
        mustSameTy(typeCheck(e2, tenv), NumT)
        NumT
      case Div(e1, e2) =>
        mustSameTy(typeCheck(e1, tenv), NumT)
        mustSameTy(typeCheck(e2, tenv), NumT)
        NumT
      case Mod(e1, e2) =>
        mustSameTy(typeCheck(e1, tenv), NumT)
        mustSameTy(typeCheck(e2, tenv), NumT)
        NumT
      case Eq(e1, e2) =>
        mustSameTy(typeCheck(e1, tenv), NumT)
        mustSameTy(typeCheck(e2, tenv), NumT)
        BoolT
      case Lt(e1, e2) =>
        mustSameTy(typeCheck(e1, tenv), NumT)
        mustSameTy(typeCheck(e2, tenv), NumT)
        BoolT
      case Val(x, e1, e2) =>
        val ty1 = typeCheck(e1, tenv)
        typeCheck(e2, tenv + (x -> ty1))
      case Id(x) =>
        tenv.vars.getOrElse(x, error("Undefined Type"))
      case Fun(xs, e) =>
        typeCheck(e, tenv ++ xs.map(typeCheck(_, tenv)))
      case Rec(x0, xs, ty, e, ne) =>
        val tys = xs.map(_.ty)
        tys.map(mustValid(_, tenv))
        mustValid(ty, tenv)

        tenv + (x0 -> ArrowT(tys, ty)) ++ xs.zip(tys).toMap
      case App(e0, es)       =>
      case If(e0, e1, e2)    =>
      case TypeDef(t, xs, e) =>
      case Match(e, xs)      =>

  def interp(expr: Expr, env: Env): Value = ???

}
