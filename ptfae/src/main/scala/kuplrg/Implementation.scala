package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  extension (lty: Type)
    def ===(rty: Type): Boolean =
      (lty, rty) match
        case (NumT, NumT) => true
        case (ArrowT(pty1, rty1), ArrowT(pty2, rty2)) =>
          pty1 === pty2
          rty1 === rty2
          true
        case (VarT(_), VarT(_)) => true
        case (PolyT(name1, ty1), PolyT(name2, ty2)) =>
          ty1 === subst(ty2, name1, ty2)
          true
        case _ => error("Type mismatch")
    def ?(tenv: TypeEnv): Type =
      lty match
        case NumT                   => NumT
        case ArrowT(paramTy, retTy) => ArrowT(paramTy ? tenv, retTy ? tenv)
        case VarT(name) =>
          if (!tenv.tys.contains(name)) error("Unknown type")
          VarT(name)
        case PolyT(name, ty) => PolyT(name, ty ? tenv.addType(name))

  def subst(bodyTy: Type, name: String, ty: Type): Type =
    bodyTy match
      case NumT => NumT
      case ArrowT(paramTy, retTy) =>
        ArrowT(subst(paramTy, name, ty), subst(retTy, name, ty))
      case VarT(x) => if (name == x) ty else VarT(x)
      case PolyT(x, bodyTy) =>
        if (name == x) PolyT(x, bodyTy) else PolyT(x, subst(bodyTy, name, ty))

  def typeCheck(expr: Expr, tenv: TypeEnv): Type =
    expr match
      case Num(number) => NumT
      case Add(left, right) =>
        typeCheck(left, tenv) === NumT
        typeCheck(right, tenv) === NumT
        NumT
      case Mul(left, right) =>
        typeCheck(left, tenv) === NumT
        typeCheck(right, tenv) === NumT
        NumT
      case Val(name, init, body) =>
        val initTy = typeCheck(init, tenv)
        val bodyTy = typeCheck(body, tenv.addVar(name, initTy))
        bodyTy
      case Id(name) =>
        tenv.vars.getOrElse(name, error("Unknown type"))
      case Fun(param, ty, body) =>
        ty ? tenv
        val nty = typeCheck(body, tenv.addVar(param, ty))
        ArrowT(ty, nty)
      case App(fun, arg) =>
        typeCheck(fun, tenv) match
          case ArrowT(paramTy, retTy) =>
            val argTy = typeCheck(arg, tenv)
            paramTy === argTy
            retTy
          case _ => error("Not a function")
      case TypeAbs(name, body) =>
        if (tenv.tys.contains(name)) error("Already defined")
        val ty = PolyT(name, typeCheck(body, tenv.addType(name)))
        ty
      case TypeApp(expr, ty) =>
        ty ? tenv
        typeCheck(expr, tenv) match
          case PolyT(name, bodyTy) => subst(bodyTy, name, ty ? tenv)
          case _                   => error("Homomorphic type")

  def interp(expr: Expr, env: Env): Value =
    expr match
      case Num(number) => NumV(number)
      case Add(left, right) =>
        val NumV(n1) = interp(left, env)
        val NumV(n2) = interp(right, env)
        NumV(n1 + n2)
      case Mul(left, right) =>
        val NumV(n1) = interp(left, env)
        val NumV(n2) = interp(right, env)
        NumV(n1 * n2)
      case Val(name, init, body) =>
        val v1 = interp(init, env)
        val v2 = interp(body, env + (name -> v1))
        v2
      case Id(name) =>
        env.getOrElse(name, error("Free identifier"))
      case Fun(param, ty, body) => CloV(param, body, env)
      case App(fun, arg) =>
        interp(fun, env) match
          case CloV(param, body, nenv) =>
            val v1 = interp(arg, env)
            val v2 = interp(body, nenv + (param -> v1))
            v2
          case _ => error("Not a function")
      case TypeAbs(name, body) => TypeAbsV(name, body, env)
      case TypeApp(expr, ty) =>
        interp(expr, env) match
          case TypeAbsV(name, body, nenv) =>
            val v = interp(body, nenv)
            v
          case _ => error("Homomorphic type")
}
