package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env, fenv: FEnv): Value =
    expr match
      case Num(number) => number
      case Add(left, right) =>
        interp(left, env, fenv) + interp(right, env, fenv)
      case Mul(left, right) =>
        interp(left, env, fenv) * interp(right, env, fenv)
      case Val(name, init, body) =>
        interp(body, env + (name -> interp(init, env, fenv)), fenv)
      case Id(name) =>
        env.getOrElse(name, error("free identifier"))
      case App(fname, arg) =>
        val fdef =
          fenv.getOrElse(fname, error("unknown function"))
        interp(
          fdef.body,
          Map(fdef.param -> interp(arg, env, fenv)),
          fenv
        )

  def interpDS(expr: Expr, env: Env, fenv: FEnv): Value =
    expr match
      case Num(number) => number
      case Add(left, right) =>
        interpDS(left, env, fenv) + interpDS(right, env, fenv)
      case Mul(left, right) =>
        interpDS(left, env, fenv) * interpDS(right, env, fenv)
      case Val(name, init, body) =>
        interpDS(body, env + (name -> interpDS(init, env, fenv)), fenv)
      case Id(name) =>
        env.getOrElse(name, error("free identifier"))
      case App(fname, arg) =>
        val fdef =
          fenv.getOrElse(fname, error("unknown function"))
        interpDS(
          fdef.body,
          env + (fdef.param -> interpDS(arg, env, fenv)),
          fenv
        )
}
