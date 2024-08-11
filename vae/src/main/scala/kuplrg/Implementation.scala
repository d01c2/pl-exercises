package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env): Value = 
    expr match
      case Num(number) => number
      case Add(left, right) => interp(left, env) + interp(right, env)
      case Mul(left, right) => interp(left, env) * interp(right, env)
      case Val(name, init, body) => interp(body, env + (name -> interp(init, env)))
      case Id(name) => env.getOrElse(name, error(s"free identifier"))
    
  def freeIds(expr: Expr): Set[String] = 
    def aux(expr: Expr, ids: Set[String]): Set[String] =
      expr match
        case Num(number) => Set.empty
        case Add(left, right) => aux(left, ids) ++ aux(right, ids)
        case Mul(left, right) => aux(left, ids) ++ aux(right, ids)
        case Val(name, init, body) => aux(init, ids) ++ aux(body, ids + name)
        case Id(name) => if ids.contains(name) then Set.empty else Set(name)
    aux(expr, Set.empty)

  def bindingIds(expr: Expr): Set[String] =
    def aux(expr: Expr, ids: Set[String]): Set[String] =
      expr match
        case Num(number) => Set.empty
        case Add(left, right) => aux(left, ids) ++ aux(right, ids)
        case Mul(left, right) => aux(left, ids) ++ aux(right, ids)
        case Val(name, init, body) => aux(init, ids) ++ aux(body, ids + name) + name
        case Id(name) => if ids.contains(name) then Set(name) else Set.empty
    aux(expr, Set.empty)

  def boundIds(expr: Expr): Set[String] = 
    def aux(expr: Expr, ids: Set[String]): Set[String] =
      expr match
        case Num(number) => Set.empty
        case Add(left, right) => aux(left, ids) ++ aux(right, ids)
        case Mul(left, right) => aux(left, ids) ++ aux(right, ids)
        case Val(name, init, body) => aux(init, ids) ++ aux(body, ids + name)
        case Id(name) => if ids.contains(name) then Set(name) else Set.empty
    aux(expr, Set.empty)

  def shadowedIds(expr: Expr): Set[String] = 
    def aux(expr: Expr, ids: Set[String]): Set[String] =
      expr match
        case Num(number) => Set.empty
        case Add(left, right) => aux(left, ids) ++ aux(right, ids)
        case Mul(left, right) => aux(left, ids) ++ aux(right, ids)
        case Val(name, init, body) => 
          if ids.contains(name) then aux(init, ids) ++ aux(body, ids + name) + name
          else aux(init, ids) ++ aux(body, ids + name)
        case Id(name) => Set.empty
    aux(expr, Set.empty)

}
