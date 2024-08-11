package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr): Value = expr match
    case Num(number) => number
    case Add(left, right) => interp(left) + interp(right)
    case Mul(left, right) => interp(left) * interp(right)
    

  def countNums(expr: Expr): Int = expr match
    case Num(number) => 1
    case Add(left, right) => countNums(left) + countNums(right)
    case Mul(left, right) => countNums(left) + countNums(right)

}
