package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = 
    // Helper Function - malloc
    def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)

    expr match
      case Num(n) => (NumV(n), mem)
      case Add(e1, e2) =>
        val (v1, m1) = interp(e1, env, mem)
        val (v2, m2) = interp(e2, env, m1)
        (v1, v2) match
          case (NumV(n1), NumV(n2)) => (NumV(n1 + n2), m2) 
          case _ => error("invalid operation")
      case Mul(e1, e2) =>
        val (v1, m1) = interp(e1, env, mem)
        val (v2, m2) = interp(e2, env, m1)
        (v1, v2) match
          case (NumV(n1), NumV(n2)) => (NumV(n1 * n2), m2) 
          case _ => error("invalid operation")
      case Id(x) => (env.getOrElse(x, error("free identifier")), mem)
      case Fun(x, e) => (CloV(x, e, env), mem)
      case App(e1, e2) =>
        val (v1, m1) = interp(e1, env, mem)
        val (v2, m2) = interp(e2, env, m1)
        v1 match
          case CloV(x, e3, nenv) => interp(e3, nenv + (x -> v2), m2)
          case _ => error("not a function")
      case NewBox(e) =>
        val (v, m1) = interp(e, env, mem)
        val a = malloc(m1)
        (BoxV(a), m1 + (a -> v))
      case GetBox(e) =>
        val (v1, m1) = interp(e, env, mem)
        v1 match
          case BoxV(a) => (m1(a), m1)
          case _ => error("not a box")
      case SetBox(e1, e2) =>
        val (v1, m1) = interp(e1, env, mem)
        val (v, m2) = interp(e2, env, m1)
        v1 match
          case BoxV(a) => (v, m2 + (a -> v))
          case _ => error("not a box")
      case Seq(e1, e2) =>
        val (_, m1) = interp(e1, env, mem)
        val (v2, m2) = interp(e2, env, m1)
        (v2, m2)
}
