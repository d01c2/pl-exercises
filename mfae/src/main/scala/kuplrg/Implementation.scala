package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  
  // Helper Function - malloc
  def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)
  
  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = 
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
      case Id(x) => (mem(env.getOrElse(x, error("free identifier"))), mem)
      case Var(x, e1, e2) => 
        val (v1, m1) = interp(e1, env, mem)
        val a = malloc(m1)
        interp(e2, env + (x -> a), m1 + (a -> v1))
      case Fun(x, e) => (CloV(x, e, env), mem)
      case App(e1, e2) =>
        val (v1, m1) = interp(e1, env, mem)
        val (v2, m2) = interp(e2, env, m1)
        v1 match
          case CloV(x, e3, nenv) => 
            val a = malloc(m2)
            interp(e3, nenv + (x -> a), m2 + (a -> v2))
          case _ => error("not a function")
      case Assign(x, e) => 
        val (v, nmem) = interp(e, env, mem)
        (v, nmem + (env.getOrElse(x, error("free identifier")) -> v))
      case Seq(e1, e2) =>
        val (_, m1) = interp(e1, env, mem)
        interp(e2, env, m1)

  def interpCBR(expr: Expr, env: Env, mem: Mem): (Value, Mem) =
    expr match
      case Num(n) => (NumV(n), mem)
      case Add(e1, e2) =>
        val (v1, m1) = interpCBR(e1, env, mem)
        val (v2, m2) = interpCBR(e2, env, m1)
        (v1, v2) match
          case (NumV(n1), NumV(n2)) => (NumV(n1 + n2), m2) 
          case _ => error("invalid operation")
      case Mul(e1, e2) =>
        val (v1, m1) = interpCBR(e1, env, mem)
        val (v2, m2) = interpCBR(e2, env, m1)
        (v1, v2) match
          case (NumV(n1), NumV(n2)) => (NumV(n1 * n2), m2) 
          case _ => error("invalid operation")
      case Id(x) => (mem(env.getOrElse(x, error("free identifier"))), mem)
      case Var(x, e1, e2) => 
        val (v1, m1) = interpCBR(e1, env, mem)
        val a = malloc(m1)
        interpCBR(e2, env + (x -> a), m1 + (a -> v1))
      case Fun(x, e) => (CloV(x, e, env), mem)
      case App(e1, e2) =>
        val (v1, m1) = interpCBR(e1, env, mem)
        e2 match
          case Id(x) =>
            v1 match
              case CloV(nx, e2, nenv) =>
                interpCBR(e2, nenv + (nx -> env.getOrElse(x, error("free identifier"))),m1)
              case _ => error("not a function")
          case _ =>
            v1 match
              case CloV(x, e3, nenv) =>
                val (v2, m2) = interpCBR(e2, env, m1)
                val a = malloc(m2)
                interpCBR(e3, nenv + (x -> a), m2 + (a -> v2))
              case _ => error("not a function")
      case Assign(x, e) =>
        val (v, nmem) = interpCBR(e, env, mem)
        (v, nmem + (env.getOrElse(x, error("free identifier")) -> v))
      case Seq(e1, e2) =>
        val (_, m1) = interpCBR(e1, env, mem)
        val (v2, m2) = interpCBR(e2, env, m1)
        (v2, m2)
}
