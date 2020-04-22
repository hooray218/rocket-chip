package freechips.rocketchip.linting

import firrtl.ir._
import firrtl._
import firrtl.traversals.Foreachers._

import scala.collection.mutable

case class LintError(module: String, name: String, info: Info) {
  def flatten(info: Info): Seq[Info] = info match {
    case MultiInfo(seq) => seq.flatMap(flatten)
    case other => Seq(other)
  }
  def getScalaInfos: Set[Info] = flatten(info).collect {
    case i if i.serialize.contains("scala") => i
  }.toSet
}
case class LintExceptions(seq: Seq[LintError]) extends FirrtlUserException(
  s"""Linting Report: ${seq.size} number of exceptions! Only showing first few:
     |${seq.zip(0 until 10)
           .map { case (lint: LintError, idx: Int) => s"$idx. ${lint.module}: ${lint.name} at ${lint.getScalaInfos}"}
           .mkString("\n")}
     """.stripMargin
)

class ChiselLinting extends Transform {

  override def inputForm = HighForm

  override def outputForm = HighForm

  type Errors = mutable.HashMap[String, LintError]

  override def execute(state: CircuitState): CircuitState = {
    val es = state.circuit.modules.flatMap(lintModule)
    if(es.nonEmpty) throw LintExceptions(es) else state
  }

  /** Determines whether name is prepended with an underscore, indicating a bad name
    */
  def isTemporary(name: String): Boolean = name.nonEmpty && name.head == '_'
  def isTemporary(expr: Expression): Boolean = isTemporary(getName(expr))

  def getName(expr: Expression): String = expr match {
    case r: WRef => r.name
    case f: WSubField => getName(f.expr)
    case i: WSubIndex => getName(i.expr)
    case a: WSubAccess => getName(a.expr)
    case other => throw new Exception(s"Unexpected match! $other")
  }

  private def lintModule(m: DefModule): Seq[LintError] = {
    val errors = new Errors()
    m.foreach(lintStatement(errors, m.name))
    errors.values.toSeq
  }

  private def lintStatement(errors: Errors, mname: String)(s: Statement): Unit = {
    s foreach lintStatement(errors, mname)
    s match {
      case r: DefRegister  if isTemporary(r.name) => errors(r.name) = LintError(mname, r.name, r.info)
      case i: WDefInstance if isTemporary(i.name) => errors(i.name) = LintError(mname, i.name, i.info)
      case Connect(info, loc, expr) if errors.contains(getName(loc)) => lintConnect(errors, mname)(info, loc)
      case PartialConnect(info, loc, expr) if errors.contains(getName(loc)) => lintConnect(errors, mname)(info, loc)
      case other =>
    }
  }
  
  private def lintConnect(errors: Errors, mname: String)(info: Info, loc: Expression): Unit = {
    val name = getName(loc)
    val newInfo = errors(name).info match {
      case NoInfo => info
      case MultiInfo(infos) if info != NoInfo => MultiInfo(info +: infos)
      case f: FileInfo if info != NoInfo => MultiInfo(Seq(info, f))
      case other => other
    }
    errors(getName(loc)) = LintError(mname, name, newInfo)
  }
}
