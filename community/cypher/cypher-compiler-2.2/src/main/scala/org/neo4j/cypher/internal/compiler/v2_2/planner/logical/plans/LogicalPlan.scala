/**
 * Copyright (c) 2002-2014 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypher.internal.compiler.v2_2.planner.logical.plans

import java.io.{FileWriter, BufferedWriter, PrintWriter}
import java.lang.reflect.Method

import org.neo4j.cypher.internal.compiler.v2_2.Foldable._
import org.neo4j.cypher.internal.compiler.v2_2.Rewritable._
import org.neo4j.cypher.internal.compiler.v2_2.ast.{True, Identifier, Expression}
import org.neo4j.cypher.internal.compiler.v2_2.perty._
import org.neo4j.cypher.internal.compiler.v2_2.planner.PlannerQuery
import org.neo4j.cypher.internal.compiler.v2_2.{InternalException, Rewritable}

/*
A LogicalPlan is an algebraic query, which is represented by a query tree whose leaves are database relations and
non-leaf nodes are algebraic operators like selections, projections, and joins. An intermediate node indicates the
application of the corresponding operator on the relations generated by its children, the result of which is then sent
further up. Thus, the edges of a tree represent data flow from bottom to top, i.e., from the leaves, which correspond
to data in the database, to the root, which is the final operator producing the query answer. */
abstract class LogicalPlan
  extends Product
  with Rewritable
  with PageDocFormatting
  with Serializable {


  self =>

//  with ToPrettyString[LogicalPlan] {

//  def toDefaultPrettyString(formatter: DocFormatter) =
//    toPrettyString(formatter)(InternalDocHandler.docGen)

  def lhs: Option[LogicalPlan]
  def rhs: Option[LogicalPlan]
  def solved: PlannerQuery
  def availableSymbols: Set[IdName]

  log(this.getClass.getName)

  def log(msg:String) = {
    // Logger created by Max
    val fbw = new PrintWriter(new BufferedWriter(new FileWriter("LogicalPlan.txt", true)));
//    fbw.println("############################### "+msg+" ############################### ")
    fbw.println(msg)
    val sb = new StringBuilder
    traverse(self, sb)
    fbw.println(sb.toString())
    fbw.close()
  }

  def leafs: Seq[LogicalPlan] = this.treeFold(Seq.empty[LogicalPlan]) {
    case plan: LogicalPlan
      if plan.lhs.isEmpty && plan.rhs.isEmpty => (acc, r) => r(acc :+ plan)
  }

  def traverse(head: LogicalPlan, sb:StringBuilder) : Unit = {
    if(head == None || head == null) {
      return
    }
    if(head.lhs != null && !head.lhs.isEmpty)
      traverse(head.lhs.get, sb)
    sb.append(head.toString)
    if(head.lhs != null && !head.rhs.isEmpty)
      traverse(head.rhs.get, sb)
  }
//  override def toString = {
//    val sb = new StringBuilder
//    sb.append("--------- NODE ----------\n")
//    // solved
//    sb.append(solved.toString)
//    //availableSymbols
//    if(availableSymbols != null) {
//      sb.append("availableSymbols = [")
//      val it = availableSymbols.iterator
//      while(it.hasNext) {
//        val idName = it.next()
//        sb.append(idName.name+",")
//      }
//      sb.append("]\n")
//    }
//    sb.toString()
//  }
  def printPrettyTree(fileName: String, details: Boolean): Unit = {
    val fbw = new PrintWriter(new BufferedWriter(new FileWriter(fileName, true)));
    fbw.println("################################################################")
    prettyTree(self, s"#", fbw, false)
    if(details == true) {
      fbw.println("------------------------ DETAILS OF THE ABOVE TREE ------------------------")
      prettyTree(self, s"", fbw, true)
      fbw.println("------------------------ END OF DETAILS ------------------------")
    }
    fbw.println("################################################################\n")
    fbw.close()
  }

  def prettyTree(head: LogicalPlan, prefix: String, fbw: PrintWriter, details: Boolean): Unit = {
    if(head == null || head == None) {
      return
    }
    fbw.println(prefix + head.getClass.getName)
    if(details)
      fbw.println(head.toString)
    if(head.lhs != null && !head.lhs.isEmpty)
      prettyTree(head.lhs.get, prefix+"-", fbw, details)
    if(head.rhs != null && !head.rhs.isEmpty)
      prettyTree(head.rhs.get, prefix+"-", fbw, details)
  }


  def updateSolved(newSolved: PlannerQuery): LogicalPlan = {
    val arguments = this.children.toList :+ newSolved
    try {
      copyConstructor.invoke(this, arguments: _*).asInstanceOf[this.type]
    } catch {
      case e: IllegalArgumentException if e.getMessage.startsWith("wrong number of arguments") =>
        throw new InternalException("Logical plans need to be case classes, and have the PlannerQuery in a separate constructor")
    }
  }

  lazy val copyConstructor: Method = this.getClass.getMethods.find(_.getName == "copy").get

  def updateSolved(f: PlannerQuery => PlannerQuery): LogicalPlan =
    updateSolved(f(solved))

  def dup(children: Seq[AnyRef]): this.type =
    if (children.iterator eqElements this.children)
      this
    else {
      val constructor = this.copyConstructor
      val params = constructor.getParameterTypes
      val args = children.toVector
      if ((params.length == args.length + 1) && params.last.isAssignableFrom(classOf[PlannerQuery]))
        constructor.invoke(this, args :+ this.solved: _*).asInstanceOf[this.type]
      else
        constructor.invoke(this, args: _*).asInstanceOf[this.type]
    }

  def mapExpressions(f: (Set[IdName], Expression) => Expression): LogicalPlan = self
}

abstract class LogicalLeafPlan extends LogicalPlan {
  final val lhs = None
  final val rhs = None
  def argumentIds: Set[IdName]
}

final case class IdName(name: String) extends PageDocFormatting // with ToPrettyString[IdName] {
//  def toDefaultPrettyString(formatter: DocFormatter) =
//    toPrettyString(formatter)(InternalDocHandler.docGen)

object IdName {
  implicit val byName = Ordering[String].on[IdName](_.name)

  def fromIdentifier(identifier: Identifier) = IdName(identifier.name)
}
