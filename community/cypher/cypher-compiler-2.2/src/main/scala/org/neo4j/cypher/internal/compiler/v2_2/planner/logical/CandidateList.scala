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
package org.neo4j.cypher.internal.compiler.v2_2.planner.logical

import java.io.{FileWriter, BufferedWriter, PrintWriter}

import org.neo4j.cypher.internal.compiler.v2_2.planner.logical.plans.LogicalPlan

case class CandidateList(plans: Seq[LogicalPlan] = Seq.empty) {

  override def toString= {
    val sb = new StringBuilder
    for (plan <- plans)
      sb.append(plan.toString+"\n\n")
    sb.toString()
  }

  def printPrettyPlans(plans: Seq[LogicalPlan], details: Boolean) = {
    for(plan <- plans) plan.printPrettyTree("CandidateList.txt", details)
  }

  def ++(other: CandidateList): CandidateList = {
    val candidateList = CandidateList(plans ++ other.plans)
    val fbwtmp = new PrintWriter(new BufferedWriter(new FileWriter("CandidateList.txt", true)));
    fbwtmp.println("------------------------------ ++ ------------------------------")
    fbwtmp.println("Original CandidateList = ")
    fbwtmp.close()
    printPrettyPlans(plans, false)
    val fbw = new PrintWriter(new BufferedWriter(new FileWriter("CandidateList.txt", true)));
    fbw.println("Added CandidateList = ")
    fbw.close()
    printPrettyPlans(other.plans, false)
    //    fbw.println("other = " +other.toString)
//    fbw.println("New CandidateList = "+candidateList.toString + "\n")
//    fbw.close()
    candidateList
  }

  def +(plan: LogicalPlan) = {
    val candidateList = copy(plans :+ plan)
    val fbwtmp = new PrintWriter(new BufferedWriter(new FileWriter("CandidateList.txt", true)));
    fbwtmp.println("------------------------------ + ------------------------------")
    fbwtmp.println("Original CandidateList = ")
    fbwtmp.close()
    printPrettyPlans(plans, false)
    val fbw = new PrintWriter(new BufferedWriter(new FileWriter("CandidateList.txt", true)));
    fbw.println("Added CandidateList = ")
    fbw.close()
    plan.printPrettyTree("CandidateList.txt", false)
    candidateList
  }

  final val VERBOSE = false

  def bestPlan(implicit context: LogicalPlanningContext): Option[LogicalPlan] = {
    val fbwtmp = new PrintWriter(new BufferedWriter(new FileWriter("CandidateList.txt", true)));
    fbwtmp.println("----------------- bestPlan -----------------")
    fbwtmp.println("plans = ")
    fbwtmp.close()
    printPrettyPlans(plans, true)

    val costs = context.cost
    val comparePlans = (c: LogicalPlan) =>
      (-c.solved.numHints, costs(c, context.cardinalityInput), -c.availableSymbols.size)

    val fbw = new PrintWriter(new BufferedWriter(new FileWriter("CandidateList.txt", true)));

    if (VERBOSE) {
      fbw.println("VERBOSE is true!!!!!!")
      val sortedPlans = plans.sortBy(comparePlans)
      if (sortedPlans.size > 1) {
        println("Get best of:")
        fbw.println("Get best of:")
        for (plan <- sortedPlans) {
          println("* " + plan.toString + s"\n${costs(plan, context.cardinalityInput)}\n")
          fbw.println("* " + plan.toString + s"\n${costs(plan, context.cardinalityInput)}\n")
        }

        println("Best is:")
        fbw.println("Best is:")
        println(sortedPlans.head.toString)
        fbw.println(sortedPlans.head.toString)
        println()
      }

      sortedPlans.headOption
    } else {
      if (plans.isEmpty) {
        fbw.println("plans = empty\n\n")
        fbw.close()
        None
      }
      else {
        val some = Some(plans.minBy(comparePlans))
        if(some.isEmpty) {
          fbw.println("plans.minBy(comparePlans) didn't produce a result")
          fbw.close()
          return some
        }

        fbw.println("Return = \n")
        fbw.close()
        some.get.printPrettyTree("CandidateList.txt", false)
        some
      }
    }
  }

  def map(f: LogicalPlan => LogicalPlan): CandidateList = copy(plans = plans.map(f))

  def isEmpty = plans.isEmpty
}

object Candidates {
  def apply(plans: LogicalPlan*): CandidateList = CandidateList(plans)
}
