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

import java.io.{PrintWriter, BufferedWriter, FileWriter}

import org.neo4j.cypher.internal.compiler.v2_2.ast.Collection
import org.neo4j.cypher.internal.compiler.v2_2.commands.ManyQueryExpression
import org.neo4j.cypher.internal.compiler.v2_2.planner.logical.Metrics._
import org.neo4j.cypher.internal.compiler.v2_2.planner.logical.plans._

/*
A very simplistic cost model. Each row returned by an operator costs 1. That's it.
 */
case class CardinalityCostModel(cardinality: CardinalityModel) extends CostModel {

  val CPU_BOUND_PLAN_COST_PER_ROW: CostPerRow = 0.1
  val DB_ACCESS_BOUND_PLAN_COST_PER_ROW: CostPerRow = 1.0

  private def costPerRow(plan: LogicalPlan): CostPerRow = plan match {

    case _: NodeHashJoin |
      _: Aggregation |
      _: AbstractLetSemiApply |
      _: Limit |
      _: Optional |
      _: SingleRow |
      _: Argument |
      _: OuterHashJoin |
      _: AbstractSemiApply |
      _: Skip |
      _: Sort |
      _: SortedLimit |
      _: Union |
      _: UnwindCollection
    => CPU_BOUND_PLAN_COST_PER_ROW

    case NodeIndexSeek(_, _, _, ManyQueryExpression(Collection(elements)), _) =>
      DB_ACCESS_BOUND_PLAN_COST_PER_ROW * Multiplier(elements.size)

    case NodeIndexSeek(_, _, _, ManyQueryExpression(Collection(elements)), _) =>
      DB_ACCESS_BOUND_PLAN_COST_PER_ROW * Multiplier(10)

    case _ => DB_ACCESS_BOUND_PLAN_COST_PER_ROW
  }

  private def cardinalityForPlan(plan: LogicalPlan, input: QueryGraphCardinalityInput): Cardinality = plan match {
    case Selection(_, left) => {
      val result = cardinality(left, input)
//      fbw.println("Cardinality in non-selection case = "+result.toString)
//      fbw.close()
      result
    }
    case _                  => {
      val result = plan.lhs.map(p => cardinality(p, input)).getOrElse(cardinality(plan, input))
//      val fbw = new PrintWriter(new BufferedWriter(new FileWriter("CardinalityCostModel.txt", true)));
//      fbw.println("Cardinality in non-selection case = "+result.toString)
//      fbw.close()
      result
    }
  }
//

  def apply(plan: LogicalPlan, input: QueryGraphCardinalityInput): Cost = {
    val fbwtemp = new PrintWriter(new BufferedWriter(new FileWriter("CardinalityCostModel.txt", true)));
    fbwtemp.println("---------------- apply --------------")
    fbwtemp.close()
    val fbw = new PrintWriter(new BufferedWriter(new FileWriter("CardinalityCostModel.txt", true)));
    plan.printPrettyTree("CardinalityCostModel.txt", true)
    fbw.println(plan.getClass.getName+"\n")
    val cost = plan match {
      case CartesianProduct(lhs, rhs) =>
        fbw.println("---- Case CartesianProduct --- ")
        val cost = apply(lhs, input) + cardinality(lhs, input) * apply(rhs, input)
        fbw.println(cost.toString)
        cost

      case Apply(lhs, rhs) =>
        fbw.println("---- Case Apply --- ")
        val newInput = input.withCardinality(cardinality(lhs, input))
        val lCost = apply(lhs, input)
        val rCost = apply(rhs, newInput)
        val cost = lCost + rCost
        fbw.println("lCost = " + lCost.toString)
        fbw.println("rCost = "+rCost.toString)
        fbw.println("cost = "+cost)
        cost

      case OuterHashJoin(_, lhs, rhs) =>
        fbw.println("---- Case OuterHashJoin --- ")
        val lCost = apply(lhs, input)
        val rCost = apply(rhs, input)
        val cost = lCost + rCost
        fbw.println("lCost = " + lCost.toString)
        fbw.println("rCost = "+rCost.toString)
        fbw.println("cost = "+cost)
        cost

      case _ =>
        fbw.println("---- Default Case --- ")
        val lhsCost = plan.lhs.map(p => apply(p, input)).getOrElse(Cost(0))
        val rhsCost = plan.rhs.map(p => apply(p, input)).getOrElse(Cost(0))
        val costForThisPlan = cardinalityForPlan(plan, input) * costPerRow(plan)
        val totalCost = costForThisPlan + lhsCost + rhsCost
        fbw.println("lhsCost = "+lhsCost.toString)
        fbw.println("rhsCost = "+rhsCost.toString)
        fbw.println("costForThisPlan = "+costForThisPlan.toString)
        fbw.println("totalCost = "+totalCost.toString)
        totalCost
    }
    fbw.println()
    fbw.close()
    return cost
  }
}
