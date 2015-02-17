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
package org.neo4j.cypher.internal.compiler.v2_2.planner

import java.io.{FileWriter, BufferedWriter, PrintWriter}
import org.neo4j.cypher.internal.compiler.v2_2.ast._
import org.neo4j.cypher.internal.compiler.v2_2.ast.convert.plannerQuery.ExpressionConverters._
import org.neo4j.cypher.internal.compiler.v2_2.perty._
import org.neo4j.cypher.internal.compiler.v2_2.planner.logical.plans._

import scala.collection.GenTraversableOnce

case class QueryGraph(patternRelationships: Set[PatternRelationship] = Set.empty,
                      patternNodes: Set[IdName] = Set.empty,
                      argumentIds: Set[IdName] = Set.empty,
                      selections: Selections = Selections(),
                      optionalMatches: Seq[QueryGraph] = Seq.empty,
                      hints: Set[Hint] = Set.empty,
                      shortestPathPatterns: Set[ShortestPathPattern] = Set.empty)
  extends PageDocFormatting{ // with ToPrettyString[QueryGraph] {

//  def toDefaultPrettyString(formatter: DocFormatter) =
//    toPrettySt   ring(formatter)(InternalDocHandler.docGen)

  def log(msg:String) = {
    // Logger created by Max
    val fbw = new PrintWriter(new BufferedWriter(new FileWriter("QueryGraph.txt", true)));
    fbw.println("############################### "+msg+" ############################### ")
    fbw.println(this.toString())
    fbw.close()
  }

  def addPatternNodes(nodes: IdName*): QueryGraph =  {
    val queryGraph = copy(patternNodes = patternNodes ++ nodes)
    queryGraph.log("addPatternNodes")
    return queryGraph
  }

  def addPatternRel(rel: PatternRelationship): QueryGraph = {
    val queryGraph = copy(
      patternNodes = patternNodes + rel.nodes._1 + rel.nodes._2,
      patternRelationships = patternRelationships + rel
    )
    queryGraph.log("addPatternRel")
    return queryGraph
  }


  def addPatternRels(rels: Seq[PatternRelationship]) =
    rels.foldLeft[QueryGraph](this)((qg, rel) => qg.addPatternRel(rel))

  def addShortestPath(shortestPath: ShortestPathPattern): QueryGraph = {
    val rel = shortestPath.rel
    val queryGraph = copy (
      patternNodes = patternNodes + rel.nodes._1 + rel.nodes._2,
      shortestPathPatterns = shortestPathPatterns + shortestPath
    )
    queryGraph.log("addShortestPath")
    return queryGraph
  }

  def addShortestPaths(shortestPaths: ShortestPathPattern*): QueryGraph = {
    val queryGraph = shortestPaths.foldLeft(this)((qg, p) => qg.addShortestPath(p))
    queryGraph.log("addShortestPaths")
    return queryGraph
  }
  def addArgumentId(newIds: Seq[IdName]): QueryGraph = {
    val queryGraph = copy(argumentIds = argumentIds ++ newIds)
    queryGraph.log("addArgumentId")
    return queryGraph
  }
  def addSelections(selections: Selections): QueryGraph = {
    val queryGraph = copy(selections = Selections(selections.predicates ++ this.selections.predicates))
    queryGraph.log("addSelections")
    return queryGraph
  }

  def addPredicates(predicates: Expression*): QueryGraph = {
    val newSelections = Selections(predicates.flatMap(_.asPredicates).toSet)
    val queryGraph = copy(selections = selections ++ newSelections)
    queryGraph.log("addPredicates")
    return queryGraph
  }

  def addHints(addedHints: GenTraversableOnce[Hint]): QueryGraph = {
    val queryGraph = copy(hints = hints ++ addedHints)
    queryGraph.log("addHints")
    return queryGraph
  }

  def withoutArguments(): QueryGraph = {
    val queryGraph = withArgumentIds(Set.empty)
    queryGraph.log("withoutArguments")
    return queryGraph
  }
  def withArgumentIds(newArgumentIds: Set[IdName]): QueryGraph = {
    val queryGraph = copy(argumentIds = newArgumentIds)
    queryGraph.log("withArgumentIds")
    return queryGraph
  }


  def withAddedOptionalMatch(optionalMatch: QueryGraph): QueryGraph = {
    val argumentIds = allCoveredIds intersect optionalMatch.allCoveredIds
    val queryGraph = copy(optionalMatches = optionalMatches :+ optionalMatch.addArgumentId(argumentIds.toSeq))
    queryGraph.log("withAddedOptionalMatch")
    return queryGraph
  }

  def withOptionalMatches(optionalMatches: Seq[QueryGraph]): QueryGraph = {
    val queryGraph = copy(optionalMatches = optionalMatches)
    queryGraph.log("withOptionalMatches")
    return queryGraph
  }

  def withSelections(selections: Selections): QueryGraph = {
    val queryGraph = copy(selections = selections)
    queryGraph.log("withSelections")
    return queryGraph
  }

  def knownLabelsOnNode(node: IdName): Seq[LabelName] =
    selections
      .labelPredicates.getOrElse(node, Seq.empty)
      .flatMap(_.labels).toSeq

  def findRelationshipsEndingOn(id: IdName): Set[PatternRelationship] =
    patternRelationships.filter { r => r.left == id || r.right == id }

  def allPatternNodes: Set[IdName] =
    patternNodes ++ optionalMatches.flatMap(_.allPatternNodes)

  def coveredIds: Set[IdName] = {
    val patternIds = QueryGraph.coveredIdsForPatterns(patternNodes, patternRelationships)
    patternIds ++ argumentIds ++ selections.predicates.flatMap(_.dependencies)
  }

  def allCoveredIds: Set[IdName] = {
    log("allCoveredIds")
    val optionalMatchIds = optionalMatches.flatMap(_.allCoveredIds)
    coveredIds ++ optionalMatchIds
  }

  val allHints: Set[Hint] =
    if (optionalMatches.isEmpty) hints else hints ++ optionalMatches.flatMap(_.allHints)

  def numHints = allHints.size

  def ++(other: QueryGraph): QueryGraph = {
    val queryGraph = QueryGraph(
      selections = selections ++ other.selections,
      patternNodes = patternNodes ++ other.patternNodes,
      patternRelationships = patternRelationships ++ other.patternRelationships,
      optionalMatches = optionalMatches ++ other.optionalMatches,
      argumentIds = argumentIds ++ other.argumentIds,
      hints = hints ++ other.hints,
      shortestPathPatterns = shortestPathPatterns ++ other.shortestPathPatterns
    )
    queryGraph.log("++(other: QueryGraph)")
    return queryGraph
  }


  def isCoveredBy(other: QueryGraph): Boolean = {
    patternNodes.subsetOf(other.patternNodes) &&
      patternRelationships.subsetOf(other.patternRelationships) &&
      argumentIds.subsetOf(other.argumentIds) &&
      optionalMatches.toSet.subsetOf(other.optionalMatches.toSet) &&
      selections.predicates.subsetOf(other.selections.predicates) &&
      shortestPathPatterns.subsetOf(other.shortestPathPatterns)
  }

  def covers(other: QueryGraph): Boolean = other.isCoveredBy(this)

  def hasOptionalPatterns = optionalMatches.nonEmpty

  def patternNodeLabels: Map[IdName, Set[LabelName]] =
    patternNodes.collect { case node: IdName => node -> selections.labelsOnNode(node) }.toMap

  // This is here to stop usage of copy from the outside
  private def copy(patternRelationships: Set[PatternRelationship] = patternRelationships,
                   patternNodes: Set[IdName] = patternNodes,
                   argumentIds: Set[IdName] = argumentIds,
                   selections: Selections = selections,
                   optionalMatches: Seq[QueryGraph] = optionalMatches,
                   hints: Set[Hint] = hints,
                   shortestPathPatterns: Set[ShortestPathPattern] = shortestPathPatterns) =
  QueryGraph(patternRelationships, patternNodes, argumentIds, selections, optionalMatches, hints, shortestPathPatterns)
}

object QueryGraph {
  val empty = QueryGraph()

  def coveredIdsForPatterns(patternNodeIds: Set[IdName], patternRels: Set[PatternRelationship]) = {
    val patternRelIds = patternRels.flatMap(_.coveredIds)
    patternNodeIds ++ patternRelIds
  }

  implicit object byCoveredIds extends Ordering[QueryGraph] {
    import scala.math.Ordering.Implicits

    def compare(x: QueryGraph, y: QueryGraph): Int = {
      val xs = x.coveredIds.toSeq.sorted(IdName.byName)
      val ys = y.coveredIds.toSeq.sorted(IdName.byName)
      Implicits.seqDerivedOrdering[Seq, IdName](IdName.byName).compare(xs, ys)
    }
  }
}
