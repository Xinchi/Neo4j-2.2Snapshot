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
package org.neo4j.cypher.internal.compiler.v2_2.spi

import org.neo4j.cypher.internal.compiler.v2_2.ast.False

import scala.io.Source
import java.io.{BufferedWriter, FileWriter, PrintWriter}
import org.neo4j.cypher.internal.compiler.v2_2.planner.logical.{Cardinality, Selectivity}
import org.neo4j.cypher.internal.compiler.v2_2.{LabelId, PropertyKeyId, RelTypeId}

import scala.collection.mutable
sealed trait StatisticsKey
case class NodesWithLabelCardinality(labelId: Option[LabelId]) extends StatisticsKey
case class CardinalityByLabelsAndRelationshipType(lhs: Option[LabelId], relType: Option[RelTypeId], rhs: Option[LabelId]) extends StatisticsKey
case class IndexSelectivity(labelId: LabelId, propertyKeyId: PropertyKeyId) extends StatisticsKey

case class MutableGraphStatisticsSnapshot(map: mutable.Map[StatisticsKey, Double] = mutable.Map.empty) {
  def freeze: GraphStatisticsSnapshot =
    GraphStatisticsSnapshot(map.toMap)
}

case class GraphStatisticsSnapshot(map: Map[StatisticsKey, Double] = Map.empty) {
  def recompute(statistics: GraphStatistics): GraphStatisticsSnapshot = {
    val snapshot = MutableGraphStatisticsSnapshot()
    val instrumented = InstrumentedGraphStatistics(statistics, snapshot)
    map.keys.foreach {
      case NodesWithLabelCardinality(labelId) =>
        instrumented.nodesWithLabelCardinality(labelId)
      case CardinalityByLabelsAndRelationshipType(lhs, relType, rhs) =>
        instrumented.cardinalityByLabelsAndRelationshipType(lhs, relType, rhs)
      case IndexSelectivity(labelId, propertyKeyId) =>
        instrumented.indexSelectivity(labelId, propertyKeyId)
    }
    snapshot.freeze
  }

  def diverges(snapshot: GraphStatisticsSnapshot, minThreshold: Double): Boolean = {
    assert(map.keySet == snapshot.map.keySet)
    def vectorLength(map: Map[StatisticsKey, Double]): Double =
      Math.sqrt(map.values.map(Math.pow(_, 2)).kahanSum)

    val dotProduct: Double = (map.toSeq ++ snapshot.map.toSeq)
      .groupBy(_._1)
      .values
      .map(_.map(_._2).product)
      .kahanSum

    val cosa = dotProduct / (vectorLength(map) * vectorLength(snapshot.map))
    val divergence = Math.acos(cosa) * 2 / Math.PI
    divergence > minThreshold
  }

  implicit class RichDoubleIterable(xs: Iterable[Double]) {
    def kahanSum: Double = {
      val (sum, carry) = xs.foldLeft((0.0, 0.0)){ case ((sum, carry), x) => {
        val x2 = x - carry
        val newSum = sum + x2
        (newSum, (newSum - sum) - x2)
      }}
      sum
    }
  }
}

case class InstrumentedGraphStatistics(inner: GraphStatistics, snapshot: MutableGraphStatisticsSnapshot) extends GraphStatistics {
  val fbw = new PrintWriter(new BufferedWriter(new FileWriter("InstrumentedGraphStatistics.txt",true)))
  fbw.println("-------------- Constructor ---------------")
  fbw.println("inner = \n"+inner.toString)
  fbw.println("snapshot = \n"+snapshot.toString+"\n")
  fbw.println("------- Read File -------")
  fbw.close()
  val nodesWithLabelCardinalityInputDict = getStatsForCardinalityWithLabel(CardinalityInputType.NodesWithLabelCardinalityTypeInput)
  val cardinalityByLabelAndRelationshipTypeInputDict = getStatsForCardinalityWithLabelAndRelationType()

  object CardinalityInputType extends Enumeration {
    type CardinalityType = Value
    val NodesWithLabelCardinalityTypeInput, CardinalityByLabelAndRelationshipTypeInput, IndexSelectivityInput = Value
  }
  import CardinalityInputType._

  def getStatsForCardinalityWithLabel(t: CardinalityType):mutable.Map[Int, Double] = {
    val res = mutable.Map[Int, Double]()
    val filename = t match {
      case NodesWithLabelCardinalityTypeInput => "/Users/Max1/Dropbox/UCSD/MSProject/neo4j/cardinalityInput/nodesWithLabelCardinality.txt"
      case CardinalityByLabelAndRelationshipTypeInput => "/Users/Max1/Dropbox/UCSD/MSProject/neo4j/cardinalityInput/cardinalityByLabelAndRelationship.txt"
      case _ => "None"
    }

    for (line <- Source.fromFile(filename).getLines()) {
      val data = line.split(",")
      //      res(data(0)) = data(1).toInt
      res += data(0).toInt -> data(1).toDouble
    }
    res
  }

  def getStatsForCardinalityWithLabelAndRelationType(): mutable.Map[Int, mutable.Map[Int, mutable.Map[Int, Double]]] = {
    val res = mutable.Map[Int, mutable.Map[Int, mutable.Map[Int, Double]]]()
    val filename = "/Users/Max1/Dropbox/UCSD/MSProject/neo4j/cardinalityInput/cardinalityByLabelAndRelationship.txt"
    for (line <- Source.fromFile(filename).getLines()) {
      val data = line.split(",")
      val fromLabelDict = res.getOrElseUpdate(data(0).toInt, mutable.Map[Int, mutable.Map[Int,Double]]())
      val relationDict = fromLabelDict.getOrElseUpdate(data(1).toInt, mutable.Map[Int,Double]())
      val toLabelDict = relationDict.getOrElseUpdate(data(2).toInt, data(3).toDouble)
    }
    res
  }
  def nodesWithLabelCardinality(labelId: Option[LabelId]): Cardinality = {
//    Configuration
    val fromInput:Boolean = false
//    End of configuration
    val fbw = new PrintWriter(new BufferedWriter(new FileWriter("InstrumentedGraphStatistics.txt",true)))
    fbw.println("-------------- nodesWithLabelCardinality ---------------")
    fbw.println("inner = "+inner.toString+"\n")
    fbw.println("snapshot = "+snapshot.toString+"\n")
    fbw.println("labelId = "+labelId.toString+"\n")

    if(fromInput) {
      fbw.println("* cardinality from manual input * ")
      fbw.println(nodesWithLabelCardinalityInputDict.toString())
      // Empty Case
      if(labelId.isEmpty) {
        fbw.println("cardinality = "+Cardinality(nodesWithLabelCardinalityInputDict.get(-1).get)+"\n")
        return Cardinality(nodesWithLabelCardinalityInputDict.get(-1).get)
      }
      if(nodesWithLabelCardinalityInputDict.get(labelId.get.id).isEmpty) {
        //      throw new Exception("The labelID is not defined!")
        fbw.println("label input is not defined")
        return Cardinality(2)
      }
      val res = Cardinality(nodesWithLabelCardinalityInputDict.get(labelId.get.id).get)
      fbw.println("cardinality = "+res.toString+"\n")
      fbw.close()
      return res
    }

    val res = snapshot.map.getOrElseUpdate(NodesWithLabelCardinality(labelId), inner.nodesWithLabelCardinality(labelId).amount)
    fbw.println("cardinality = "+res.toString+"\n")
    fbw.close()
    res
  }


  def cardinalityByLabelsAndRelationshipType(fromLabel: Option[LabelId], relTypeId: Option[RelTypeId], toLabel: Option[LabelId]): Cardinality = {
    val fbw = new PrintWriter(new BufferedWriter(new FileWriter("InstrumentedGraphStatistics.txt",true)))
    fbw.println("-------------- cardinalityByLabelsAndRelationshipType ---------------\n")
    fbw.println("dict = "+cardinalityByLabelAndRelationshipTypeInputDict.toString())
    fbw.println("fromLabel = "+fromLabel.toString+"\n")
    fbw.println("relTypeId = "+relTypeId.toString+"\n")
    fbw.println("toLabel = "+toLabel.toString+"\n")
    val res = snapshot.map.getOrElseUpdate(
      CardinalityByLabelsAndRelationshipType(fromLabel, relTypeId, toLabel),
      inner.cardinalityByLabelsAndRelationshipType(fromLabel, relTypeId, toLabel).amount
    )
//    if(fromLabel == Some(LabelId(20))) {
//      val res2 = Cardinality(1000)
//      fbw.println("cardinality got or updated to = " + res2.toString+"\n")
//      fbw.close()
//      return res2
//    }
    fbw.println("cardinality got or updated to = " + res.toString+"\n")
    fbw.close()
    res
  }

  def indexSelectivity(label: LabelId, property: PropertyKeyId): Option[Selectivity] = {
    val selectivity = inner.indexSelectivity(label, property)
    snapshot.map.getOrElseUpdate(IndexSelectivity(label, property), selectivity.fold(0.0)(_.factor))
    selectivity
  }
}
