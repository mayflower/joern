package io.joern.php2cpg.passes

import io.joern.php2cpg.astcreation.AstCreator
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.codepropertygraph.generated.PropertyDefaults
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*

/** Sets all untyped AST nodes to `Defines.Any` as a fallback.
  *
  * Note: This is a legacy pass that provides a blanket fallback for nodes without explicit types. For proper type
  * inference, use `PhpTypeRecoveryPassGenerator` from the post-processing passes in
  * `io.joern.x2cpg.frontendspecific.php2cpg`. The type recovery pass handles:
  *   - Literal type inference (int, string, etc.)
  *   - Object instantiation types
  *   - Method return type propagation
  *   - Type hints from parameters and return types
  *
  * This pass runs before `TypeNodePass` to ensure all nodes have a type full name set, which is required for proper
  * TYPE node creation.
  */
class AnyTypePass(cpg: Cpg) extends ForkJoinParallelCpgPass[AstNode](cpg) {

  override def generateParts(): Array[AstNode] = {
    cpg.graph.nodesWithProperty(PropertyNames.TypeFullName, PropertyDefaults.TypeFullName).collectAll[AstNode].toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, node: AstNode): Unit = {
    diffGraph.setNodeProperty(node, PropertyNames.TypeFullName, Defines.Any)
  }
}
