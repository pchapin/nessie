package edu.vtc.nessie

import edu.vtc.nesc.ASTNode

abstract class Processor(root: ASTNode) {
  def process() = root
}
