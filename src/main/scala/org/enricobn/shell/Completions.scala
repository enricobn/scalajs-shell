package org.enricobn.shell

import org.enricobn.vfs.VirtualFolder

/**
  * Created by enrico on 12/14/16.
  */

sealed trait CompletionResult
final case class NewLine(line: String) extends CompletionResult
final case class Proposals(proposals: Seq[String]) extends CompletionResult
final case class NoProposals() extends CompletionResult

trait Completions {

  def complete(line: String, currentFolder: VirtualFolder) : CompletionResult

}


