package grit.tools

import grit.core.tool.ToolName

/** The names of grit's own tools, every one this module makes: `about`, `clearance`,
  * `propose_model_setting`, `probe_pair` and the coding tools. A plugin's tool may take none
  * of them.
  */
object Names {
  val all: Vector[ToolName] =
    Vector(About.Name, Cleared.Name, Tuning.Name, Probes.Name) ++ Coding.hosted.map(_.name)
}
