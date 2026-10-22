package grit.tools

import grit.core.tool.ToolName

/** The names of grit's own tools, every one this module makes: `about`, `clearance`,
  * `message_ids`, `file_read`, `propose_model_setting`, `probe_pair`, `label_room`, `quiet_room`
  * and the coding tools. A plugin's tool may take none of them.
  */
object Names {
  val all: Vector[ToolName] =
    Vector(
      About.Name,
      Cleared.Name,
      MessageIds.Name,
      FileReading.Name,
      Tuning.Name,
      Probes.Name,
      Administering.LabelRoom,
      Administering.QuietRoom
    ) ++ Coding.hosted
      .map(_.name)
}
