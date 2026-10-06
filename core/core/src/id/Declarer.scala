package grit.core.id

/** Who declared a schedule: the deployment itself, or one of its plugins. */
enum Declarer {
  case Deployment
  case Plugin(name: PluginName)
}
