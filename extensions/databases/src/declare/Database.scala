package grit.databases.declare

import grit.core.place.Service
import grit.core.secret.Variable

/** A Postgres database a deployment's turns may read at `service`'s place: `url` names the
  * variable holding its JDBC URL, `jdbc:postgresql://[host[:port][,…]]/database[?parameters]`
  * (the host may be empty, as for a cloud connector's socket factory), with no credential and
  * none of the settings grit makes ([[grit.databases.connect.DatabaseReader.open]]); logged in
  * as `login` says, as `role` and no other; `instructions` its Place layer.
  */
final case class Database(
    service: Service,
    url: Variable,
    login: Login,
    role: DatabaseRole,
    instructions: String,
    limits: QueryLimits = QueryLimits.Default
)
