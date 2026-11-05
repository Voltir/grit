package grit.databases.connect

import utest.*

/** [[DatabaseReader.readable]] against a live Postgres: what a role may read, and only that. */
object ReadableLiveTests extends TestSuite {

  private lazy val db = AppDatabase("readable_live")

  private val Integer = "integer"

  private val Leads = Relation(
    "app",
    "leads",
    Relation.Kind.Table,
    Some("One row per lead the application took in."),
    Vector(
      Relation.Column("id", Integer, Some("Ours, in the offers URL.")),
      Relation.Column("external_id", "text", Some("The one sent to the supplier."))
    ),
    Vector.empty
  )

  private val LeadIds =
    Relation(
      "app",
      "lead_ids",
      Relation.Kind.View,
      None,
      Vector(Relation.Column("id", Integer, None)),
      Vector.empty
    )

  val tests = Tests {
    test("the role's granted columns alone, by schema and name, with comments, kinds and keys") {
      val user = db.login(
        "catalog",
        Seq(
          "SELECT ON app.leads",
          "SELECT (id, lead_id) ON app.applicants",
          "SELECT ON app.lead_ids"
        )
      )
      val reader = db.reader(user)
      try {
        assert(
          reader.readable() == Right(
            Vector(
              Relation(
                "app",
                "applicants",
                Relation.Kind.Table,
                None,
                Vector(
                  Relation.Column("id", Integer, None),
                  Relation.Column("lead_id", Integer, None)
                ),
                Vector(Relation.Reference("app", "leads", Vector(("lead_id", "id"))))
              ),
              LeadIds,
              Leads
            )
          )
        )
      } finally reader.close()
    }

    test("a foreign key whose column the role may not select is not listed") {
      val user = db.login("keyless", Seq("SELECT ON app.leads", "SELECT (id) ON app.applicants"))
      val reader = db.reader(user)
      try {
        assert(
          reader.readable().map(_.filter(_.name == "applicants")) == Right(
            Vector(
              Relation(
                "app",
                "applicants",
                Relation.Kind.Table,
                None,
                Vector(Relation.Column("id", Integer, None)),
                Vector.empty
              )
            )
          )
        )
      } finally reader.close()
    }
  }
}
