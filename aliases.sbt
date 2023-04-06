import Utils.*

addCommandAlias("ls", "projects")
addCommandAlias("cd", "project")
addCommandAlias("root", "cd cards")
addCommandAlias("c", "compile")
addCommandAlias("tc", "test:compile")
addCommandAlias("t", "test")
addCommandAlias("r", "run")
addCommandAlias("dep", "dependencyUpdates")

onLoadMessage +=
  s"""|
      |───────────────────────────────
      |    List of defined ${styled("aliases")}
      |────────────┬──────────────────
      |${styled("ls")}          │ projects
      |${styled("cd")}          │ project
      |${styled("root")}        │ cd cards
      |${styled("c")}           │ compile
      |${styled("tc")}          │ test:compile
      |${styled("t")}           │ test
      |${styled("r")}           │ run
      |${styled("dep")}         │ dependencyUpdates
      |────────────┴──────────────────""".stripMargin
