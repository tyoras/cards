import Utils.*

addCommandAlias("ls", "projects")
addCommandAlias("cd", "project")
addCommandAlias("root", "cd cards")
addCommandAlias("c", "compile")
addCommandAlias("ca", "Test / compile")
addCommandAlias("t", "test")
addCommandAlias("r", "run")
addCommandAlias("fmtCheck", "scalafmtSbtCheck; scalafmtCheckAll")
addCommandAlias("fmt", "scalafmtSbt; scalafmtAll")
addCommandAlias(
  "dep",
  "reload plugins; dependencyUpdates; reload return; dependencyUpdates",
)

onLoadMessage +=
  s"""|
      |╭─────────────────────────────────╮
      |│     List of defined ${styled("aliases")}     │
      |├─────────────┬───────────────────┤
      |│ ${styled("l")}           │ projects          │
      |│ ${styled("cd")}          │ project           │
      |│ ${styled("root")}        │ cd root           │
      |│ ${styled("c")}           │ compile           │
      |│ ${styled("ca")}          │ compile all       │
      |│ ${styled("t")}           │ test              │
      |│ ${styled("r")}           │ run               │
      |│ ${styled("fmt")}         │ fmt & fix check   │
      |│ ${styled("fmtCheck")}    │ fix then fmt      │
      |│ ${styled("dep")}         │ dependencyUpdates │
      |╰─────────────┴───────────────────╯""".stripMargin
