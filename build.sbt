lazy val root = (project in file(".")).
  settings(
    name := "koffio-validation",
    version := "0.0.1",
    scalaVersion := "2.11.7"
  )

resolvers += Resolver.sonatypeRepo("releases")

resolvers += "jitpack.io" at "https://jitpack.io"

//val skinnyValidator  = "org.skinny-framework"      %%  "skinny-validator"          % skinnyVersion exclude("joda-time", "joda-time")

libraryDependencies ++= Seq(
  "com.wix" %% "accord-core" % "0.5",
  "net.atinu" %% "dvalidation" % "0.2",
  "org.scalaz" %% "scalaz-core" % "7.1.0", // dependency for dvalidation
  "org.skinny-framework" %% "skinny-validator" % "1.3.18",
  "com.github.davegurnell" % "validation" % "3bcd4bff98"
)
