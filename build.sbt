import java.io.File

lazy val checkCopyrights = taskKey[Unit]("Checks copyrights of source files")
lazy val buildParsers = taskKey[Unit]("Builds parsers")
lazy val deleteParserDir = taskKey[Unit]("Delete java parser directory")

lazy val root = (project in file(".")).
  settings(
    name := "SAFE",
    version := "2.0",
    organization := "kr.ac.kaist.safe",
    scalaVersion := "2.12.6",
    checkCopyrights in Compile := {
      val violated: String = (baseDirectory.value + "/bin/checkCopyrights.sh" !!)
      if (violated != "") {
        throw new Error("\nFix the copyright(s) of the following:\n" + violated)
      }
    },
    buildParsers in Compile := {
      // xtc
      val xtcFile = new File("./lib/xtc.jar")
      if (!xtcFile.exists) {
        // TODO exception handling: not downloaded
        IO.download(new URL("https://cs.nyu.edu/rgrimm/xtc/xtc.jar"), xtcFile)
      }

      // webix
      val webixJsFile = new File("./src/main/resources/assets/js/webix.js")
      val webixCssFile = new File("./src/main/resources/assets/css/webix.css")
      if (!webixJsFile.exists)
        IO.download(new URL("http://cdn.webix.com/edge/webix.js"), webixJsFile)
      if (!webixCssFile.exists)
        IO.download(new URL("http://cdn.webix.com/edge/webix.css"), webixCssFile)

      val options = ForkOptions(bootJars = Seq(xtcFile))
      val srcDir = baseDirectory.value + "/src/main"
      val inDir = srcDir + "/scala/kr/ac/kaist/safe/parser/"
      val outDir = srcDir + "/java/kr/ac/kaist/safe/parser/"
      val outFile = file(outDir)
      if (!outFile.exists) IO.createDirectory(outFile)
      val arguments = Seq("-in", srcDir + "/scala", "-enc-out", "UTF-8",
                          "-out", outDir, inDir + "JS.rats")
      val mainClass = "xtc.parser.Rats"
      val cache = FileFunction.cached(outFile,
                                      FilesInfo.lastModified,
                                      FilesInfo.exists) {
        in: Set[File] => {
          Fork.java(options, mainClass +: arguments)
          Set(file(inDir + "JS.rats"))
        }
      }
      cache(file(inDir).asFile.listFiles.toSet)
    },
    testOptions in Test += Tests.Argument("-fDG", baseDirectory.value + "/tests/detail"),
    compile <<= (compile in Compile) dependsOn (buildParsers in Compile, checkCopyrights in Compile),
    test <<= (testOnly in Test).toTask(
      " kr.ac.kaist.safe.CFGBuildTest" +
      " kr.ac.kaist.safe.BasicAnalyzeTest" +
      " kr.ac.kaist.safe.HTMLAnalyzeTest") dependsOn compile
  )

scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature",
                                   "-language:postfixOps",
                                   "-language:implicitConversions")

unmanagedJars in Compile ++= Seq(file("lib/xtc.jar"), file("lib/jericho-html-3.3.jar"))
cleanFiles ++= Seq(file("src/main/java/kr/ac/kaist/safe/parser/"))

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "scala-tool",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test" withSources,
  "com.typesafe.akka" %% "akka-http" % "10.0.10",
  "io.spray" %% "spray-json" % "1.3.2",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.2",
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.12" % "2.9.1",
  "org.jline" % "jline" % "3.10.0"
)

javacOptions ++= Seq("-encoding", "UTF-8")

retrieveManaged := true
