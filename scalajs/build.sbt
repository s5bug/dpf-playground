import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleSplitStyle}
import scala.sys.process.Process
import scala.util.Try

lazy val dpfPlayground = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.8.4",

    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("tf.bug.dpfplayground.DpfPlayground"),

    scalaJSLinkerConfig ~= {
      _.withESFeatures(_.withESVersion(ESVersion.ES2021))
        // .withExperimentalUseWebAssembly(true) // cats-effect doesn't support WASM yet
        .withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("tf.bug")))
    },

    scalacOptions ++= Seq(
      "-no-indent",
      "-new-syntax"
    ),

    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-dom" % "2.8.1",
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-effect" % "3.7.0",
      "co.fs2" %% "fs2-core" % "3.13.0",
      "com.armanbilge" %% "calico" % "0.2.3",
      "org.typelevel" %% "spire" % "0.18.0",
      "org.scodec" %% "scodec-core" % "2.3.3",
    ),
  )
