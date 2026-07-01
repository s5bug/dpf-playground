package tf.bug.dpfplayground.component

import calico.html.io.{*, given}
import cats.effect.*
import fs2.concurrent.{Signal, SignallingRef}
import fs2.dom.HtmlElement
import tf.bug.dpfplayground.state.{DynamicDpf, SOption, XOption, YOption}

object Setup {

  def apply(dpfState: SignallingRef[IO, DynamicDpf]): Resource[IO, HtmlElement[IO]] = {
    div(
      idAttr := "setup",
      h1("Setup"),

      div(
        label(forId := "input-domain", "Input Domain: "),
        select(
          idAttr := "input-domain",
          children[Int] { xoi =>
            option(value := xoi.toString, XOption.values(xoi).name)
          } <-- Signal.constant(XOption.values.indices.toList)
        )
      ),

      div(
        label(forId := "seed-block", "Seed Block: "),
        select(
          idAttr := "seed-block",
          children[Int] { soi =>
            option(value := soi.toString, SOption.values(soi).name)
          } <-- Signal.constant(SOption.values.indices.toList)
        )
      ),

      div(
        label(forId := "output-group", "Output Group: "),
        select(
          idAttr := "output-group",
          children[Int] { yoi =>
            option(value := yoi.toString, YOption.values(yoi).name)
          } <-- Signal.constant(YOption.values.indices.toList)
        )
      ),

      div(
        label(forId := "leaf-structure", "Leaf Structure: "),
        // TODO component based on seed block and output group
      ),

      hr(()),

      div(
        label(forId := "seed-1", "Seed 1: "),
        // TODO component based on seed block
      ),

      div(
        label(forId := "seed-2", "Seed 2: "),
        // TODO component based on seed block
      ),

      hr(()),

      div(
        label(forId := "input-point", "Input Point: "),
        // TODO component based on input domain
      ),

      div(
        input(`type` := "checkbox", idAttr := "secret-share-input"),
        label(forId := "secret-share-input", "Secret Share"),
      ),

      hr(()),

      div(
        label(forId := "output-value", "Output Value: "),
        // TODO component based on output group
      ),

      div(
        input(`type` := "checkbox", idAttr := "secret-share-output"),
        label(forId := "secret-share-output", "Secret Share"),
      ),
    )
  }

}
