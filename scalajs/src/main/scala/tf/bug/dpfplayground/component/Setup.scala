package tf.bug.dpfplayground.component

import calico.frp.{*, given}
import calico.html.io.{*, given}
import cats.effect.*
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.{Signal, SignallingRef}
import fs2.dom.HtmlElement
import tf.bug.dpfplayground.state.SimpleInstanceOption.{Exists, ParamsInstanceTriple, ParamsInstanceTriple2, ParamsPair}
import tf.bug.dpfplayground.state.{DynamicDpf, EmbeddingOption, SOption, SimpleInstanceOption, XOption, YOption}

final case class Setup[F[_]](
  element: HtmlElement[F],
  result: Signal[F, Option[DynamicDpf]]
)

object Setup {

  // note: this is all "wrong"/unintuitive to make managing state easier
  private final case class PairSignal[F[_], A <: SimpleInstanceOption](
    element: Signal[F, Option[Resource[F, HtmlElement[F]]]],
    result: Signal[F, Option[ParamsPair[A]]]
  )

  private final case class TripleSignal[F[_], A <: SimpleInstanceOption](
    element: Signal[F, Option[Resource[F, HtmlElement[F]]]],
    result: Signal[F, Option[ParamsInstanceTriple[A]]]
  )

  private final case class Triple2Signal[F[_], A <: SimpleInstanceOption](
    element1: Signal[F, Option[Resource[F, HtmlElement[F]]]],
    element2: Signal[F, Option[Resource[F, HtmlElement[F]]]],
    result: Signal[F, Option[ParamsInstanceTriple2[A]]]
  )

  private def simpleSelectBox[A <: SimpleInstanceOption](
    companion: SimpleInstanceOption.Companion[A],
    id: String,
    name: String,
    r: SignallingRef[IO, Option[A]]
  ): Resource[IO, HtmlElement[IO]] = {
    div(
      label(forId := id, s"${name}: "),
      select.withSelf { self => (
        idAttr := id,
        required := true,

        option(value := "", disabled := true, selected := true, hidden := true),

        children[Int] { idx =>
          option(value := idx.toString, companion.values(idx).name)
        } <-- Signal.constant(companion.values.indices.toList),

        value <-- r.map(_.map(a => companion.values.indexOf(a).toString).getOrElse("")),

        onInput --> {
          _.evalMap(_ => self.value.get)
            .map(_.toIntOption.map(companion.values(_)))
            .foreach(r.set)
        }
      )}
    )
  }

  private def simpleParametersComponent[A <: SimpleInstanceOption](
    option: Signal[IO, Option[A]]
  ): Resource[IO, PairSignal[IO, A]] = for {
    out <- SignallingRef.of[IO, Option[ParamsPair[A]]](None).toResource

    elemOpt <- option.discrete.switchMap {
      case Some(choice) =>
        Stream.resource(for {
          // TODO: check the choice, and cache the previous value of params
          params <- SignallingRef.of[IO, Option[choice.Params]](choice.defaultParams).toResource

          _ <- params.discrete.evalMap {
            case Some(p) => out.set(Some(ParamsPair(choice, p)))
            case None => out.set(None)
          }.compile.drain.background

          elem = choice.takeParams(params)
        } yield elem)
      case None =>
        Stream.exec(out.set(None)) ++ Stream.emit(None)
    }.hold1Resource
  } yield PairSignal(elemOpt, out)

  private def instanceOfParametersComponent[A <: SimpleInstanceOption](
    pair: Signal[IO, Option[ParamsPair[A]]],
  ): Resource[IO, TripleSignal[IO, A]] = for {
    out <- SignallingRef.of[IO, Option[ParamsInstanceTriple[A]]](None).toResource

    elemOpt <- pair.discrete.switchMap {
      case Some(pp) =>
        Stream.resource(for {
          inst <- SignallingRef.of[IO, Option[pp.x.Instance[pp.y.type]]](pp.x.defaultInstance(pp.y)).toResource

          _ <- inst.discrete.evalMap {
            case Some(i) => out.set(Some(Exists(pp.x, pp.y, i)))
            case None => out.set(None)
          }.compile.drain.background

          elem = pp.x.takeInstance(pp.y, inst)
        } yield elem)
      case None =>
        Stream.exec(out.set(None)) ++ Stream.emit(None)
    }.hold1Resource
  } yield TripleSignal(elemOpt, out)

  private def instanceOfParametersComponent2[A <: SimpleInstanceOption](
    pair: Signal[IO, Option[ParamsPair[A]]],
  ): Resource[IO, Triple2Signal[IO, A]] = for {
    out <- SignallingRef.of[IO, Option[ParamsInstanceTriple2[A]]](None).toResource

    elemOpts <- pair.discrete.switchMap {
      case Some(pp) =>
        Stream.resource(for {
          instA <- SignallingRef.of[IO, Option[pp.x.Instance[pp.y.type]]](pp.x.defaultInstance(pp.y)).toResource
          instB <- SignallingRef.of[IO, Option[pp.x.Instance[pp.y.type]]](pp.x.defaultInstance(pp.y)).toResource

          _ <- (instA: Signal[IO, Option[pp.x.Instance[pp.y.type]]], instB).tupled.discrete.evalMap(_.tupled match {
            case Some(t) => out.set(Some(Exists(pp.x, pp.y, t)))
            case None => out.set(None)
          }).compile.drain.background

          elems = (pp.x.takeInstance(pp.y, instA), pp.x.takeInstance(pp.y, instB))
        } yield elems)
      case None =>
        Stream.exec(out.set(None)) ++ Stream.emit((None, None))
    }.hold1Resource

    elem1Opt = elemOpts.map(_._1)
    elem2Opt = elemOpts.map(_._2)
  } yield Triple2Signal(elem1Opt, elem2Opt, out)

  def apply(): Resource[IO, Setup[IO]] = {
    for {
      // our UI state is Any-typed to prevent having to juggle where state is with what it should be
      xr <- SignallingRef.of[IO, Option[XOption]] (Some(DynamicDpf.Default.x)).toResource
      sr <- SignallingRef.of[IO, Option[SOption]] (Some(DynamicDpf.Default.s)).toResource
      yr <- SignallingRef.of[IO, Option[YOption]] (Some(DynamicDpf.Default.y)).toResource

      selectInput <- simpleSelectBox(XOption, "input-domain", "Input Domain", xr)
      selectInputParameters <- simpleParametersComponent(xr)
      selectInputInstance <- instanceOfParametersComponent(selectInputParameters.result)

      selectSeed <- simpleSelectBox(SOption, "seed-generation", "Seed Generation", sr)
      selectSeedParameters <- simpleParametersComponent(sr)
      selectSeedInstance2 <- instanceOfParametersComponent2(selectSeedParameters.result)

      selectOutput <- simpleSelectBox(YOption, "output-group", "Output Group", yr)
      selectOutputParameters <- simpleParametersComponent(yr)
      selectOutputInstance <- instanceOfParametersComponent(selectOutputParameters.result)

      elem <- div(
        idAttr := "setup",
        h1("Setup"),

        div(
          selectInput,
          selectInputParameters.element
        ),
        div(
          selectSeed,
          selectSeedParameters.element
        ),
        div(
          selectOutput,
          selectOutputParameters.element
        ),

        div(
          label(forId := "leaf-structure", "Leaf Structure: "),
          // TODO component based on seed block and output group
        ),

        hr(()),

        div(
          label(forId := "seed-p0", "Seed p0: "),
          selectSeedInstance2.element1
        ),

        div(
          label(forId := "seed-p1", "Seed p1: "),
          selectSeedInstance2.element2
        ),

        hr(()),

        div(
          label(forId := "input-point", "Input Point: "),
          selectInputInstance.element
        ),

        div(
          input(`type` := "checkbox", idAttr := "secret-share-input"),
          label(forId := "secret-share-input", "Secret Share"),
        ),

        hr(()),

        div(
          label(forId := "output-value", "Output Value: "),
          selectOutputInstance.element
        ),

        div(
          input(`type` := "checkbox", idAttr := "secret-share-output"),
          label(forId := "secret-share-output", "Secret Share"),
        ),
      )

      dpfSignal = Signal.constant[IO, Option[DynamicDpf]](None)
    } yield Setup(elem, dpfSignal)
  }

}
