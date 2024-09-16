package jmh

import zio.json.{DeriveJsonCodec, JsonCodec}

final case class BenchmarkRun(
    benchmark: String,
    mode: BenchmarkMode,
    primaryMetric: PrimaryMetric
)

object BenchmarkRun:
  implicit val codec: JsonCodec[BenchmarkRun] = DeriveJsonCodec.gen[BenchmarkRun]

case class PrimaryMetric(
    score: Double,
    scoreUnit: ScoreUnit
):
  def render: String = f"${formatNumber(score)} ${scoreUnit.render}"

  private def formatNumber(num: Double): String =
    val parts                = num.toString.split("\\.")
    val integerPart          = parts(0).reverse.grouped(3).mkString(",").reverse
    val decimalPart          = if parts.length > 1 then parts(1) else "00"
    val formattedDecimalPart = if decimalPart.length > 2 then decimalPart.substring(0, 2) else decimalPart
    s"$integerPart.$formattedDecimalPart"

object PrimaryMetric:
  implicit val codec: JsonCodec[PrimaryMetric] = DeriveJsonCodec.gen[PrimaryMetric]

sealed trait ScoreUnit extends Product with Serializable:
  def render: String = this match
    case ScoreUnit.OpsPerMillisecond => "ops/ms"
    case ScoreUnit.OpsPerNanosecond  => "ops/ns"
    case ScoreUnit.OpsPerMicrosecond => "ops/us"
    case ScoreUnit.OpsPerSecond      => "ops/s"
    case ScoreUnit.OpsPerMinute      => "ops/m"
    case ScoreUnit.OpsPerHour        => "ops/h"
    case ScoreUnit.OpsPerDay         => "ops/d"
    case ScoreUnit.MillisecondsPerOp => "ms/op"
    case ScoreUnit.NanosecondsPerOp  => "ns/op"
    case ScoreUnit.MicrosecondsPerOp => "us/op"
    case ScoreUnit.SecondsPerOp      => "s/op"
    case ScoreUnit.MinutesPerOp      => "m/op"
    case ScoreUnit.HoursPerOp        => "h/op"
    case ScoreUnit.DaysPerOp         => "d/op"
    case ScoreUnit.Milliseconds      => "ms"
    case ScoreUnit.Nanoseconds       => "ns"
    case ScoreUnit.Microseconds      => "us"
    case ScoreUnit.Seconds           => "s"
    case ScoreUnit.Minutes           => "m"
    case ScoreUnit.Hours             => "h"
    case ScoreUnit.Days              => "d"

object ScoreUnit:
  case object OpsPerMillisecond extends ScoreUnit
  case object OpsPerNanosecond  extends ScoreUnit
  case object OpsPerMicrosecond extends ScoreUnit
  case object OpsPerSecond      extends ScoreUnit
  case object OpsPerMinute      extends ScoreUnit
  case object OpsPerHour        extends ScoreUnit
  case object OpsPerDay         extends ScoreUnit
  case object MillisecondsPerOp extends ScoreUnit
  case object NanosecondsPerOp  extends ScoreUnit
  case object MicrosecondsPerOp extends ScoreUnit
  case object SecondsPerOp      extends ScoreUnit
  case object MinutesPerOp      extends ScoreUnit
  case object HoursPerOp        extends ScoreUnit
  case object DaysPerOp         extends ScoreUnit
  case object Milliseconds      extends ScoreUnit
  case object Nanoseconds       extends ScoreUnit
  case object Microseconds      extends ScoreUnit
  case object Seconds           extends ScoreUnit
  case object Minutes           extends ScoreUnit
  case object Hours             extends ScoreUnit
  case object Days              extends ScoreUnit

  implicit val codec: JsonCodec[ScoreUnit] = JsonCodec.string.transform(
    {
      case "ops/ms" => OpsPerMillisecond
      case "ops/ns" => OpsPerNanosecond
      case "ops/us" => OpsPerMicrosecond
      case "ops/s"  => OpsPerSecond
      case "ops/m"  => OpsPerMinute
      case "ops/h"  => OpsPerHour
      case "ops/d"  => OpsPerDay
      case "ms/op"  => MillisecondsPerOp
      case "ns/op"  => NanosecondsPerOp
      case "us/op"  => MicrosecondsPerOp
      case "s/op"   => SecondsPerOp
      case "m/op"   => MinutesPerOp
      case "h/op"   => HoursPerOp
      case "d/op"   => DaysPerOp
      case "ms"     => Milliseconds
      case "ns"     => Nanoseconds
      case "us"     => Microseconds
      case "s"      => Seconds
      case "m"      => Minutes
      case "h"      => Hours
      case "d"      => Days

    },
    _.render
  )

sealed trait BenchmarkMode extends Product with Serializable

object BenchmarkMode:
  case object Throughput     extends BenchmarkMode
  case object AverageTime    extends BenchmarkMode
  case object SampleTime     extends BenchmarkMode
  case object SingleShotTime extends BenchmarkMode
  case object All            extends BenchmarkMode

  implicit val codec: JsonCodec[BenchmarkMode] = JsonCodec.string.transform(
    {
      case "thrpt"  => Throughput
      case "avgt"   => AverageTime
      case "sample" => SampleTime
      case "ss"     => SingleShotTime
      case "all"    => All
    },
    {
      case Throughput     => "thrpt"
      case AverageTime    => "avgt"
      case SampleTime     => "sample"
      case SingleShotTime => "ss"
      case All            => "all"
    }
  )
