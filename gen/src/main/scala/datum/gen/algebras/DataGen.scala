package datum.gen.algebras

import cats.Traverse
import datum.patterns.data
import datum.patterns.data.Data
import datum.patterns.schemas._
import datum.patterns.properties._
import datum.modifiers.Optional
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import higherkindness.droste.{Algebra, AlgebraM, scheme}

object DataGen {

  private val helpers: AlgebraM[Gen, SchemaF, Data] = AlgebraM {

    // If the value is empty, sometimes drop the key from the output
    case ObjF(fields, _) =>
      Gen.chooseNum(0.0, 1.0).map { x =>
        data.obj(fields.filter {
          case (_, data.empty) => x <= 0.5
          case _               => true
        })
      }

    case RowF(elements, _) => data.row(elements.map(_.value))

    case err => throw new Exception(s"Impossible: ${pprint.apply(err)}")
  }

  private val unixtime = Gen.chooseNum(Integer.MAX_VALUE / 8, Integer.MAX_VALUE).map { ts =>
    java.time.Instant.ofEpochSecond(ts)
  }

  import scala.collection.JavaConverters._
  // JDK <= 1.8 has the following bug https://bugs.openjdk.java.net/browse/JDK-8138664
  // So DataGen will avoid generating timezones with GMT0 specified as the tz
  private val allzones =
    java.time.ZoneId.getAvailableZoneIds.asScala.filter(_ != "GMT0").toVector.map(java.time.ZoneId.of)

  private val zones = Gen.oneOf(allzones)

  val algebra: Algebra[SchemaF, Gen[Data]] = Algebra {
    case ValueF(IntType, _)     => arbitrary[Int].map(data.integer)
    case ValueF(LongType, _)    => arbitrary[Long].map(data.long)
    case ValueF(FloatType, _)   => arbitrary[Float].map(data.float)
    case ValueF(DoubleType, _)  => arbitrary[Double].map(data.double)
    case ValueF(TextType, _)    => Gen.asciiPrintableStr.map(data.text)
    case ValueF(BooleanType, _) => arbitrary[Boolean].map(data.boolean)
    case ValueF(BytesType, _)   => arbitrary[Array[Byte]].map(data.bytes)

    case ValueF(DateType, _) =>
      for {
        ts <- unixtime
        z <- zones
      } yield {
        data.date(ts.atZone(z).toLocalDate)
      }

    case ValueF(TimestampType, _) =>
      unixtime.map { ts =>
        data.timestamp(ts)
      }

    case ValueF(DateTimeType, _) =>
      for {
        ts <- unixtime
        z <- zones
      } yield {
        data.localTime(ts.atZone(z).toLocalDateTime)
      }

    case ValueF(ZonedDateTimeType, _) =>
      for {
        ts <- unixtime
        z <- zones
      } yield {
        data.zonedTime(ts.atZone(z))
      }

    case ArrayF(element, _) =>
      Gen.resize(5, Gen.containerOf[Vector, Data](element).map(data.row))

    case UnionF(alternatives, _) =>
      for {
        name <- Gen.oneOf(alternatives.keySet.toSeq)
        v <- alternatives(name)
      } yield data.union(name, v)

    case other =>
      Traverse[SchemaF].sequence(other).flatMap(helpers(_))
  }

  def optional(alg: Algebra[SchemaF, Gen[Data]]): Algebra[SchemaF, Gen[Data]] = Algebra { schema =>
    if (schema.properties.get(Optional.key).contains(true.prop)) {
      Gen.frequency(1 -> Gen.const(data.empty), 5 -> alg(schema))
    } else {
      alg(schema)
    }
  }

  def define(alg: Algebra[SchemaF, Gen[Data]] = algebra): Schema => Gen[Data] = {
    scheme.cata(alg)
  }
}
