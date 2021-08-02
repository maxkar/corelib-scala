package io.github.maxkar
package json.classic

/** Converters for sequences. */
trait SeqConverters:
  /** Converts a json into a sequence if there is a converter for a single element. */
  given jsonToSeq[T](using base: Conversion[Json, T], s: Conversion[Json, Seq[Json]]): Conversion[Json, Seq[T]] =
    new Conversion[Json, Seq[T]] {
      override def apply(v: Json): Seq[T] =
        s(v).map(base)
    }


  /** Converts a sequence of items into json array if there is a simple element conversion. */
  given seqToJson[T](using base: Conversion[T, Json], s: Conversion[Seq[Json], Json]): Conversion[Seq[T], Json] =
    new Conversion[Seq[T], Json] {
      override def apply(v: Seq[T]): Json =
        s(v.map(base))
    }

end SeqConverters
