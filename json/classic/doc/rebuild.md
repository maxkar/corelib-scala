# Changing Defaults

The library was designed to provide good-enough conversions between
Scala and Json values. However, it is possible to define your own ways
of converting things.

This task is pretty simple. You define your own set of conversions for
all the types you need and do not import library's implicits.

You may want to add collection and option conversions derivation functions
defined in the `OptConverters`, `SeqConverters` and `MapConverters` mix-ins.
