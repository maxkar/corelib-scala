package io.github.maxkar
package text

import scala.annotation.targetName


/** Provider of the location for the given type. */
trait LocationInfo[M[_], T] {
  /** Returns location in the stream. */
  def getLocation(stream: T): M[Location]

  extension (t: T) {
    @targetName("getLocationExt")
    inline def getLocation(): M[Location] =
      this.getLocation(t)
  }
}
