package io.github.maxkar
package http.server.jetty.qos

import fun.coroutine.Coroutine

/** Qos-related definition and type shortcuts. */
private object HQ {
  /** Suspension constructor - how to describe external routine to be called. */
  type Suspension[Qos] = [T] =>> Operation[Qos, T]

  /** Step constructor - how to make the "step" type. */
  type Step[Qos] = [T] =>> Coroutine.Routine[Suspension[Qos], T]

  /** Result of executing one step. */
  type StepResult[Qos] = [T] =>> Coroutine.RunResult[Suspension[Qos], T]
}
