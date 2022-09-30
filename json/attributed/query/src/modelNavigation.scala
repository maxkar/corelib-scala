package io.github.maxkar
package json.attr.query

import json.attr.Json
import json.query.ModelNavigation
import json.query.ModelStepResult


/**
 * Implementation of the simple model navigation.
 */
given modelNavigation[A]: ModelNavigation[Json[A]] with
  override def index(base: Json[A], index: Int): ModelStepResult[Json[A]] =
    base match
      case Json.Array(items, _) =>
        if (0 <= index && index < items.length)
        then ModelStepResult.Success(items(index))
        else ModelStepResult.MissingValue
      case _ => ModelStepResult.IllegalSelector
    end match
  end index


  override def key(base: Json[A], key: String): ModelStepResult[Json[A]] =
    base match
      case Json.Object(items, _) =>
        items.get(key) match
          case Some(x) => ModelStepResult.Success(x.value)
          case None => ModelStepResult.MissingValue
        end match
      case _ => ModelStepResult.IllegalSelector
    end match
  end key
end modelNavigation


