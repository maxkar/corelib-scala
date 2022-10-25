package io.github.maxkar
package json.simple

import json.simple.Json
import json.query.ModelNavigation
import json.query.ModelStepResult


/**
 * Implementation of the simple model navigation.
 */
given modelNavigation: ModelNavigation[Json] with
  override def index(base: Json, index: Int): ModelStepResult[Json] =
    base match
      case Json.Array(items) =>
        if (0 <= index && index < items.length)
        then ModelStepResult.Success(items(index))
        else ModelStepResult.MissingValue
      case _ => ModelStepResult.IllegalSelector
    end match
  end index


  override def key(base: Json, key: String): ModelStepResult[Json] =
    base match
      case Json.Object(items) =>
        items.get(key) match
          case Some(x) => ModelStepResult.Success(x)
          case None => ModelStepResult.MissingValue
        end match
      case _ => ModelStepResult.IllegalSelector
    end match
  end key
end modelNavigation
