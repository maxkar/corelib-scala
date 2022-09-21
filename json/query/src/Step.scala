package io.github.maxkar
package json.query

/**
 * One navigation step in the JSON query. The "step" indicates how to
 * get to a next element (i.e. by indexing or by name).
 */
enum Step:
  /**
   * An element is assumed to be an array and the next step is indexing in that array.
   * @param index index of the element in the array.
   */
  case Index(index: Int)

  /**
   * An element is assumed to be an object and the step specifies the step name to be used.
   * @param name name of the element to be accessed.
   */
  case Name(name: String)


  /** Checks if this step is just a simple name (and could be written using dot notation). */
  def isSimpleName(): Boolean =
    this match
      case Index(_) => false
      case Name(name) => Step.isSimpleName(name)
    end match
  end isSimpleName


  /**
   * Encodes the step in "javascript-like" form. The encoding is as follows:
   *  * Array elements are encoded as `[<index>]`
   *  * Simple names (no special characters) are encoded as `.<name>`
   *  * Complex names (with special characters) are encoded as `["<escaped-name>"]`
   * @return encoded form of the step.
   */
  def encode(): String =
    this match
      case Index(idx) => s"[${idx}]"
      case Name(name) if Step.isSimpleName(name) => s".${name}"
      case Name(name) => s"[\"${Step.escapeName(name)}\"]"
    end match
  end encode


  /**
   * Encodes the step in "javascript-like" form ensuring no leading dot is generated.
   *  The encoding is as follows:
   *  * Array elements are encoded as `[<index>]`
   *  * Simple names (no special characters) are encoded as `<name>`
   *  * Complex names (with special characters) are encoded as `["<escaped-name>"]`
   * @return encoded form of the step.
   */
  def encodeNoLeadingDot(): String =
    this match
      case Index(idx) => s"[${idx}]"
      case Name(name) if Step.isSimpleName(name) => s"${name}"
      case Name(name) => s"[\"${Step.escapeName(name)}\"]"
    end match
  end encodeNoLeadingDot
end Step


object Step:
  /**
   * Conversion from int values to step values.
   */
  given intToStepConversion: Conversion[Int, Step] with
    def apply(x: Int): Step = Step.Index(x)

  /**
   * Conversion from strings to step values.
   */
  given stringToStepConversion: Conversion[String, Step] with
    def apply(x: String): Step = Step.Name(x)


  /** Hexadecimal digits. */
  private val HEX_CHARS = "0123456789ABCDEF"


  /** Converts code into hexadecimal digit. */
  private def toHexDigit(char: Char): Char =
    HEX_CHARS.charAt(char)


  /**
   * Checks if the given string name is "simple" name (i.e. does not contain any
   * special characters and could be written as ".name" in the path)
   */
  private def isSimpleName(name: String): Boolean =
    if name.isEmpty() then return false
    if !java.lang.Character.isLetter(name.charAt(0)) then return false
    return name.forall(java.lang.Character.isLetterOrDigit)
  end isSimpleName


  /**
   * Escapes the name according to javascript/json string escaping rules.
   * @param name name to escape.
   * @return escaped name (without additional quotes around it).
   */
  private def escapeName(name: String): String =
    val res = new StringBuilder()
    var ptr = 0

    while ptr < name.length() do
      name.charAt(ptr) match
        case '"' => res ++= "\\\""
        case '\\' => res ++= "\\\\"
        case '\b' => res ++= "\\b"
        case '\f' => res ++= "\\f"
        case '\n' => res ++= "\\n"
        case '\r' => res ++= "\\r"
        case '\t' => res ++= "\\t"
        case x if x < 0x0010 =>
          res ++= "\\u000"
          res += toHexDigit(x)
        case x if x < 0x0020 =>
          res ++= "\\u001"
          res += toHexDigit(x)
        case other =>
          res += other
      end match

      ptr += 1
    end while

    res.toString()
  end escapeName
end Step
