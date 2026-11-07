package grit.prose.words

/** The English grit writes the same way wherever it writes it. */
object Words {

  /** `n` and `unit`, the unit with `s` added unless `n` is 1: `1 row`, `3 rows`, `0 rows`. */
  def counted(n: Long, unit: String): String = if (n == 1) s"1 $unit" else s"$n ${unit}s"
}
