

class ToDValues[A] {
  private val values: Array[Option[A]] = Array.fill(24)(None: Option[A])
  /**
   * This allows you to get a value for a particular hour. If there isn’t
   * a value, it will throw an exception.
   *
   * @param hour the hour of the day to get. Should be between 0 and 23 inclusive.
   * @return the value stored for that hour.
   */
  def apply(hour: Int): A = values(hour).get
  /**
   * This allows you to get a value for a particular hour. If there isn’t
   * a value, it will return None.
   *
   * @param hour the hour of the day to get. Should be between 0 and 23 inclusive.
   * @return an Option of the value stored for that hour.
   */
  def get(hour: Int): Option[A] = values(hour)
  /**
   * Allows you to set the value in a particular hour.
   *
   * @param hour the hour of the day. Should be between 0 and 23 inclusive.
   * @param v the new value to set.
   */
  def update(hour: Int, v: A) = values(hour) = Some(v)
  /**
   * Allows you to set the value in a particular hour using a String for time.
   *
   * @param hour the hour of the day. Should be between 0 and 23 inclusive.
   * @param v the new value to set.
   */
  def update(time: String, v: A) = {
    val hour = hourFromTime(time)
    values(hour) = Some(v)
  }
  /**
   * This method clears the value at a particular time.
   *
   * @param hour the hour to clear.
   */
  def clear(hour: Int): Unit = { values(hour) = None }
  /**
   * This method clears the value at a particular time.
   *
   * @param hour the hour to clear.
   */
  def clear(time: String): Unit = {
    val hour = hourFromTime(time)
    values(hour) = None
  }
  /**
   * Allows you to combine two sets of data using a specified function.
   *
   * @param o the other set of data.
   * @param f The function to apply to the two data types.
   */
  def combine(o: ToDValues[A], f: (Option[A], Option[A]) => Option[A]):
  ToDValues[A] = {
    val ret = new ToDValues[A]
    for ((v, i) <- (values.lazyZip(o.values)).map((v1, v2) => f(v1,
      v2)).zipWithIndex) {
      ret.values(i) = v
    }
    ret
  }
  override def toString(): String = "ToD :\n"+
    (for ((o, i) <- values.zipWithIndex) yield i+" : "+o).mkString("\n")
  private def hourFromTime(time: String): Int = {
    time.substring(0, time.indexOf(":")).toInt +
      (if (time.endsWith("PM") && !time.startsWith("12")) 12 else 0)
  }
}

val riders1 = new ToDValues[Int]
val riders2 = new ToDValues[Int]
val worker1 = new ToDValues[String]
val worker2 = new ToDValues[String]
riders1(12) = 5 // same as riders1.update(12, 5)
riders1("8:24AM") = 10 // same as riders1.update("8:24AM", 10)
riders1(14) = 7
riders2("2:13PM") = 8
worker1(12) = "Kyle" // same as worker1.update(12, "Kyle")
val totalRiders = riders1.combine(riders2, (o1, o2) => (o1, o2) match {
  case (None, None) => None
  case (Some(a), None) => Some(a)
  case (None, Some(b)) => Some(b)
  case (Some(a), Some(b)) => Some(a + b)
})
println(riders1)
println(totalRiders)

-