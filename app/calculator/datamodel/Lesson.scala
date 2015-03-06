package calculator.datamodel

/**
 * @author justusadam
 */
class Lesson(val number:Int, val day:Int, val weight:Int, val subject:String) {
  def time = (day, number)

  override def toString = subject + "  " + weight
}
