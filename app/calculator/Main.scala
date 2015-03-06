package calculator

import datamodel.Lesson


import scala.util.Sorting

/**
 * @author justusadam
 */
object Main {
  def main(args: Array[String]) {
    val lessons = List(
      new Lesson(1, 1, 2, "TGI"),
      new Lesson(1, 1, 2, "FS"),
      new Lesson(1, 1, 1, "TGI"),
      new Lesson(2, 2, 3, "TGI"),
      new Lesson(1, 1, 3, "RA"),
      new Lesson(2, 2, 2, "RA"),
      new Lesson(1, 2, 3, "FS")
    )

    val weighted = Calculator.calc(lessons).groupBy(x => x.values.map(_.weight).sum).toList

    print("weight    schedule\n")
    Sorting.stableSort(weighted, (a: (Int, _), b: (Int, _)) => a._1 < b._1) foreach { case (a, b) => print(s"$a    $b   \n")}
  }

  def render_schedule(schedule: List[Map[(Int, Int), String]]): String = {
    def cells(x:Any*) = {

    }
    ""
  }
}
