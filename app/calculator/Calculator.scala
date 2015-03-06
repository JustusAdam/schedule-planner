package calculator

import datamodel.Lesson

import scala.collection.mutable
import scala.util.Sorting

/**
 * Provide methods to calculate (possible) schedule layouts
 *
 * @author justusadam
 *
 */
object Calculator {

  /**
   * Calculate the possible layout for the provided list of lessons
   *
   * @param lessons list of lessons to allocate
   * @return List of Maps of lessons mapped to times
   */
  def calc(lessons: List[Lesson]) : List[Map[(Int, Int), Lesson]] = {

    val mapped_lessons: Map[String, List[Lesson]] = lessons groupBy (_.subject)

    val sort_func:(Lesson) => Int = (a: Lesson) => a.weight

    val sorted_lessons = mapped_lessons mapValues (
      x =>
        Sorting.stableSort(
          x,
          sort_func
        ).toList
      )

    val (min_list_primer:List[Lesson], lists_values: List[(String, List[Lesson])]) = (for ((t, x :: xs) <- sorted_lessons) yield (x, (t, xs))).unzip
    val lists = lists_values.toMap

    val x :: min_list = Sorting.stableSort(min_list_primer, sort_func).toList

    calc_step(x, lists, Map[(Int, Int), Lesson](), min_list)

  }

  def calc_step(
                 x: Lesson,
                 lists: Map[String, List[Lesson]],
                 map: Map[(Int, Int), Lesson],
                 min_list: List[Lesson]

                 ): List[Map[(Int, Int), Lesson]] = {

    def reduce_lists(subject: String, map: Map[(Int, Int), Lesson]) = {
      lists.get(subject) map {
        case Nil => Nil
        case c :: cs =>
          calc_step(c, lists.updated(subject, cs), map, min_list)
      }
    }

    map.get(x.time) match {
      case None =>

        val new_map = map + ((x.time, x))

        min_list match {
          case Nil => List(new_map)
          case c :: cs =>
            calc_step(c, lists, new_map, cs)
        }

      case Some(old) =>
        val r1 = reduce_lists(x.subject, map) getOrElse Nil
        val r2 = reduce_lists(old.subject, map + ((x.time, x))) getOrElse Nil

        r1 ++ r2
    }
  }
}

