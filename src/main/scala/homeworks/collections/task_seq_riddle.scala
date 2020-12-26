package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
    @tailrec
    def nextLineLoop(line: List[Int], acc: List[Int]): List[Int] = {
      val elem = line.head
      val index = line.indexWhere(x => elem != x) // находим индекс, где прерывается последовательность повтора числа

      // разделяем на два списка - 1 - непрерывная последовательность, по ней считаем сколько чисел
      // 2 - остальная часть, по которой продолжим расчет последовательности
      val pairSeqAndOther = line.splitAt(index)
      if (pairSeqAndOther._1 == Nil)
        (elem :: pairSeqAndOther._2.length :: acc).reverse
      else
        nextLineLoop(pairSeqAndOther._2, elem :: pairSeqAndOther._1.length :: acc)
    }

    if (currentLine == Nil) List[Int](1)
    else nextLineLoop(currentLine, Nil)
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = List(1) #:: funSeq.map(nextLine)

}