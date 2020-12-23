package homeworks.futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {
  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]]): Future[(List[A], List[Throwable])] = {
    implicit val ecGlobal: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    val acc: (List[A], List[Throwable]) = (Nil, Nil)

    // дабы не лишний раз не проходить partition сразу аккумулируем в кортеж
    // с помощью map и recover вытаскиваем значение, т.к. они возвращают Future
    // acc делаем Future, и над ним делаем flatMap, чтобы наполнять кортеж
    futures
      .foldRight(Future.successful(acc)) {
        case (x, acc) => acc.flatMap(
          res => x
            .map { case value: A => (value :: res._1, res._2) }
            .recover { case ex: Throwable => (res._1, ex :: res._2) })
      }
  }

}