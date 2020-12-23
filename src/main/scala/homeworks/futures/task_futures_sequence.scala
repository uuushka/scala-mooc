package homeworks.futures

import scala.concurrent.Future
import scala.util.{Failure, Success}

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

    val acc: List[Either[A, Throwable]] = Nil
    val futureValues = futures
      .foldRight(acc) { // распределяем вычисления фьючи в Either
        case (x, acc) => x.value match {
          case Some(Success(value)) => Left(value) :: acc
          case Some(Failure(ex)) => Right(ex) :: acc
          case None => Right(new Exception("Future not completed")) :: acc
        }
      }
      .partitionMap { // делим по Left и Right на кортеж
        case l@Left(_) => l
        case r@Right(_) => r
      }

    Future.successful(futureValues)
  }

}