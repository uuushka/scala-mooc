package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  private lazy val Alphabet: List[Char] = ('A' to 'Z').toList
  /**
   * Алгоритм для обработки каждого символа шифра/де-шифра
   * Если выходим за рамки алфавита, то идем по кругу
   *
   * @param c       символ алфавита A - Z
   * @param offset  смещение шфира
   * @param toRight направление смещения
   * @return шифрованный символ
   */
  private def caesarCipherAZ(c: Char, offset: Int, toRight: Boolean = true): Char = {
    val direction = if (toRight) 1 else -1
    val alphabetCap = Alphabet.length
    val indexChar = Alphabet.indexOf(c)
    val cipherCharIndex = indexChar + direction * (offset % alphabetCap)
    cipherCharIndex match {
      case ind if ind < 0 => Alphabet(ind+alphabetCap)
      case ind if ind >= alphabetCap => Alphabet(ind-alphabetCap)
      case ind@_ => Alphabet(ind)
    }
  }

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = {
    assert(offset > 0, "offset must be positive number")
    word.map(c => caesarCipherAZ(c, offset))
  }


  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    assert(offset > 0, "offset must be positive number")
    cipher.map(c => caesarCipherAZ(c, offset, false))
  }

}
