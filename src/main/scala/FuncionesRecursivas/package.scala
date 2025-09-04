package FuncionesRecursivas

def maxLin(l: List[Int]): Int =
  if (l.isEmpty) throw new IllegalArgumentException("Lista vacía")
  else if (l.tail.isEmpty) l.head
  else
    val maxTail = maxLin(l.tail)
    if (l.head > maxTail) l.head else maxTail

def maxIt(l: List[Int]): Int =
  if (l.isEmpty) throw new IllegalArgumentException("Lista vacía")
  else
    def aux(lista: List[Int], acc: Int): Int =
      if (lista.isEmpty) acc
      else
        val nuevoAcc = if (lista.head > acc) lista.head else acc
        aux(lista.tail, nuevoAcc)
    aux(l.tail, l.head)

def movsTorresHanoi(n : Int) : BigInt = {
    def expNumTwo(acc : BigInt, index : Int) : BigInt = {
        if (index == 0) acc
        else {
            val aux = acc * 2
            expNumTwo(aux, index - 1)
        }
    }
    expNumTwo(1, n) - 1
}


def torresHanoi(n: Int, t1: Int, t2: Int, t3: Int): List[(Int, Int)] = {
  if (n == 1) {
    List((t1, t3))
  }
  else {

    val parte1 = torresHanoi(n - 1, t1, t3, t2)

    val movimiento = List((t1, t3))

    val parte2 = torresHanoi(n - 1, t2, t1, t3)

    parte1 ++ movimiento ++ parte2
  }
}

